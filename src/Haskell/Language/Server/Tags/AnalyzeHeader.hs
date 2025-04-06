----------------------------------------------------------------------------
-- |
-- Module      :  Server.Tags.AnalyzeHeader
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Thursday, 22 September 2016
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OrPatterns        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Haskell.Language.Server.Tags.AnalyzeHeader
  ( analyzeHeader
  ) where

import Control.Arrow (first, second)
import Control.Monad.Except.Ext
import Control.Monad.Trans.Maybe

import Data.Char
import Data.Foldable.Ext (toList, foldFor)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Monoid (Ap(..))
import Data.Semigroup
import Data.Set (Set)
import Data.Set qualified as S
import Data.Strict.Pair qualified as Strict
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Prettyprinter qualified as PP
import Prettyprinter.Combinators
import Prettyprinter.Ext

import Haskell.Language.Lexer.Types
  (stripNewlines, tokToName, Pos(..), Line, SrcPos(..), Type, posLine, unLine, PragmaType(..), ServerToken(..), Type(..), ProcessMode(..))
import Haskell.Language.Lexer.Types qualified as Types

import Control.Monad.Logging
import Data.ErrorMessage
import Data.KeyMap (KeyMap)
import Data.KeyMap qualified as KM
import Data.MonoidalMap (MonoidalMap)
import Data.MonoidalMap qualified as MonoidalMap
import Data.Path
import Data.SubkeyMap (SubkeyMap)
import Data.SubkeyMap qualified as SubkeyMap
import Data.Symbols
import Haskell.Language.Blocks
import Haskell.Language.Server.Tags.Types.Imports
import Haskell.Language.Server.Tags.Types.Modules

extractImportBlocks
  :: [Pos ServerToken]
  -> ( [NonEmpty (Pos ServerToken)] -- each import in its own block
     , [Pos ServerToken]   -- remaining tokens
     )
extractImportBlocks = go [] [] . breakBlocks ProcessVanilla KeepDirectives
  where
    go
      :: [NonEmpty (Pos ServerToken)]
      -> [NonEmpty (Pos ServerToken)]
      -> [NonEmpty (Pos ServerToken)]
      -> ([NonEmpty (Pos ServerToken)], [Pos ServerToken])
    go imports other = \case
      block@(Pos _ KWImport{} :| _) : tss -> go (block : imports) other tss
      ts : tss                            -> go imports (ts : other) tss
      []                                  -> (reverse imports, foldMap toList $ reverse other)

analyzeHeader
  :: (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => FullPath 'File
  -> [Pos ServerToken]
  -> m (Maybe ModuleHeader, [Pos ServerToken])
analyzeHeader filename ts =
  -- logDebug $ "[analyzeHeader] ts =" <+> ppTokens ts
  case dropWhile ((/= KWModule) . valOf) ts of
    Pos _ KWModule :
      (dropNLs -> Pos _ (T modName) :
        (break ((== KWWhere) . valOf) . dropNLs -> (exportList, Pos _ KWWhere : body))) -> do
      let (imports, rest) = extractImportBlocks body
      (importSpecs, importQualifiers) <- analyzeImports filename imports
      let importQualifiers' = MonoidalMap.unMonoidalMap importQualifiers
      exports                         <- analyzeExports filename importQualifiers' $ dropAllCppDefines exportList
      let header = ModuleHeader
            { mhModName          = mkModuleName modName
            , mhImports          = importSpecs
            , mhImportQualifiers = importQualifiers'
            , mhExports          = exports
            }
      pure (Just header, rest)
      -- No header present.
    _ -> pure (Nothing, ts)

pattern PAs           :: Pos ServerToken
pattern PAs           <- Pos _ (T "as")
pattern PComma        :: Pos ServerToken
pattern PComma        <- Pos _ Comma
pattern PHiding       :: Pos ServerToken
pattern PHiding       <- Pos _ (T "hiding")
pattern PImport       :: Pos ServerToken
pattern PImport       <- Pos _ KWImport
pattern PForeign      :: Pos ServerToken
pattern PForeign      <- Pos _ KWForeign
pattern PLParen       :: Pos ServerToken
pattern PLParen       <- Pos _ LParen
pattern PModule       :: Pos ServerToken
pattern PModule       <- Pos _ KWModule
pattern PName         :: Text -> Pos ServerToken
pattern PName name    <- Pos _ (T name)
pattern PNewline      :: Int -> Pos ServerToken
pattern PNewline x    <- Pos _ (Newline x)
pattern PPattern      :: Pos ServerToken
pattern PPattern      <- Pos _ (T "pattern")
pattern PQualified    :: Pos ServerToken
pattern PQualified    <- Pos _ (T "qualified")
pattern PRParen       :: Pos ServerToken
pattern PRParen       <- Pos _ RParen
pattern PSourcePragma :: Pos ServerToken
pattern PSourcePragma <- Pos _ (Pragma SourcePragma)
pattern PString       :: Pos ServerToken
pattern PString       <- Pos _ String
pattern PType         :: Pos ServerToken
pattern PType         <- Pos _ KWType

pattern PAnyName      :: Text -> Pos ServerToken
pattern PAnyName name <- Pos _ (tokToName -> Just name)

-- pattern PCppDefine     :: Text -> Pos ServerToken
-- pattern PCppDefine str <- Pos _ (CppDefine str)

pattern PName'              :: Line -> Text -> Pos ServerToken
pattern PName' line name    <- Pos SrcPos{posLine = line} (T name)
pattern PAnyName'           :: Line -> Text -> Pos ServerToken
pattern PAnyName' line name <- Pos SrcPos{posLine = line} (tokToName -> Just name)

analyzeImports
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => FullPath 'File
  -> [NonEmpty (Pos ServerToken)]
  -> m ( SubkeyMap ImportKey (NonEmpty ImportSpec)
       , MonoidalMap ImportQualifier (NonEmpty ModuleName)
       )
analyzeImports filename = getAp . foldMap (Ap . go . toList)
  where
    mkQual = mkImportQualifier . mkModuleName

    go
      :: [Pos ServerToken]
      -> m ( SubkeyMap ImportKey (NonEmpty ImportSpec)
           , MonoidalMap ImportQualifier (NonEmpty ModuleName)
           )
    go ts = do
      -- logDebug $ "[analyzeImports] ts =" <+> ppTokens ts
      let d = dropNLs
      res <- runMaybeT $ do
        -- Drop initial "import" keyword and {-# SOURCE #-} pragma, if any
        (ts2, importTarget) <- case dropWhile (\case { PImport -> False; PForeign -> False; _ -> True }) $ d ts of
          PImport  : (d -> PSourcePragma : rest) -> pure (rest, HsBootModule)
          PImport  :                       rest  -> pure (rest, VanillaModule)
          _                                      ->
            throwErrorWithCallStack $ "Invalid shape of import block:" ## ppTokens ts
        let dropSafeImport :: [Pos ServerToken] -> [Pos ServerToken]
            dropSafeImport = \case
              PName "safe" : rest -> rest
              rest                -> rest
            dropPackageImport :: [Pos ServerToken] -> [Pos ServerToken]
            dropPackageImport = \case
              PString : rest      -> rest
              rest                -> rest
            extractQualified :: [Pos ServerToken] -> ([Pos ServerToken], Bool)
            extractQualified = \case
              PQualified : rest -> (rest, True)
              rest              -> (rest, False)

            ts3    :: [Pos ServerToken]
            isQual :: Bool
            (ts3, isQual)
              = first (dropPackageImport . d)
              . extractQualified
              . d
              . dropSafeImport
              . d
              $ ts2
        -- Extract import name and renaming alias, if any
        (ts4, name, qualName, isQualPost) <- case d ts3 of
          PName name : (d -> PQualified : (d -> PAs : (d -> PName qualName : rest))) -> pure (rest, name, Just qualName, True)
          PName name : (d -> PAs : (d -> PName qualName : rest))                     -> pure (rest, name, Just qualName, False)
          PName name :                                    rest                       -> pure (rest, name, Nothing, False)
          _                                                                          ->
            throwErrorWithCallStack $ "Cannot extract import name and renaming alias from import block:" ## ppTokens ts3
        -- Make sense of the data collected before
        let qualType = case (isQual || isQualPost, qualName) of
              (True,  Nothing)        -> Qualified $ mkQual name
              (True,  Just qualName') -> Qualified $ mkQual qualName'
              (False, Nothing)        -> Unqualified
              (False, Just qualName') -> BothQualifiedAndUnqualified $ mkQual qualName'
        pure (name, qualType, importTarget, ts4)
      case res of
        Nothing                                  -> pure (mempty, mempty)
        Just (name, qualType, importTarget, ts5) -> add name qualType importTarget ts5
      where
        add
          :: Text
          -> ImportQualification
          -> ImportTarget
          -> [Pos ServerToken]
          -> m ( SubkeyMap ImportKey (NonEmpty ImportSpec)
               , MonoidalMap ImportQualifier (NonEmpty ModuleName)
               )
        add name qual importTarget toks = do
          (importList, toks') <- analyzeImportList filename toks
          let spec    = mkNewSpec importList
              imports = SubkeyMap.singleton
                          (ImportKey importTarget modName)
                          (spec :| [])
          case toks' of
            [] -> pure (imports, qualifiers)
            _  ->
              throwErrorWithCallStack $ "Trailing tokens after import list:" ## ppTokens toks'
          where
            modName :: ModuleName
            modName = mkModuleName name
            qualifiers :: MonoidalMap ImportQualifier (NonEmpty ModuleName)
            qualifiers = case getQualifier qual of
              Just q  -> MonoidalMap.singleton q (modName :| [])
              Nothing -> mempty
            mkNewSpec :: ImportListSpec ImportList -> ImportSpec
            mkNewSpec importList = ImportSpec
              { ispecImportKey     = ImportKey
                  { ikModuleName   = modName
                  , ikImportTarget = importTarget
                  }
              , ispecQualification = qual
              , ispecImportList    = importList
              }

-- Analyze comma-separated list of entries, starting at _|_:
-- - Foo_|_
-- - Foo_|_(Bar, Baz)
-- - Foo_|_ hiding (Bar, Baz)
-- - Quux_|_(..)
analyzeImportList
  :: (Applicative m, MonadError ErrorMessage m)
  => FullPath 'File
  -> [Pos ServerToken]
  -> m (ImportListSpec ImportList, [Pos ServerToken])
analyzeImportList filename toks = do
  -- logDebug $ "[analyzeImpotrList] toks =" <+> ppTokens toks
  case lastNL toks of
    PNewline 0 : _ -> pure (NoImportList, toks)
    toks'          -> case dropNLs toks' of
      []                                    -> pure (NoImportList, toks)
      PHiding : (dropNLs -> PLParen : rest) -> findImportListEntries filename Hidden mempty (dropNLs rest)
      PLParen : rest                        -> findImportListEntries filename Imported mempty (dropNLs rest)
      _                                     -> pure (NoImportList, toks)

findImportListEntries
  :: forall m. (Applicative m, MonadError ErrorMessage m)
  => FullPath 'File
  -> ImportType
  -> KeyMap Set (EntryWithChildren () UnqualifiedSymbolName)
  -> [Pos ServerToken]
  -> m (ImportListSpec ImportList, [Pos ServerToken])
findImportListEntries filename importType = go'
  where
    go' acc toks' = do
      -- logDebug $ "[findImportListEntries] toks =" <+> ppTokens toks
      case dropNLs toks' of
        []                                                                ->
          pure (SpecificImports importList, [])
        -- Reaching here means tricks with preprocessor which we cannot
        -- reasonably handle. E.g.
        --
        -- > import Foo
        -- > #ifdef FOO
        -- >   ( foo
        -- >   , bar
        -- > #else
        -- >   ( baz
        -- >   , quux
        -- > #endif
        -- >   , fizz
        -- >   )
        rest@(PImport : _)                                                ->
          pure (SpecificImports importList, rest)
        PRParen : rest                                                    ->
          pure (SpecificImports importList, rest)
        -- Type import
        PType : PName name : rest                                         ->
          entryWithoutChildren name rest
        PType : PLParen : PAnyName name : PRParen : rest                  ->
          entryWithoutChildren name rest
        -- Pattern import
        PPattern : restWithName@(PName name : rest)
          | isVanillaTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name rest
          | otherwise                          ->
            entryWithoutChildren "pattern" restWithName
        PPattern : restWithName@(PLParen : PAnyName name : PRParen : rest)
          | isOpTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name rest
          | otherwise                          ->
            entryWithoutChildren "pattern" restWithName
        -- Vanilla function/operator/consturtor/type import
        PLParen : PAnyName name : PRParen : rest                          ->
          entryWithChildren "operator in import list" name rest
        PLParen : PName name : PRParen : rest                             ->
          entryWithChildren "operator in import list" name rest
        PName name : rest                                                 ->
          entryWithChildren "name in import list" name rest
        PLParen : rest                                                    ->
          go' acc rest
        Pos _ HSCDirective : rest                                         -> do
          -- We cannot run hsc2hs here so we'll conservatively
          -- assume that everything is imported from a module.
          (_, remaining) <- go' mempty $ dropCommas rest
          pure (AssumedWildcardImportList, remaining)
        Pos _ HSCDirectiveBraced : rest                                   -> do
          -- We cannot run hsc2hs here so we'll conservatively
          -- assume that everything is imported from a module.
          (_, remaining) <- go' mempty $ dropCommas $ dropBalancedBraces 1 rest
          pure (AssumedWildcardImportList, remaining)
        rest                                                              ->
          throwErrorWithCallStack $ "Unrecognised shape of import list:" ## ppTokens rest
      where
        importList :: ImportList
        importList = ImportList
          { ilEntries    = acc
          , ilImportType = importType
          }

        entryWithChildren
          :: Doc Void
          -> Text
          -> [Pos ServerToken]
          -> m (ImportListSpec ImportList, [Pos ServerToken])
        entryWithChildren descr name rest = do
          (children, rest') <- snd $ analyzeChildren descr filename $ dropNLs rest
          name'             <- mkUnqualName name
          let newEntry = EntryWithChildren name' $ (() <$) <$> children
          go' (KM.insert newEntry acc) $ dropCommas rest'

        entryWithoutChildren :: Text -> [Pos ServerToken] -> m (ImportListSpec ImportList, [Pos ServerToken])
        entryWithoutChildren name rest = do
          name' <- mkUnqualName name
          let newEntry = mkEntryWithoutChildren name'
          go' (KM.insert newEntry acc) $ dropCommas rest

mkUnqualName :: (WithCallStack, MonadError ErrorMessage m) => Text -> m UnqualifiedSymbolName
mkUnqualName name =
  case mkUnqualifiedSymbolName (mkSymbolName name) of
    Nothing    ->
      throwErrorWithCallStack $ "Invalid qualified entry on import list:" <+> docFromText name
    Just name' -> pure name'

analyzeExports
  :: forall m. (WithCallStack, MonadError ErrorMessage m, MonadLog m)
  => FullPath 'File
  -> Map ImportQualifier (NonEmpty ModuleName)
  -> [Pos ServerToken]
  -> m (ModuleExportSpec ModuleExports)
analyzeExports filename importQualifiers ts = do
  -- logDebug $ "[analyzeExports] ts =" <+> ppTokens ts
  case stripNewlines ts of
    []                        -> pure NoExports
    -- Drop any other module declarations that could have arisen thanks to e.g. CPP.
    PModule : PName _ : rest -> analyzeExports filename importQualifiers rest
    PLParen : PRParen : _    -> pure EmptyExports
    PLParen : rest           -> SpecificExports <$> go mempty mempty rest
    toks                     ->
      throwErrorWithCallStack $ "Unrecognised shape of export list:" ## ppTokens toks
  where
    -- Analyze comma-separated list of entries like
    -- - Foo
    -- - Foo(Bar, Baz)
    -- - Quux(..)
    -- - pattern PFoo
    -- - module Data.Foo.Bar
    go :: KeyMap NonEmpty (EntryWithChildren PosAndType (SymbolName, PosAndType))
       -> Set ModuleName
       -> [Pos ServerToken]
       -> m ModuleExports
    go entries reexports toks = do
      -- logDebug $ "[analyzeExports.go] toks =" <+> ppTokens toks
      case toks of
        [] ->
          pure exports
        PRParen : _ ->
          pure exports
        -- Pattern export
        PPattern : restWithName@(PName' line name : rest)
          | isVanillaTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name line Types.Pattern rest
          | otherwise                 ->
            entryWithoutChildren "pattern" line Types.Function restWithName
        PPattern : restWithName@(PLParen : PAnyName' line name : PRParen : rest)
          | isOpTypeName name
          , not $ isChildrenList filename rest ->
            entryWithoutChildren name line Types.Pattern rest
          | otherwise                 ->
            entryWithoutChildren "pattern" line Types.Function restWithName
        -- Type export
        PType : PName' line name : rest ->
          entryWithoutChildren name line Types.Family rest
        PType : PLParen : PAnyName' line name : PRParen : rest ->
          entryWithoutChildren name line Types.Family rest
        -- Module reexport
        PModule : PName name : rest ->
          consumeComma entries (newReexports <> reexports) rest
          where
            modName = mkModuleName name
            newReexports :: Set ModuleName
            newReexports
              = S.fromList
              $ toList
              $ M.findWithDefault (modName :| []) (mkImportQualifier modName) importQualifiers
        -- Vanilla function/operator/consturtor/type export
        PLParen : PName' line name : PRParen : rest ->
          entryWithChildren "operator in export list" name line (typeForName Types.Type name) rest
        PLParen : Pos SrcPos{posLine} (tokToName -> Just name) : PRParen : rest ->
          entryWithChildren "operator in export list" name posLine (typeForName Types.Type name) rest
        PName' line name : rest ->
          entryWithChildren "name in export list" name line (typeForName Types.Type name) rest
        PLParen : rest ->
          go entries reexports rest
        toks' ->
          throwErrorWithCallStack $ "Unrecognised export list structure:" <+> ppTokens toks'
      where
        exports :: ModuleExports
        exports = ModuleExports
          { meExportedEntries    = entries
          , meReexports          = reexports
          , meHasWildcardExports = getAny $ foldMap exportsAllChildren entries
          }
        entryWithChildren
          :: Doc Void
          -> Text
          -> Line
          -> Type
          -> [Pos ServerToken]
          -> m ModuleExports
        entryWithChildren listType name !line typIfNoChildren rest = do
          -- logDebug $ "[analyzeExports.entryWithChildren] rest =" <+> ppTokens rest
          let presence    :: ChildrenPresence
              getChildren :: m (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken])
              (presence, getChildren) = analyzeChildren listType filename rest
          (children, rest') <- getChildren
          entryType <-
            case ( presence
                 , foldFor children $ foldMap $ \PosAndType{patType} ->
                     Strict.Pair (Any (patType == Constructor)) (Any (patType == Type))) of
              (ChildrenAbsent,  _)                      -> pure typIfNoChildren
              (ChildrenPresent, Strict.Pair (Any False) (Any False)) -> pure Type
              (ChildrenPresent, Strict.Pair (Any True)  (Any False)) -> pure Type
              (ChildrenPresent, Strict.Pair (Any False) (Any True))  -> pure Family
              (ChildrenPresent, Strict.Pair (Any True)  (Any True))  -> throwErrorWithCallStack $
                "Unexpected children specification for exported name" <+> PP.squotes (pretty name) <> "." <+>
                "It specifies both constructor names and type names among children:" ##
                  ppTokens rest
          let newEntry = EntryWithChildren (mkSymbolName name, PosAndType filename line entryType) children
          consumeComma (KM.insert newEntry entries) reexports rest'
        entryWithoutChildren
          :: Text
          -> Line
          -> Type
          -> [Pos ServerToken]
          -> m ModuleExports
        entryWithoutChildren name !line typ rest = do
          -- logDebug $ "[analyzeExports.entryWithoutChildren] rest =" <+> ppTokens rest
          let newEntry = mkEntryWithoutChildren (mkSymbolName name, PosAndType filename line typ)
          consumeComma (KM.insert newEntry entries) reexports rest
        exportsAllChildren :: EntryWithChildren PosAndType a -> Any
        exportsAllChildren (EntryWithChildren _ visibility) =
          maybe mempty isExportAllChildren visibility
          where
            isExportAllChildren VisibleAllChildren             = Any True
            isExportAllChildren (VisibleSpecificChildren _)    = mempty
            isExportAllChildren (VisibleAllChildrenPlusSome _) = Any True

    -- Continue parsing by consuming comma delimiter.
    consumeComma
      :: KeyMap NonEmpty (EntryWithChildren PosAndType (SymbolName, PosAndType))
      -> Set ModuleName
      -> [Pos ServerToken]
      -> m ModuleExports
    consumeComma entries reexports = go entries reexports . dropCommas

typeForName :: Types.Type -> Text -> Types.Type
typeForName constructorLikeTag name =
  case T.uncons $ unqualSymNameText $ stripQualifiedPart name of
    Just (':', _) -> constructorLikeTag
    Just (c, _)
      | isAlpha c -> if isUpper c then constructorLikeTag else Types.Function
      | otherwise -> Types.Operator
    Nothing -> Types.Function

isChildrenList :: FullPath 'File -> [Pos ServerToken] -> Bool
isChildrenList filename toks =
  case fst res of
    ChildrenPresent -> True
    ChildrenAbsent  -> False
  where
    res :: (ChildrenPresence, Either ErrorMessage (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken]))
    res = analyzeChildren mempty filename toks

data ChildrenPresence = ChildrenPresent | ChildrenAbsent

data WildcardPresence = WildcardPresent | WildcardAbsent

instance Semigroup WildcardPresence where
  (<>) WildcardAbsent y              = y
  (<>) x              WildcardAbsent = x
  (<>) _              _              = WildcardPresent

instance Monoid WildcardPresence where
  mempty = WildcardAbsent
  mappend = (<>)

analyzeChildren
  :: forall m. (WithCallStack, MonadError ErrorMessage m)
  => Doc Void
  -> FullPath 'File
  -> [Pos ServerToken]
  -> (ChildrenPresence, m (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken]))
analyzeChildren listType filename toks =
  case dropNLs toks of
    []                                               -> (ChildrenAbsent, pure (Nothing, []))
    toks'@(PComma : _)                               -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PRParen : _)                              -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PName _ : _)                              -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PModule : _)                              -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PPattern : _)                             -> (ChildrenAbsent, pure (Nothing, toks'))
    toks'@(PType : _)                                -> (ChildrenAbsent, pure (Nothing, toks'))
    -- PLParen : PName ".." : PRParen : rest            -> (ChildrenPresent, pure (Just VisibleAllChildren, rest))
    PLParen : PRParen : rest                         -> (ChildrenAbsent, pure (Nothing, rest))
    PLParen : rest@(PAnyName name : _)
      | isNonOperatorName name       -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    PLParen : rest@(PType : PAnyName name : _)
      | isNonOperatorName name       -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    PLParen : rest@(PLParen : PAnyName name : PRParen : _)
      | not $ isNonOperatorName name -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    PLParen : rest@(PType : PLParen : PAnyName name : PRParen : _)
      | not $ isNonOperatorName name -> analyzeList rest
      | otherwise                    -> (ChildrenAbsent, pure (Nothing, toks))
    toks'                                            ->
      ( ChildrenAbsent
      , throwErrorWithCallStack $ "While analyzing" <+> PP.squotes (pretty filename) <> ": cannot handle children of" <+> listType <> ":" ## ppTokens toks'
      )
  where
    analyzeList
      :: WithCallStack
      => [Pos ServerToken]
      -> (ChildrenPresence, m (Maybe (ChildrenVisibility PosAndType), [Pos ServerToken]))
    analyzeList = second (fmap mkVisibility) . extractChildren mempty mempty . dropNLs
      where
        mkVisibility
          :: (Map UnqualifiedSymbolName PosAndType, WildcardPresence, t)
          -> (Maybe (ChildrenVisibility PosAndType), t)
        mkVisibility (children, wildcard, toks') = (visibility, toks')
          where
            visibility
              | M.null children =
                case wildcard of
                  WildcardPresent -> Just VisibleAllChildren
                  WildcardAbsent  -> Just $ VisibleSpecificChildren children
              | otherwise       =
                case wildcard of
                  WildcardPresent -> Just $ VisibleAllChildrenPlusSome children
                  WildcardAbsent  -> Just $ VisibleSpecificChildren children

    extractChildren
      :: WithCallStack
      => WildcardPresence
      -> Map UnqualifiedSymbolName PosAndType
      -> [Pos ServerToken]
      -> (ChildrenPresence, m (Map UnqualifiedSymbolName PosAndType, WildcardPresence, [Pos ServerToken]))
    extractChildren wildcardPresence !names = \case
      []                                                     ->
        (childrenPresence, pure (names, wildcardPresence, []))
      PRParen : rest                                         ->
        (childrenPresence, pure (names, wildcardPresence, rest))
      PType : PName' line name : rest                        ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename line Type) names) $ dropCommas rest
      PType : PLParen : PAnyName' line name : PRParen : rest ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename line Type) names) $ dropCommas rest
      PName ".." : rest                                      ->
        extractChildren (wildcardPresence <> WildcardPresent) names $ dropCommas rest
      PAnyName' line name : rest                             ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename line (typeForName Constructor name)) names) $ dropCommas rest
      PLParen : PAnyName' line name : PRParen : rest         ->
        extractChildren wildcardPresence (M.insert (stripQualifiedPart name) (PosAndType filename line (typeForName Constructor name)) names) $ dropCommas rest
      PLParen : rest                                         ->
        extractChildren wildcardPresence names $ dropNLs rest
      toks'                                                  ->
        (ChildrenAbsent, throwErrorWithCallStack $ "Unrecognised children list structure:" ## ppTokens toks')
      where
        childrenPresence
          | M.null names =
            case wildcardPresence of
              WildcardPresent -> ChildrenPresent
              WildcardAbsent  -> ChildrenAbsent
          | otherwise    = ChildrenPresent

stripQualifiedPart :: Text -> UnqualifiedSymbolName
stripQualifiedPart = snd . splitQualifiedPart . mkSymbolName

isNonOperatorName :: Text -> Bool
isNonOperatorName =
  T.all check . unqualSymNameText . snd . splitQualifiedPart . mkSymbolName
  where
    check :: Char -> Bool
    check '\'' = True
    check '_'  = True
    check '#'  = True
    check '.'  = True
    check c    = isAlphaNum c

newtype Tokens = Tokens [Pos ServerToken]

instance Pretty Tokens where
  pretty (Tokens ts) =
    ppDictHeader "Tokens"
      [ "tokens" :-> ppListWith ppTokenVal ts
      ]
    where
      ppTokenVal :: Pos ServerToken -> Doc ann
      ppTokenVal (Pos SrcPos{posLine} tok) =
        pretty (unLine posLine) <> PP.colon <> pretty tok

ppTokens :: [Pos ServerToken] -> Doc ann
ppTokens = pretty . Tokens . take 16

-- | Drop prefix of newlines except the last one.
lastNL :: [Pos ServerToken] -> [Pos ServerToken]
lastNL (PNewline _ : ts@(PNewline _ : _)) = lastNL ts
lastNL ts                                 = ts

-- | Drop prefix of newlines.
dropNLs :: [Pos ServerToken] -> [Pos ServerToken]
dropNLs (PNewline _ : ts) = dropNLs ts
dropNLs ts                = ts

dropCommas :: [Pos ServerToken] -> [Pos ServerToken]
dropCommas = go . dropNLs
  where
    go (Pos _ Comma : ts) = dropCommas ts
    go ts                 = ts

isVanillaTypeName  :: Text -> Bool
isVanillaTypeName = maybe False (isUpper . fst) . T.uncons

isOpTypeName :: Text -> Bool
isOpTypeName = maybe False ((== ':') . fst) . T.uncons

dropBalancedBraces :: Int -> [Pos ServerToken] -> [Pos ServerToken]
dropBalancedBraces _ []                              = []
dropBalancedBraces 0 ts                              = ts
dropBalancedBraces n (Pos _ HSCDirectiveBraced : ts) = dropBalancedBraces (n + 1) ts
dropBalancedBraces n (Pos _ LBrace       : ts)       = dropBalancedBraces (n + 1) ts
dropBalancedBraces n (Pos _ RBrace       : ts)       = dropBalancedBraces (n - 1) ts
dropBalancedBraces n (_                  : ts)       = dropBalancedBraces n ts

dropAllCppDefines :: [Pos ServerToken] -> [Pos ServerToken]
dropAllCppDefines = filter $ \case
  Pos _ CppDefine{} -> False
  _                 -> True

-- dropCppDefinesInBalancedParens :: Int -> [Pos ServerToken] -> [Pos ServerToken]
-- dropCppDefinesInBalancedParens _ []                              = []
-- dropCppDefinesInBalancedParens 0 ts                              = ts
-- dropCppDefinesInBalancedParens n (Pos _ HSCDirectiveBraced : ts) = dropBalancedBraces (n + 1) ts
-- dropCppDefinesInBalancedParens n (Pos _ LBrace       : ts)       = dropBalancedBraces (n + 1) ts
-- dropCppDefinesInBalancedParens n (Pos _ RBrace       : ts)       = dropBalancedBraces (n - 1) ts
-- dropCppDefinesInBalancedParens n (_                  : ts)       = dropBalancedBraces n ts
