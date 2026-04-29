-- |
-- Module:     Haskell.Language.Tags.Analyze
-- Copyright:  (c) Sergey Vinokurov 2026
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE CPP               #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}

module Haskell.Language.Tags.Analyze
  ( ProcessMode(..)
  , processTokens

  -- for tests
  , UnstrippedTokens(..)
  , whereBlock
  ) where

import Prelude hiding (last)

import Control.Arrow ((***))
import Control.Monad
import Data.Char qualified as Char
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Void (Void)
import Prettyprinter (Doc, pretty, (<+>))
import Prettyprinter.Generics (Generic, PPGeneric(..), Pretty)

import Haskell.Language.Lexer.CppTypes qualified as CPP
import Haskell.Language.Lexer.Types
import Haskell.Language.Tags.Types

-- | Newlines have to remain in the tokens because 'breakBlocks' relies on
-- them.  But they make pattern matching on the tokens unreliable because
-- newlines might be anywhere.  A newtype makes sure that the tokens only get
-- stripped once and that I don't do any pattern matching on unstripped tokens.
newtype UnstrippedTokens = UnstrippedTokens { unUnstrippedTokens :: [Pos Token] }
  deriving (Show, Semigroup, Monoid)

mapTokens :: ([Pos Token] -> [Pos Token]) -> UnstrippedTokens -> UnstrippedTokens
mapTokens f (UnstrippedTokens tokens) = UnstrippedTokens (f tokens)

-- | Drop @n@ non-newline tokens.
dropTokens :: Int -> UnstrippedTokens -> UnstrippedTokens
dropTokens k = mapTokens (f k)
  where
    f :: Int -> [Pos Token] -> [Pos Token]
    f 0 xs                       = xs
    f _ []                       = []
    f n (Pos _ (Newline _) : xs) = f n xs
    f n (Pos _ _           : xs) = f (n - 1) xs


data ProcessMode
  = ProcessVanilla
  -- ^ LitVanilla Haskell file - everything can produce tags
  | ProcessAlexHappy
  -- ^ Alex/Happy, only first and last braced blocks may produce tags
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving Pretty via PPGeneric ProcessMode

processTokens :: ProcessMode -> [Pos Token] -> ([Pos TagVal], [Doc Void])
processTokens mode
  = splitAndRemoveRepeats
  . concatMap blockTags
  . breakBlocks mode
  . UnstrippedTokens
  where
    splitAndRemoveRepeats :: [Tag] -> ([Pos TagVal], [Doc Void])
    splitAndRemoveRepeats tags =
      ( earliestRepeats ++ newTags
      , map valOf warnings
      )
      where
        (newTags, repeatableTags, warnings) = partitionTags tags
        -- For RepeatableTag s with duplicate keys, pick the one with the lowest
        -- posLine.
        earliestRepeats :: [Pos TagVal]
        earliestRepeats =
          M.elems $
            M.fromListWith minLine $
              zip (map valOf repeatableTags) repeatableTags
        minLine x y
          | tagLine x < tagLine y = x
          | otherwise             = y

startIdentChar :: Char -> Bool
startIdentChar '_' = True
startIdentChar c   = Char.isAlpha c

identChar :: Bool -> Char -> Bool
identChar considerDot c = case c of
  '\'' -> True
  '_'  -> True
  '#'  -> True
  '.'  -> considerDot
  c'   -> Char.isAlphaNum c'

isHaskellOp :: Text -> Bool
isHaskellOp str = case headt str of
  Nothing  -> False
  Just ':' -> False
  Just _   -> T.all haskellOpChar str

isHaskellConstructorOp :: Text -> Bool
isHaskellConstructorOp str = case T.uncons str of
  Nothing        -> False
  Just (':', xs) -> T.all haskellOpChar xs
  Just _         -> False

haskellOpChar :: Char -> Bool
haskellOpChar c = case c of
  '_'   -> False
  '-'   -> True
  '!'   -> True
  '#'   -> True
  '$'   -> True
  '%'   -> True
  '&'   -> True
  '*'   -> True
  '+'   -> True
  '.'   -> True
  '/'   -> True
  '<'   -> True
  '='   -> True
  '>'   -> True
  '?'   -> True
  '@'   -> True
  '^'   -> True
  '|'   -> True
  '~'   -> True
  ':'   -> True
  '\\'  -> True
  other -> isSymbolCharacterCategory (Char.generalCategory other)

isTypeVarStart :: Text -> Bool
isTypeVarStart x = case headt x of
  Just c -> Char.isLower c || c == '_'
  _ -> False

-- | Break the input up into blocks based on indentation.
breakBlocks :: ProcessMode -> UnstrippedTokens -> [UnstrippedTokens]
breakBlocks mode
  = map UnstrippedTokens
  . filter (not . null)
  . go
  . stripSemicolonsNotInBraces
  . (case mode of
      ProcessVanilla -> id
      ProcessAlexHappy -> uncurry (++) . firstLastBracedBlock)
  . stripToplevelHscDirectives
  . filterBlank
  . unUnstrippedTokens
  where
    go :: [Pos Token] -> [[Pos Token]]
    go []     = []
    go tokens = pre : go post
      where
        (pre, post) = breakBlock tokens
    -- Blank lines mess up the indentation.
    filterBlank :: [Pos Token] -> [Pos Token]
    filterBlank [] = []
    filterBlank (Pos _ (Newline _) : xs@(Pos _ (Newline _) : _)) =
      filterBlank xs
    filterBlank (x:xs) = x : filterBlank xs

-- | Collect tokens between toplevel braces. Motivated by Alex/Happy
-- file format that uses braced blocks to separate Haskell source from
-- other directives.
firstLastBracedBlock :: [Pos Token] -> ([Pos Token], [Pos Token])
firstLastBracedBlock tokens =
  (first, last)
  where
    (first, rest) = forward 0 [] tokens
    last          = backward 0 [] $ reverse rest
    forward :: Int -> [Pos Token] -> [Pos Token] -> ([Pos Token], [Pos Token])
    forward _ acc []                       = (reverse acc, [])
    forward 0 acc (Pos _ LBrace      : ts) = forward 1 acc ts
    forward 0 acc (_                 : ts) = forward 0 acc ts
    forward 1 acc (Pos _ RBrace      : ts) = (reverse acc, ts)
    forward n acc (t@(Pos _ LBrace)  : ts) = forward (n + 1) (t : acc) ts
    forward n acc (t@(Pos _ HSCEnum) : ts) = forward (n + 1) (t : acc) ts
    forward n acc (t@(Pos _ RBrace)  : ts) = forward (n - 1) (t : acc) ts
    forward n acc (t                 : ts) = forward n (t : acc) ts

    backward :: Int -> [Pos Token] -> [Pos Token] -> [Pos Token]
    backward _ acc []                       = acc
    backward 0 acc (Pos _ RBrace      : ts) = backward 1 acc ts
    backward 0 acc (_                 : ts) = backward 0 acc ts
    backward 1 acc (Pos _ LBrace      : _)  = acc
    backward n acc (t@(Pos _ LBrace)  : ts) = backward (n - 1) (t : acc) ts
    backward n acc (t@(Pos _ HSCEnum) : ts) = backward (n - 1) (t : acc) ts
    backward n acc (t@(Pos _ RBrace)  : ts) = backward (n + 1) (t : acc) ts
    backward n acc (t                 : ts) = backward n (t : acc) ts

-- | Take until a newline, then take lines until the indent established after
-- that newline decreases. Or, alternatively, if "{" is encountered then count
-- it as a block until closing "}" is found taking nesting into account.
breakBlock :: [Pos Token] -> ([Pos Token], [Pos Token])
breakBlock = go []
  where
    go :: [Pos Token] -> [Pos Token] -> ([Pos Token], [Pos Token])
    go acc [] = (reverse acc, [])
    go acc (Pos _ Newline{} : t@(Pos _ KWModule) : ts) =
      (reverse acc ++ t : importList, drop 1 rest)
      where
        (importList, rest) = span ((/= KWWhere) . valOf) ts
    go acc (t@(Pos _ tok) : ts) = case tok of
      Newline indent -> collectIndented acc indent ts
      LBrace         -> collectBracedBlock (t : acc) go ts 1
      HSCEnum        -> collectBracedBlock (t : acc) go ts 1
      _              -> go (t : acc) ts

    collectIndented :: [Pos Token] -> Int -> [Pos Token] -> ([Pos Token], [Pos Token])
    collectIndented acc indent = goIndented acc
      where
        goIndented acc' ts' = case ts' of
          Pos _ Newline{} : Pos _ KWModule : _ ->
            (reverse acc', ts')

          []     -> (reverse acc', [])
          t : ts -> case t of
            Pos _ (Newline n) | n <= indent ->
                                (reverse acc', ts')
            Pos _ LBrace ->
              collectBracedBlock (t : acc') goIndented ts 1
            _ ->
              goIndented (t : acc') ts

    collectBracedBlock
      :: Show b
      => [Pos Token]
      -> ([Pos Token] -> [Pos Token] -> ([Pos Token], [b]))
      -> [Pos Token]
      -> Int
      -> ([Pos Token], [b])
    collectBracedBlock acc cont = goBraced acc
      where
        goBraced acc' []       _ = (reverse acc', [])
        goBraced acc' ts       0 = cont acc' ts
        goBraced acc' (t : ts) n = goBraced (t : acc') ts $! case t of
          Pos _ LBrace -> n + 1
          Pos _ RBrace -> n - 1
          _            -> n

stripToplevelHscDirectives :: [Pos Token] -> [Pos Token]
stripToplevelHscDirectives = scan
  where
    scan :: [Pos Token] -> [Pos Token]
    scan = \case
      []                            -> []
      Pos _ HSCDirectiveBraced : ts -> skip 1 ts
      t : ts                        -> t : scan ts

    skip :: Int -> [Pos Token] -> [Pos Token]
    skip _  []                             = []
    skip 0  ts                             = scan ts
    skip n (Pos _ HSCDirectiveBraced : ts) = skip (n + 1) ts
    skip n (Pos _ LBrace       : ts)       = skip (n + 1) ts
    skip n (Pos _ RBrace       : ts)       = skip (n - 1) ts
    skip n (_                  : ts)       = skip n ts

stripSemicolonsNotInBraces :: [Pos Token] -> [Pos Token]
stripSemicolonsNotInBraces =
  go False 0 0
  where
    go  :: Bool -- Whether inside let or where block or case expression
        -> Int -- Indent of last newline
        -> Int -- Parenthesis nesting depth
        -> [Pos Token]
        -> [Pos Token]
    go  _      _  _ []                                                       = []
    go !b     !k !n (tok@(Pos _ KWWhere)     : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWWhere)     : ts)                           = tok : go True k n ts
    go  b      k  n (tok@(Pos _ KWLet)       : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWLet)       : ts)                           = tok : go True k n ts
    go  b      k  n (tok@(Pos _ KWDo)        : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWDo)        : ts)                           = tok : go True k n ts
    go  b      k  n (tok@(Pos _ KWOf)        : tok'@(Pos _ LBrace) : ts)     = tok : tok' : skipBalancedParens b k (inc n) ts
    go  _      k  n (tok@(Pos _ KWOf)        : ts)                           = tok : go True k n ts
    go  _      k  n (tok@(Pos _ KWIn)        : ts)                           = tok : go False k n ts
    go  _      _  n (tok@(Pos _ (Newline k)) : ts)                           = tok : go False k n ts
    go  _      _  0 (     Pos _ Semicolon    : tok@(Pos _ (Newline k)) : ts) = tok : go False k 0 ts
    go  False  k  0 (     Pos p Semicolon    : ts)                           = Pos p (Newline k) : go False k 0 ts
    go  b      k  n (tok@(Pos _ LParen)      : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ SpliceStart) : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ LBracket)    : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ LBrace)      : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ LBanana)     : ts)                           = tok : skipBalancedParens b k (inc n) ts
    go  b      k  n (tok@(Pos _ RParen)      : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok@(Pos _ RBracket)    : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok@(Pos _ RBrace)      : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok@(Pos _ RBanana)     : ts)                           = tok : go b k (dec n) ts
    go  b      k  n (tok : ts)                                               = tok : go b k n       ts

    skipBalancedParens
      :: Bool -- Whether inside where block or after equals sign
      -> Int -- Indent of last newline
      -> Int -- Parenthesis nesting depth
      -> [Pos Token]
      -> [Pos Token]
    skipBalancedParens b k = skip
      where
        skip :: Int -> [Pos Token] -> [Pos Token]
        skip _ []                             = []
        skip 0 ts                             = go b k 0 ts
        skip n (tok@(Pos _ LParen)      : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ SpliceStart) : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ LBracket)    : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ LBrace)      : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ LBanana)     : ts) = tok : skip (inc n) ts
        skip n (tok@(Pos _ RParen)      : ts) = tok : skip (dec n) ts
        skip n (tok@(Pos _ RBracket)    : ts) = tok : skip (dec n) ts
        skip n (tok@(Pos _ RBrace)      : ts) = tok : skip (dec n) ts
        skip n (tok@(Pos _ RBanana)     : ts) = tok : skip (dec n) ts
        skip n (tok : ts)                     = tok : skip n ts

    inc :: Int -> Int
    inc n = n + 1
    dec :: Int -> Int
    dec n = max 0 (n - 1)

explodeToplevelBracedBlocks :: [Pos Token] -> [[Pos Token]]
explodeToplevelBracedBlocks toks =
  case toks of
    Pos _ LBrace : toks' -> filter (not . null) $ go [] 1 toks'
    _                    -> [toks]
  where
    go :: [Pos Token] -> Int -> [Pos Token] -> [[Pos Token]]
    go acc _   []                          = [reverse acc]
    go acc 0   ts                          = [reverse acc, ts]
    go acc n   (tok@(Pos _ LBrace)   : ts) = go (tok : acc) (n + 1) ts
    go acc 1   (     Pos _ RBrace    : ts) = reverse acc : go [] 0 ts
    go acc n   (tok@(Pos _ RBrace)   : ts) = go (tok : acc) (n - 1) ts
    go acc n@1 (     Pos _ Semicolon : ts) = reverse acc : go [] n ts
    go acc n   (tok                  : ts) = go (tok : acc) n ts

-- * extract tags

patternRecordFieldNames :: [Pos Token] -> ([Tag], [Pos Token])
patternRecordFieldNames = go []
  where
    go acc ts =
      case ts of
        Pos pos (T name) : rest -> go (mkTag pos name Pattern : acc) rest
        Pos _ Comma      : rest -> go acc rest
        _                       -> (acc, ts)

-- | Get all the tags in one indented block.
-- TODO clean this up to require less nesting, and dropDataContext duplication
blockTags :: UnstrippedTokens -> [Tag]
blockTags unstripped = case stripNewlines unstripped of
  []                                    -> []
  Pos _ SpliceStart : _                 -> []
  Pos _ ToplevelSplice : _              -> []
  Pos pos (Cpp (CPP.Define name)) : _   ->
    [mkRepeatableTag pos name Define]
  Pos _ HSCEnum : rest                  ->
    hsc2hsEnum rest
  Pos _ KWModule : Pos pos (T name) : _ ->
    [mkTag pos (snd (T.breakOnEnd "." name)) Module]
  stripped@(Pos _       (T "pattern") : Pos _ DoubleColon : _) ->
    toplevelFunctionTags stripped
  (Pos _ (T "pattern") : Pos pos (T name) : Pos _ LBrace : rest)
        | (fieldNames, Pos _ RBrace : Pos _ Equals : _) <- patternRecordFieldNames rest ->
          mkTag pos name Pattern : fieldNames
  stripped@(Pos prevPos (T "pattern") : toks) ->
    case tag of
      Nothing -> toplevelFunctionTags stripped
      Just x  -> [x]
    where
      (tag, _, _) = recordVanillaOrInfixName isTypeName Pattern prevPos
        "pattern * =" toks
  Pos _ KWForeign : decl -> foreignTags decl
  -- newtype instance * = ...
  Pos prevPos KWNewtype : Pos _ KWInstance : toks ->
    map (addParent familyNameTag) $ newtypeTags pos $
      dropTokens 2 unstripped
    where
      (familyNameTag, pos) =
        extractFamilyName prevPos "newtype instance * =" toks
  -- newtype X * = X *
  Pos prevPos KWNewtype : toks ->
    maybeToList tag
      ++ map (addParent tag) (newtypeTags pos (dropTokens 1 unstripped))
    where
      (tag, pos, _) =
        recordVanillaOrInfixName isTypeName Type prevPos "newtype * =" toks
  -- type family X ...
  Pos prevPos KWType : Pos _ KWFamily : toks -> maybeToList tag
    where
      (tag, _,  _) = recordVanillaOrInfixName isTypeFamilyName Family prevPos
        "type family * =" toks
  -- type instance X * = ...
  -- No tags in type family instances
  Pos _ KWType : Pos _ KWInstance : _ -> []
  -- type X * = ...
  Pos prevPos KWType : toks
    -- If there’s no equals sign then this is definitely not a type synonym declaration.
        | containsEquals toks
        -> maybeToList tag
        | otherwise
        -> []
    where
      (tag, _, _) = recordVanillaOrInfixName isTypeName Type prevPos
        "type * =" toks
  -- data family X ...
  Pos prevPos KWData : Pos _ KWFamily : toks ->
    map (addParent tag) $ maybeToList tag
    where
      (tag, _, _) = recordVanillaOrInfixName isTypeFamilyName Family prevPos
        "data family * =" toks
  -- data instance * = ...
  -- data instance * where ...
  Pos prevPos KWData : Pos _ KWInstance : toks ->
    map (addParent familyNameTag) $
      dataConstructorTags pos (dropTokens 2 unstripped)
    where
      (familyNameTag, pos) =
        extractFamilyName prevPos "data instance * =" toks
  -- data X * = X { X :: *, X :: * }
  -- data X * where ...
  Pos prevPos KWData : toks ->
    maybeToList tag
      ++ map (addParent tag)
           (dataConstructorTags pos (dropTokens 1 unstripped))
    where
      (tag, pos, _) = recordVanillaOrInfixName isTypeName Type prevPos
        "data * =" toks
  -- class * => X where X :: * ...
  Pos pos KWClass : _ -> classTags pos (dropTokens 1 unstripped)

  Pos _ KWInfix : _ -> []
  Pos _ KWInfixl : _ -> []
  Pos _ KWInfixr : _ -> []
  -- Deriving introduces no new names, just ignore it
  Pos _ KWDeriving : _ -> []
  -- instance * where data * = X :: * ...
  Pos pos KWInstance : _ -> instanceTags pos (dropTokens 1 unstripped)
  -- x, y, z :: *
  stripped -> toplevelFunctionTags stripped

isTypeFamilyName :: Text -> Bool
isTypeFamilyName =
  maybe False (\c -> Char.isUpper c || c == ':') . headt

isTypeName  :: Text -> Bool
isTypeName x = case headt x of
  Just c -> Char.isUpper c || c == ':'
  _ -> False

dropDataContext :: [Pos Token] -> [Pos Token]
dropDataContext = stripParensKindsTypeVars . stripOptContext

recordVanillaOrInfixName
  :: (Text -> Bool)                   -- ^ Predicate for names to select
  -> Type                             -- ^ Type of detected tag
  -> SrcPos                           -- ^ Previous position to report in errors
  -> Doc Void                         -- ^ Context to report in errors
  -> [Pos Token]                      -- ^ Tokens to analyze
  -> (Maybe Tag, SrcPos, [Pos Token]) -- ^ Possibly detected tag and rest of the tokens
recordVanillaOrInfixName isVanillaName tokenType prevPos context tokens =
  case dropDataContext tokens of
    toks | Type <- tokenType
             , Just (pos, name, rest) <- extractSpecialTypeName toks ->
           (Just $ mkTag pos name tokenType, pos, rest)
    Pos _ RParen   : _                -> (Nothing, prevPos, tokens)
    Pos _ LBracket : _                -> (Nothing, prevPos, tokens)
    Pos _ Equals   : _                -> (Nothing, prevPos, tokens)
    Pos _ Comma    : _                -> (Nothing, prevPos, tokens)
    tok : toks ->
      case tok of
        Pos pos (tokToName -> Just name) | isVanillaName name ->
                                           (Just $ mkTag pos name tokenType, pos, toks)
        _ -> case dropInfixTypeStart $ tok : toks of
          Pos pos (tokToName -> Just name) : rest ->
            (Just $ mkTag pos name tokenType, pos, rest)
          rest -> (Just $ unexp pos rest, pos, tok : toks)
            where pos = posOf tok
    [] -> (Just $ unexp prevPos [], prevPos, [])
  where
    unexp pos rest = unexpected pos (UnstrippedTokens tokens) rest context

extractSpecialTypeName :: [Pos Token] -> Maybe (SrcPos, Text, [Pos Token])
extractSpecialTypeName (Pos pos LBracket : Pos _ RBracket : rest) = Just (pos, "[]", rest)
extractSpecialTypeName (Pos pos LParen   : (tupleCommas -> (commas, Pos _ RParen : rest))) =
  Just (pos, "(" <> T.replicate commas "," <> ")", rest)
extractSpecialTypeName (tupleCommas -> (commas, Pos pos RParen : rest)) =
  Just (pos, "(" <> T.replicate commas "," <> ")", rest)
extractSpecialTypeName _ = Nothing

tupleCommas :: [Pos Token] -> (Int, [Pos Token])
tupleCommas = go 0 True
  where
    go :: Int -> Bool -> [Pos Token] -> (Int, [Pos Token])
    go !n False (Pos _ Comma : rest) = go (n + 1) True rest
    go  n False rest                 = (n, rest)
    go  n True  (Pos _ Comma : rest) = go (n + 1) True rest
    go  n True  rest'@(Pos _ (T name) : rest)
      | isTypeVarStart name = go n False rest
      | otherwise           = (n, rest')
    go  n _     rest = (n, rest)

-- same as dropWhile with counting
dropInfixTypeStart :: [Pos Token] -> [Pos Token]
dropInfixTypeStart = dropWhile f
  where
    f (Pos _ (T name)) = isInfixTypePrefix name
    f (Pos _ Backtick) = True
    f (Pos _ LParen)   = True
    f _                = False

    isInfixTypePrefix :: Text -> Bool
    isInfixTypePrefix = maybe False Char.isLower . headt

-- | It's easier to scan for tokens without pesky newlines popping up
-- everywhere.  But I need to keep the newlines in in case I hit a @where@
-- and need to call 'breakBlocks' again.
stripNewlines :: UnstrippedTokens -> [Pos Token]
stripNewlines = filter (not . isNewline) . unUnstrippedTokens

-- | hsc2hs's '#enum ... \n' or '#{enum...}' definition.
hsc2hsEnum :: [Pos Token] -> [Tag]
hsc2hsEnum = \case
  _ : Pos _ Comma : _ : Pos _ Comma : rest -> extractValues rest
  _ -> []
  where
    -- Values are not really functions, they're constants like x = 0 but there's
    -- no tag type for that.
    valueTyp = Function
    extractValues :: [Pos Token] -> [Tag]
    extractValues = \case
      Pos _ Comma : rest ->
        extractValues rest
      Pos p (T name) : Pos _ Equals : rest ->
        mkTag p name valueTyp
          : extractValues (dropUntil Comma (stripBalancedParens rest))
      Pos p (T name) : rest ->
        mkTag p (translateName name) valueTyp : extractValues rest
      _ -> []
    translateName :: Text -> Text
    translateName
      = TL.toStrict
      . TLB.toLazyText
      . snd
      . T.foldl' addChar (False, mempty)
    addChar :: (Bool, TLB.Builder) -> Char -> (Bool, TLB.Builder)
    addChar (_, acc) '_' = (True, acc)
    addChar (b, acc) c   = (False, acc <> TLB.singleton c')
      where
        c' = if b then Char.toUpper c else Char.toLower c

-- | Tags from foreign import.
--
-- e.g. @foreign import ccall safe \"name\" c_name :: ...@ will produce a tag
-- for @c_name@.
foreignTags :: [Pos Token] -> [Tag]
foreignTags decl = case decl of
  Pos _ KWImport : decl'
        | Pos pos (T name) : _ <- dropBefore isDoubleColon decl' ->
          [mkTag pos name Function]
  _ -> []
  where
    isDoubleColon (Pos _ DoubleColon) = True
    isDoubleColon _ = False

toplevelFunctionTags :: [Pos Token] -> [Tag]
toplevelFunctionTags toks = case tags of
  -- Tags of toplevel functions are all repeatable, even the ones that come
  -- from the type signature because there will definitely be tags from the
  -- body and they should be sorted out if type signature is present.
  [] -> functionTagsNoSig toks
  ts -> map toRepeatableTag ts
  where
    -- first try to detect tags from type signature, if it fails then
    -- do the actual work of detecting from body
    (tags, _) = functionTags ExpectFunctions toks
    toRepeatableTag :: Tag -> Tag
    toRepeatableTag (Tag t) = RepeatableTag t
    toRepeatableTag t       = t

functionTagsNoSig :: [Pos Token] -> [Tag]
functionTagsNoSig allToks
  -- If there’s no equals sign then this is definitely not a function/operator declaration.
  | containsEquals allToks
  = go' allToks
  | otherwise
  = []
  where
    go' :: [Pos Token] -> [Tag]
    go' (Pos _ T{} : Pos pos tok : _)
      | Just opName <- tokToOpNameExcludingBangPatSyms ExpectFunctions tok
      = [mkRepeatableTag pos opName Operator]
    go' ts = go ts

    go :: [Pos Token] -> [Tag]
    go []                           = []
    go (Pos _ LParen : Pos _ T{} : Pos _ Backtick : Pos pos' (T name') : Pos _ Backtick : Pos _ T{} : Pos _ RParen : _)
      | functionName ExpectFunctions name' = [mkRepeatableTag pos' name' Function]
    go (Pos _ LParen : Pos _ T{} : Pos pos' tok : Pos _ T{} : Pos _ RParen : _)
      | Just name' <- tokToOpName ExpectFunctions tok
      = [mkRepeatableTag pos' name' Operator]
    go toks@(Pos _ LParen : _)      = go $ stripBalancedParens toks
    go toks@(Pos _ LBrace : _)      = go $ stripBalancedBraces toks
    go toks@(Pos _ LBracket : _)    = go $ stripBalancedBrackets toks
    -- This function does not analyze type signatures.
    go (Pos _ DoubleColon : _)      = []
    go (Pos _ ExclamationMark : ts) = go ts
    go (Pos _ Tilde : ts)           = go ts
    go (Pos _ At : ts)              = go ts
    go (Pos _ Equals : _)           = functionOrOp allToks
    go (Pos _ Pipe : _)             = functionOrOp allToks
    go (Pos _ Backtick : Pos pos' (T name') : _)
      | functionName ExpectFunctions name' =
        [mkRepeatableTag pos' name' Function]
    go (Pos pos tok : _)
      | Just name <- tokToOpNameExcludingBangPatSyms ExpectFunctions tok
      = [mkRepeatableTag pos name Operator]
    go (Pos pos Dot : _)            = [mkRepeatableTag pos "." Operator]
    go (_ : ts)                     = go ts
    stripOpeningParens :: [Pos Token] -> [Pos Token]
    stripOpeningParens = dropWhile ((== LParen) . valOf)
    functionOrOp :: [Pos Token] -> [Tag]
    functionOrOp toks = case stripOpeningParens toks of
      Pos pos (T name) : _
             | functionName ExpectFunctions name ->
               [mkRepeatableTag pos name Function]
      Pos pos tok : _ -> case tokToOpName ExpectFunctions tok of
        Just name -> [mkRepeatableTag pos name Operator]
        Nothing   -> []
      [] -> []

tokToOpNameExcludingBangPatSyms :: ExpectedFuncName -> Token -> Maybe Text
tokToOpNameExcludingBangPatSyms expectation tok =
  case (expectation, tokToNameExcludingBangPatSyms tok) of
    (ExpectFunctions, res@(Just name))
            | isHaskellOp name -> res
    (ExpectConstructors, res@(Just name))
            | isHaskellConstructorOp name -> res
    _ -> Nothing

tokToOpName :: ExpectedFuncName -> Token -> Maybe Text
tokToOpName expectation tok = case (expectation, tokToName tok) of
  (ExpectFunctions, res@(Just name))
    | isHaskellOp name -> res
  (ExpectConstructors, res@(Just name))
    | isHaskellConstructorOp name -> res
  _ -> Nothing

-- | Get tags from a function type declaration: token , token , token ::
-- Return the tokens left over.
functionTags :: ExpectedFuncName -- ^ expect constructors or functions
             -> [Pos Token] -> ([Tag], [Pos Token])
functionTags constructors = go []
  where
    (opTag, funcTag) = case constructors of
      ExpectConstructors -> (Constructor, Constructor)
      ExpectFunctions    -> (Operator, Function)
    go :: [Tag] -> [Pos Token] -> ([Tag], [Pos Token])
    go tags (Pos _ LParen : opTok : Pos _ RParen : Pos _ DoubleColon : rest) =
      (reverse $ mkOpTag tags opTag opTok, rest)
    go tags (Pos pos (T name) : Pos _ DoubleColon : rest)
      | functionName constructors name =
        (reverse $ mkTag pos name funcTag : tags, rest)
    go tags (Pos _ LParen : opTok : Pos _ RParen : Pos _ Comma : rest) =
      go (mkOpTag tags opTag opTok) rest
    go tags (Pos pos (T name) : Pos _ Comma : rest)
      | functionName constructors name =
        go (mkTag pos name funcTag : tags) rest
    go tags tokens = (tags, tokens)

    mkOpTag :: [Tag] -> Type -> Pos Token -> [Tag]
    mkOpTag tags opTag' (Pos pos tok) =
      case tokToOpName constructors tok of
        Just name -> mkTag pos name opTag' : tags
        Nothing   -> tags

data ExpectedFuncName = ExpectFunctions | ExpectConstructors

functionName :: ExpectedFuncName -> Text -> Bool
functionName expect = isFunction
  where
    isFunction text = case T.uncons text of
      Just ('_', cs)
            | T.null cs -> False
      Just (c, cs) ->
        firstChar c && startIdentChar c && T.all (identChar True) cs
      Nothing      -> False
    firstChar c = case expect of
      ExpectFunctions    -> c == '_' || case Char.generalCategory c of
        Char.LowercaseLetter -> True
        Char.OtherLetter     -> True
        _                    -> False
      ExpectConstructors -> Char.isUpper c

-- | * = X *
newtypeTags :: SrcPos -> UnstrippedTokens -> [Tag]
newtypeTags _ unstripped
  | any (\case { Pos _ KWWhere -> True; _ -> False })
    (unUnstrippedTokens unstripped) =
    concatMap gadtTags (whereBlock unstripped)
newtypeTags prevPos unstripped =
  case dropUntil Equals $ stripNewlines unstripped of
    Pos pos (T name) : rest ->
      let constructor = mkTag pos name Constructor
      in  case rest of
        Pos _ LBrace : Pos funcPos (T funcName) : _ ->
          [constructor, mkTag funcPos funcName Function]
        _ ->
          [constructor]
    rest -> [unexpected prevPos unstripped rest "newtype * ="]

-- | [] (empty data declaration)
-- * = X { X :: *, X :: * }
-- * where X :: * X :: *
-- * = X | X
dataConstructorTags :: SrcPos -> UnstrippedTokens -> [Tag]
dataConstructorTags prevPos unstripped
  -- GADT
  | any (\case { Pos _ KWWhere -> True; _ -> False })
    (unUnstrippedTokens unstripped) =
    concatMap gadtTags (whereBlock unstripped)
  -- plain ADT
  | otherwise = case strip unstripped of
    [] -> [] -- empty data declaration
    rest | Just (Pos pos (T name), rest') <- extractInfixConstructor rest ->
           mkTag pos name Constructor : collectRest rest'
    rest | Just (pos, name, rest') <- extractSpecialTypeName rest ->
           mkTag pos name Constructor : collectRest rest'
    Pos pos (T name) : rest ->
      mkTag pos name Constructor : collectRest rest
    Pos _ LParen : Pos pos (T name) : Pos _ RParen : rest ->
      mkTag pos name Constructor : collectRest rest
    rest -> [unexpected prevPos unstripped rest "data * = *"]
  where
    strip :: UnstrippedTokens -> [Pos Token]
    strip = stripOptBang . stripDatatypeContext . dropUntil Equals
          . stripNewlines
    collectRest :: [Pos Token] -> [Tag]
    collectRest tokens
      | (tags@(_:_), rest) <- functionTags ExpectFunctions tokens =
        tags ++ collectRest (dropUntilNextField rest)
    collectRest toks@(Pos _ LParen : _) =
      collectRest $ stripBalancedParens toks -- dropUntilNextField rest
    collectRest (Pos pipePos Pipe : rest)
      | Just (Pos pos (T name), rest'') <- extractInfixConstructor rest' =
        mkTag pos name Constructor : collectRest rest''
      | Just (pos, name, rest'') <- extractSpecialTypeName rest' =
        mkTag pos name Constructor : collectRest rest''
      | Pos pos (T name) : rest'' <- rest'
        , functionName ExpectConstructors name =
        mkTag pos name Constructor
          : collectRest (dropUntilNextCaseOrRecordStart rest'')
      | Pos _ LParen : Pos pos (T name) : Pos _ RParen : rest'' <- rest'
        , isHaskellConstructorOp name =
        mkTag pos name Constructor
          : collectRest (dropUntilNextCaseOrRecordStart rest'')
      | otherwise =
        [unexpected pipePos unstripped rest "| not followed by tokens"]
      where
        rest' = stripOptBang $ stripDatatypeContext rest
    collectRest (_ : rest) = collectRest rest
    collectRest [] = []

    stripOptBang :: [Pos Token] -> [Pos Token]
    stripOptBang (Pos _ ExclamationMark : rest) = rest
    stripOptBang ts = ts

    extractInfixConstructor :: [Pos Token] -> Maybe (Pos Token, [Pos Token])
    extractInfixConstructor = extract . stripTypeParam
      where
        extract :: [Pos Token] -> Maybe (Pos Token, [Pos Token])
        extract (tok@(Pos _ (T name)) : rest)
          | isHaskellConstructorOp name = Just (tok, stripTypeParam rest)
        extract (Pos _ Backtick : tok@(Pos _ _) : Pos _ Backtick : rest) =
          Just (tok, stripTypeParam rest)
        extract _ = Nothing

        stripTypeParam :: [Pos Token] -> [Pos Token]
        stripTypeParam input@(Pos _ LParen : _) =
          stripBalancedParens input
        stripTypeParam input@(Pos _ LBracket : _) =
          stripBalancedBrackets input
        stripTypeParam ts = dropWhile isTypeParam $ drop 1 ts

        isTypeParam :: Pos Token -> Bool
        isTypeParam (Pos _ (T name)) = isTypeVarStart name
        isTypeParam _                = False

    dropUntilNextCaseOrRecordStart :: [Pos Token] -> [Pos Token]
    dropUntilNextCaseOrRecordStart = dropWithStrippingBalanced $
      not . \case { Pipe -> True; LBrace -> True; _ -> False }

    dropUntilNextField :: [Pos Token] -> [Pos Token]
    dropUntilNextField = dropWithStrippingBalanced $
      not . \case { Comma -> True; RBrace -> True; Pipe -> True; _ -> False }

stripDatatypeContext :: [Pos Token] -> [Pos Token]
stripDatatypeContext = stripOptContext . stripOptForall

stripOptForall :: [Pos Token] -> [Pos Token]
stripOptForall (Pos _ (T "forall") : rest) = dropUntil Dot rest
stripOptForall xs                          = xs

stripParensKindsTypeVars :: [Pos Token] -> [Pos Token]
stripParensKindsTypeVars (Pos _ LParen : xs)  =
  stripParensKindsTypeVars xs
stripParensKindsTypeVars (Pos _ DoubleColon : xs) =
  stripParensKindsTypeVars $ drop 1 $
    dropWithStrippingBalanced (\case { RParen -> False; _ -> True }) xs
stripParensKindsTypeVars (Pos _ (T name) : xs)
  | isTypeVarStart name = stripParensKindsTypeVars xs
stripParensKindsTypeVars xs = xs

stripOptContext :: [Pos Token] -> [Pos Token]
stripOptContext (stripBalancedParens -> Pos _ Implies : xs) = xs
stripOptContext origToks = go origToks
  where
    go (Pos _ Implies : xs)    = xs
    go (Pos _ Equals : _)      = origToks
    go (Pos _ Pipe : _)        = origToks
    go (Pos _ LBrace : _)      = origToks
    go (Pos _ RBrace : _)      = origToks
    go toks@(Pos _ LParen : _) = go $ stripBalancedParens toks
    go (Pos _ DoubleColon : _) = origToks
    go (_ : xs)                = go xs
    go []                      = origToks

-- | Drop all tokens for which @pred@ returns True, also drop () or []
-- parenthesized expressions.
dropWithStrippingBalanced :: (Token -> Bool) -> [Pos Token] -> [Pos Token]
dropWithStrippingBalanced p = go
  where
    go input@(Pos _ LParen : _)   = go $ stripBalancedParens input
    go input@(Pos _ LBracket : _) = go $ stripBalancedBrackets input
    go (Pos _ tok : xs) | p tok   = go xs
    go xs = xs

stripBalancedParens :: [Pos Token] -> [Pos Token]
stripBalancedParens = stripBalanced LParen RParen

stripBalancedBrackets :: [Pos Token] -> [Pos Token]
stripBalancedBrackets = stripBalanced LBracket RBracket

stripBalancedBraces :: [Pos Token] -> [Pos Token]
stripBalancedBraces = stripBalanced LBrace RBrace

stripBalanced :: Token -> Token -> [Pos Token] -> [Pos Token]
stripBalanced open close (Pos _ tok : xs)
  | tok == open = go 1 xs
  where
    go :: Int -> [Pos Token] -> [Pos Token]
    go 0 ys = ys
    go n (Pos _ tok' : ys)
      | tok' == open  = go (n + 1) ys
      | tok' == close = go (n - 1) ys
    go n (_: ys) = go n ys
    go _ []      = []
stripBalanced _ _ xs = xs

gadtTags :: UnstrippedTokens -> [Tag]
gadtTags unstripped = case dropDataContext rest of
  Pos _ LBrace : rest' -> constructorTag ++ collectFields rest'
  _                    -> constructorTag
  where
    (constructorTag, rest) =
      functionTags ExpectConstructors $ stripNewlines unstripped
    collectFields :: [Pos Token] -> [Tag]
    collectFields (Pos _ Comma : rest') = collectFields rest'
    collectFields (Pos _ RBrace : _)    = []
    collectFields tokens
      | (tags@(_:_), rest') <- functionTags ExpectFunctions tokens =
        tags ++ collectFields (dropUntilNextField rest')
      | otherwise = []
    dropUntilNextField :: [Pos Token] -> [Pos Token]
    dropUntilNextField = dropWithStrippingBalanced $
      not . \case { Comma -> True; RBrace -> True; _ -> False }

-- | * => X where X :: * ...
classTags :: SrcPos -> UnstrippedTokens -> [Tag]
classTags prevPos unstripped =
  maybeToList classTag
    ++ map (addParent classTag)
         (concatMap classBodyTags (whereBlock wherePart))
  where
    (classPart, wherePart) = spanUntil KWWhere unstripped
    (classTag, _, _) = recordVanillaOrInfixName isTypeName Class prevPos
      "class * =>" $ stripUntilImplies $ stripNewlines classPart

stripUntilImplies :: [Pos Token] -> [Pos Token]
stripUntilImplies xs = case dropUntil Implies xs of
  []  -> xs
  xs' -> xs'

classBodyTags :: UnstrippedTokens -> [Tag]
classBodyTags unstripped = case stripNewlines unstripped of
  Pos _ KWType : Pos pos (T name) : _ -> [mkTag pos name Family]
  Pos _ KWData : Pos pos (T name) : _ -> [mkTag pos name Family]
  tokens -> fst $ functionTags ExpectFunctions tokens

-- | Skip to the where and split the indented block below it.
whereBlock :: UnstrippedTokens -> [UnstrippedTokens]
whereBlock =
  concatMap (breakBlocks ProcessVanilla . UnstrippedTokens) .
    explodeToplevelBracedBlocks .
      dropUntil KWWhere .
        unUnstrippedTokens

instanceTags :: SrcPos -> UnstrippedTokens -> [Tag]
instanceTags prevPos unstripped =
  -- instances can offer nothing but some fresh data constructors since
  -- the actual datatype is really declared in the class declaration
  concatMap newtypeDecl (map (dropTokens 1) (filter isNewtypeDecl block))
    ++ concatMap dataDecl (map (dropTokens 1) (filter isDataDecl block))
  where
    newtypeDecl toks = map (addParent parent) $ newtypeTags pos toks
      where
        (parent, pos) = extractFamilyName prevPos "newtype instance * ="
          (stripNewlines toks)
    dataDecl toks = map (addParent parent) $ dataConstructorTags pos toks
      where
        (parent, pos) = extractFamilyName prevPos "data instance * ="
          (stripNewlines toks)
    block = whereBlock unstripped

    isNewtypeDecl :: UnstrippedTokens -> Bool
    isNewtypeDecl (UnstrippedTokens (Pos _ KWNewtype : _)) = True
    isNewtypeDecl _ = False

    isDataDecl :: UnstrippedTokens -> Bool
    isDataDecl (UnstrippedTokens (Pos _ KWData : _)) = True
    isDataDecl _ = False

extractFamilyName :: SrcPos -> Doc Void -> [Pos Token] -> (Maybe Tag, SrcPos)
extractFamilyName prevPos context toks = (tag, pos)
  where
    (tag, pos, _) = recordVanillaOrInfixName isTypeFamilyName Family prevPos context toks

-- * util

addParent :: Maybe Tag -> Tag -> Tag
addParent parent = onTagVal f
  where
    f (Pos pos (TagVal name typ _)) =
      Pos pos (TagVal name typ parentName)
    parentName :: Maybe ParentTag
    parentName = join $ extractParent <$> parent

mkTag :: SrcPos -> Text -> Type -> Tag
mkTag pos name typ = Tag $ Pos pos (TagVal name typ Nothing)

mkRepeatableTag :: SrcPos -> Text -> Type -> Tag
mkRepeatableTag pos name typ =
  RepeatableTag $ Pos pos TagVal
    { tvName   = name
    , tvType   = typ
    , tvParent = Nothing
    }

warning :: SrcPos -> Doc Void -> Tag
warning pos warn = Warning $ Pos pos $ pretty pos <> ":" <+> warn

unexpected :: SrcPos -> UnstrippedTokens -> [Pos Token] -> Doc Void -> Tag
unexpected prevPos (UnstrippedTokens tokensBefore) tokensHere declaration =
  warning pos ("unexpected" <+> thing <+> "after" <+> declaration)
  where
    thing = maybe "end of block" (pretty . valOf) (mhead tokensHere)
    pos
      | Just t <- mhead tokensHere   = posOf t
      | Just t <- mlast tokensBefore = posOf t
      | otherwise                    = prevPos

isNewline :: Pos Token -> Bool
isNewline (Pos _ (Newline _)) = True
isNewline _                   = False

containsEquals :: [Pos Token] -> Bool
containsEquals = any (\case { Pos _ Equals -> True; _ -> False; })

dropUntil :: Token -> [Pos Token] -> [Pos Token]
dropUntil token = drop 1 . dropWhile (not . (== token) . valOf)

spanUntil :: Token -> UnstrippedTokens
                      -> (UnstrippedTokens, UnstrippedTokens)
spanUntil token
  = (UnstrippedTokens *** UnstrippedTokens)
  . span (not . (== token) . valOf)
  . unUnstrippedTokens

-- | Drop until the element before the matching one. Return [] if the function never matches.
dropBefore :: (a -> Bool) -> [a] -> [a]
dropBefore f = go
  where
    go [] = []
    go [_] = []
    go xs@(_ : rest@(y:_))
      | f y       = xs
      | otherwise = go rest

headt :: Text -> Maybe Char
headt = fmap fst . T.uncons

mhead :: [a] -> Maybe a
mhead [] = Nothing
mhead (x:_) = Just x

mlast :: [a] -> Maybe a
mlast xs
  | null xs   = Nothing
  | otherwise = Just (L.last xs)

isSymbolCharacterCategory :: Char.GeneralCategory -> Bool
isSymbolCharacterCategory cat = case cat of
  Char.ConnectorPunctuation -> True
  Char.DashPunctuation      -> True
  Char.OtherPunctuation     -> True
  Char.MathSymbol           -> True
  Char.CurrencySymbol       -> True
  Char.ModifierSymbol       -> True
  Char.OtherSymbol          -> True
  _                         -> False


