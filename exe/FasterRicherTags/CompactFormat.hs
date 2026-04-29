-- |
-- Module:     FasterRicherTags.CompactFormat
-- Copyright:  (c) Sergey Vinokurov 2025
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

module FasterRicherTags.CompactFormat
  ( writeTo
  ) where

import Data.Bifunctor (second)
import Data.ByteString.Char8 qualified as C8
import Data.Containers.ListUtils qualified as L
import Data.Foldable (for_)
import Data.List qualified as L
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Internal qualified as TI
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLBI
import Data.Text.Unsafe qualified as TU
import GHC.Magic (inline)
import System.IO (Handle, hPutChar)

import Data.Symbols

import Haskell.Language.Tags.Types

getUniqKey :: ResolvedSymbol -> (UnqualifiedSymbolName, Type, Maybe ParentTag)
getUniqKey x =
  ( resolvedSymbolName x
  , resolvedSymbolType x
  -- Take parent into account so that e.g. accessors in different records
  -- will not get deduplicated.
  , resolvedSymbolParent x
  )

writeTo :: Handle -> [(Text, [ResolvedSymbol])] -> IO ()
writeTo dest xs = do
  list $
    for_ (map (second (L.nubOrdOn getUniqKey . L.sortBy (comparing (\x -> (resolvedSymbolName x, resolvedSymbolType x, resolvedSymbolLine x))))) $ L.sortBy (comparing fst) xs) $ \(fn, tags) -> list $ do
      txt fn
      for_ tags $ \sym ->
        list $ do
          int $ unLine $ resolvedSymbolLine sym
          txt $ unqualSymNameText $ resolvedSymbolName sym
          bool True -- unconditionally public
          rawchar ' '
          case resolvedSymbolParent sym of
            Nothing                                    -> do
              rawchar '.'
              lispChar $ toType $ resolvedSymbolType sym
              pure ()
            Just ParentTag{ptName, ptType} -> do
              lispChar $ toType $ resolvedSymbolType sym
              txt ptName
              rawchar '.'
              lispChar $ toType ptType
  where
    list :: IO a -> IO a
    list action = rawchar '(' *> action <* rawchar ')'

    -- _bstr :: ByteString -> IO ()
    -- _bstr s = do
    --   rawchar '"'
    --   () <- foldBSM qchar s
    --   rawchar '"'

    txt :: Text -> IO ()
    txt s = do
      rawchar '"'
      textFoldM_ qchar s
      rawchar '"'

    rawchar :: Char -> IO ()
    rawchar = hPutChar dest

    qchar :: Char -> IO ()
    qchar = \case
      '"'  -> C8.hPutStr dest "\\\""
      '\\' -> C8.hPutStr dest "\\\\"
      c    -> rawchar c

    lispChar :: Char -> IO ()
    lispChar c = do
      rawchar '?'
      qchar c

    int :: Int -> IO ()
    int = T.hPutStr dest . TL.toStrict . TLB.toLazyText . TLBI.decimal

    bool :: Bool -> IO ()
    bool x = T.hPutStr dest $ if x then "t" else "nil"

-- foldBSM :: (Monoid a, MonadIO m) => (Char -> m a) -> ByteString -> m a
-- foldBSM f (BSI.BS ptr len) = do
--   let ptr' = unsafeForeignPtrToPtr ptr
--   let go !acc !n
--         | n == len
--         = pure acc
--         | otherwise
--         = do
--           b <- liftIO $ peekByteOff ptr' n
--           x <- f (BSI.w2c b)
--           go (acc <> x) (n + 1)
--   res <- go mempty 0
--   liftIO $ touchForeignPtr ptr
--   pure res

{-# INLINE textFoldM_ #-}
textFoldM_ :: forall m. Monad m => (Char -> m ()) -> Text -> m ()
textFoldM_ f (TI.Text arr off len) =
  textFoldLoop off
  where
    !end = off + len
    textFoldLoop :: Int -> m ()
    textFoldLoop !j
      | j >= end  = pure ()
      | otherwise = do
        let TU.Iter c delta = TU.iterArray arr j
        inline f c
        textFoldLoop (j + delta)

toType :: Type -> Char
toType typ = case typ of
  Module      -> 'm'
  Function    -> 'f'
  Class       -> 'c'
  Type        -> 't'
  Constructor -> 'C'
  Operator    -> 'o'
  Pattern     -> 'p'
  Family      -> 'F'
  Define      -> 'D'


