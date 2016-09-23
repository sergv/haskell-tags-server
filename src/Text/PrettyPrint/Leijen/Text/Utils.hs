----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Leijen.Text.Utils
-- Copyright   :  (c) Sergey Vinokurov 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  Tuesday, 30 August 2016
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Text.PrettyPrint.Leijen.Text.Utils
  ( putDocLn
  , displayDoc
  , displayDocString
  , show'
  , show''
  , showDoc
  , docFromString
  , docFrombyteString
  , (<+>)
  , ppList
  , ppDict
  , ppListWithHeader
  , ppMap
  , ppNE
  , MapEntry(..)
  , Pretty(..)
  , Doc
  ) where

import Control.Monad.Base
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TLIO
import Text.PrettyPrint.Leijen.Text (Pretty(..), Doc, (<+>))
import qualified Text.PrettyPrint.Leijen.Text as PP

putDocLn :: (MonadBase IO m) => Doc -> m ()
putDocLn = liftBase . TLIO.putStrLn . displayDoc

displayDoc :: Doc -> TL.Text
displayDoc = PP.displayT . PP.renderPretty 0.9 80

displayDocString :: Doc -> String
displayDocString = TL.unpack . displayDoc

show' :: (Show a) => a -> T.Text
show' = T.pack . show

show'' :: (Show a) => a -> TL.Text
show'' = TL.pack . show

showDoc :: (Show a) => a -> Doc
showDoc = docFromString . show

docFromString :: String -> Doc
docFromString = PP.text . TL.pack

docFrombyteString :: UTF8.ByteString -> Doc
docFrombyteString = PP.text . TLE.decodeUtf8With TEE.lenientDecode

ppList :: forall a f. (Pretty a, Functor f, Foldable f) => Doc -> Doc -> f a -> Doc
ppList left right xs =
  case toList xs of
    []   -> left <> right
    [y]  -> left <> PP.pretty y <> right
    y:ys ->
      PP.align $
      PP.group $
      left PP.<+> PP.pretty y PP.<$$>
      PP.vcat (fmap (\x -> separator PP.<+> PP.pretty x) ys) PP.<$>
      right
  where
    separator = ","

ppDict :: (Pretty a) => Doc -> [MapEntry TL.Text a] -> Doc
ppDict header entries =
  header PP.<$>
  PP.nest 2 (ppList PP.lbrace PP.rbrace entries')
  where
    entries' = map (\(k :-> v) -> PP.fillBreak maxWidth (PP.text k) :-> v) entries
    maxWidth = fromIntegral $ maximum $ map (\(k :-> _) -> TL.length k) entries

ppListWithHeader :: (Pretty a) => Doc -> [a] -> Doc
ppListWithHeader header entries =
  header PP.<$>
  PP.nest 2 (PP.vsep (map (("-" PP.<+>) . pretty) entries))

ppMap :: (Pretty a, Pretty b) => Map a b -> Doc
ppMap = ppList PP.lbrace PP.rbrace . map (uncurry (:->)) . M.toList

ppNE :: (Pretty a) => NonEmpty a -> Doc
ppNE = ppList "[" "]" . toList

infix 0 :->

data MapEntry k v = k :-> v
  deriving (Show, Eq, Ord)

instance (Pretty k, Pretty v) => Pretty (MapEntry k v) where
  pretty (x :-> y) =
    PP.group $ PP.nest 4 $ pretty (pretty x) <+> "->" PP.<$> PP.align (pretty y)
