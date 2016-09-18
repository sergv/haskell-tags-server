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
{-# LANGUAGE TypeSynonymInstances  #-}

module Text.PrettyPrint.Leijen.Text.Utils
  ( putDocLn
  , displayDoc
  , displayDocString
  , show'
  , show''
  , showDoc
  , docFromString
  ) where

import Control.Monad.Base
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Text.PrettyPrint.Leijen.Text as PP

putDocLn :: (MonadBase IO m) => PP.Doc -> m ()
putDocLn = liftBase . TLIO.putStrLn . displayDoc

displayDoc :: PP.Doc -> TL.Text
displayDoc = PP.displayT . PP.renderPretty 0.9 80

displayDocString :: PP.Doc -> String
displayDocString = TL.unpack . displayDoc

show' :: (Show a) => a -> T.Text
show' = T.pack . show

show'' :: (Show a) => a -> TL.Text
show'' = TL.pack . show

showDoc :: (Show a) => a -> PP.Doc
showDoc = docFromString . show

docFromString :: String -> PP.Doc
docFromString = PP.text . TL.pack
