----------------------------------------------------------------------------
-- |
-- Module      :  Logging
-- Copyright   :  (c) Sergey Vinokurov 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  ( errorM
  , infoM
  , debugM
  , initLogger
  , show'
  , Log.Priority(..)
  , Log.addHandler
  , Log.removeHandler
  , Log.setHandlers
  , Log.getLevel
  , Log.setLevel
  , Log.clearLevel
  , module System.Log.Handler.Simple
  ) where

import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Log.Logger as Log
import System.Log.Handler.Simple
import System.IO

errorM :: (MonadIO m) => Text -> m ()
errorM = liftIO . Log.errorM origin . T.unpack

infoM :: (MonadIO m) => Text -> m ()
infoM = liftIO . Log.infoM origin . T.unpack

debugM :: (MonadIO m) => Text -> m ()
debugM = liftIO . Log.debugM origin . T.unpack

initLogger :: (Log.Logger -> Log.Logger) -> IO ()
initLogger f = do
  -- reset global handler
  Log.updateGlobalLogger Log.rootLoggerName (Log.setHandlers ([] :: [GenericHandler Handle]))
  Log.updateGlobalLogger origin f

show' :: (Show a) => a -> Text
show' = T.pack . show

origin :: String
origin = "tags-server"
