{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types(
	module Types, 
	module Control.Monad.Except

) where

import Control.Monad.Except

import PatternRecogn.Types

type ErrT m = ExceptT String m

instance MonadLog IO where
	doLog = putStrLn

instance (MonadLog m) => MonadLog (ErrT m) where
	doLog = lift . doLog
