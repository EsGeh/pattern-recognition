module Types(
	module Types, 
	module Control.Monad.Except

) where

import Control.Monad.Except

type ErrT m a = ExceptT String m a
