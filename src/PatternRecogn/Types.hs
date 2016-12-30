module PatternRecogn.Types(
	-- ClassificationParam(..),
	MonadLog(..),
	Label,
	module Lina
) where

import PatternRecogn.Lina as Lina
--import Control.Monad.IO.Class

import Foreign.C.Types( CInt )

type Label = CInt

-- type ClassificationParam = ()

class (Monad m) => MonadLog m where
	doLog :: String -> m ()
