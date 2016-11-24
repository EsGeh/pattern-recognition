module PatternRecogn.Types(
	-- ClassificationParam(..),
	MonadLog(..),
	Label,
	module Lina
) where

import PatternRecogn.Lina as Lina

import Foreign.C.Types( CInt )

type Label = CInt

-- type ClassificationParam = ()

class (Monad m) => MonadLog m where
	doLog :: String -> m ()
