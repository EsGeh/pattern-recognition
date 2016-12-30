module PatternRecogn.Types(
	-- ClassificationParam(..),
	MonadLog(..),
	Label,
	TrainingDataBundled,
	TrainingData,
	module Lina
) where

import PatternRecogn.Lina as Lina
--import Control.Monad.IO.Class

import Foreign.C.Types( CInt )

type Label = CInt

type TrainingDataBundled =
	[(Matrix, Label)]

type TrainingData =
	[(Vector, Label)]

class (Monad m) => MonadLog m where
	doLog :: String -> m ()
