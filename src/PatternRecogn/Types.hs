module PatternRecogn.Types(
	MonadLog(..),
	Label,
	TrainingDataBundled,
	TrainingData,
	TrainingDataBin(), toTrainingDataBin, fromTrainingDataBin,
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

newtype TrainingDataBin
	= TrainingDataBin {
		fromTrainingDataBin :: TrainingDataBundled
	}

toTrainingDataBin bundled =
	let
		potentialData = take 2 bundled
	in
		if
			length potentialData == 2
		then
			Just $ TrainingDataBin potentialData
		else
			Nothing


class (Monad m) => MonadLog m where
	doLog :: String -> m ()
