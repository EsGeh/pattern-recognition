{-# LANGUAGE TupleSections #-}
module PatternRecogn.Types(
	MonadLog(..),
	Label,
	TrainingDataBundled,
	TrainingData,
	TrainingDataBin(), toTrainingDataBin, fromTrainingDataBin,
	fromBundled, toBundled,
	module Lina
) where

import PatternRecogn.Utils
import PatternRecogn.Lina as Lina
--import Control.Monad.IO.Class
import Control.Monad

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

fromBundled :: TrainingDataBundled -> TrainingData
fromBundled =
	join
	.
	map (uncurry helper)
	where
		helper :: Matrix -> Label -> [(Vector,Label)]
		helper samples label =
			Lina.toRows samples `zip` repeat label

toBundled :: TrainingData -> TrainingDataBundled
toBundled =
	map helper
	.
	partitionBy snd
	where
		helper :: [(Vector, Label)] -> (Matrix, Label)
		helper l =
			(,label) . Lina.fromRows . map fst
			$
			l
			where
				label = snd $ head l

class (Monad m) => MonadLog m where
	doLog :: String -> m ()
