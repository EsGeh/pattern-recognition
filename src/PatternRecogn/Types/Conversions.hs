{-# LANGUAGE TupleSections #-}
module PatternRecogn.Types.Conversions(
	toTrainingDataBin, fromTrainingDataBin,
	fromBundled, toBundled,
) where

import PatternRecogn.Types.Types
import PatternRecogn.Utils
import PatternRecogn.Lina as Lina
import Control.Monad


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
