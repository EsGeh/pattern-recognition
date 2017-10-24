{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.NearestNeighbours where

import PatternRecogn.Lina as Lina
import PatternRecogn.Types

import Data.List( minimumBy )


-- this method will use the training set for classification
type ClassificationParam =
	TrainingData

-----------------------------------------------
-- calculate classification parameter:
-----------------------------------------------

-- nothing to do here: just use training data as parameter for classification
calcClassificationParams :: TrainingData -> ClassificationParam
calcClassificationParams = id


-----------------------------------------------
-- classify new feature vectors based on the classification parameter:
-----------------------------------------------

-- classify a row of unknown feature vectors based on the training set
classify ::
	ClassificationParam ->
	Matrix -> VectorOf Label
classify trainingData =
	Lina.fromList
	.
	map (classifySingleVector trainingData)
	.
	Lina.toRows

classifySingleVector :: ClassificationParam -> Vector -> Label
classifySingleVector trainingData input =
	snd $ -- extract the label
	minimumBy (\x y -> closestInput (fst x) (fst y)) trainingData
	where
		closestInput x y =
			(norm_2 $ x - input)
			`compare`
			(norm_2 $ y - input)
	
{-
infoStringForParam :: ClassificationParam -> String
infoStringForParam (beta,_) =
	concat $ ["beta size:", show $ size beta]
-}
