{-# LANGUAGE ScopedTypeVariables #-}
module PatternRecogn.NearestNeighbours where

import PatternRecogn.Lina as Lina
import PatternRecogn.Types

import Data.List( sortBy )
import Data.Tuple( swap )
import qualified Data.Map as M


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
	Int -- ^ how many neighbours to consider
	-> ClassificationParam ->
	Matrix -> VectorOf Label
classify k trainingData =
	Lina.fromList
	.
	map (classifySingleVector k trainingData)
	.
	Lina.toRows

classifySingleVector ::
	Int -- ^ how many neighbours to consider
	-> ClassificationParam ->
	Vector -> Label
classifySingleVector k trainingData input =
	snd . M.findMax . swapMap $
	count $
	map snd $ -- extract the label
	take k $
	sortBy (\x y -> closestInput (fst x) (fst y)) $
	trainingData
	where
		closestInput x y =
			(norm_2 $ x - input)
			`compare`
			(norm_2 $ y - input)

swapMap :: Ord a => M.Map k a -> M.Map a k
swapMap = M.fromList . map swap . M.toList

count :: forall a . Ord a => [a] -> M.Map a Int
count =
	foldr conc M.empty
	where
		conc :: a -> M.Map a Int -> M.Map a Int
		conc x acc =
			case M.lookup x acc of
				Nothing -> M.insert x 1 acc
				Just _ ->
					M.adjust (+1) x acc

{-
infoStringForParam :: ClassificationParam -> String
infoStringForParam (beta,_) =
	concat $ ["beta size:", show $ size beta]
-}
