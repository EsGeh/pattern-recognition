{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.ExampleClassificationAlgorithm where

import PatternRecogn.Types
import PatternRecogn.Utils
--import qualified PatternRecogn.Lina as Lina


calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	()
	
classify :: (Label, Label) -> ClassificationParam -> Matrix -> VectorOf Label
classify (labelNeg, labelPos) param input =
	-- TODO: insert code

infoStringForParam :: ClassificationParam -> String
infoStringForParam param =
	-- TODO: insert code

-- | helper functions

calcClassificationQuality :: VectorOf Label -> VectorOf Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

prependOnes m =
	konst 1 (rows m,1) ||| m
