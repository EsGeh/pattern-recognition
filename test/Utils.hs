module Utils where

import PatternRecogn.Types


calcClassificationQuality :: VectorOf Label -> VectorOf Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res
