{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.ExampleClassificationAlgorithm where

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as Lina

import Foreign.C.Types( CInt )


type Matrix = Lina.Matrix Double
type Vector = Lina.Vector Double
type Label = CInt

type ClassificationParam =
	-- TODO: insert code

calcClassificationParams :: Matrix -> Matrix -> ClassificationParam
calcClassificationParams set1 set2 =
	-- TODO: insert code
	
classify :: (Label, Label) -> ClassificationParam -> Matrix -> Lina.Vector Label
classify (labelNeg, labelPos) param input =
	-- TODO: insert code


infoStringForParam :: ClassificationParam -> String
infoStringForParam param =
	-- TODO: insert code

-- | helper functions

calcClassificationQuality :: Lina.Vector Label -> Lina.Vector Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

prependOnes m =
	konst 1 (rows m,1) ||| m
