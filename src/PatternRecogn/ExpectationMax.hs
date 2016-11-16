{-# LANGUAGE FlexibleContexts #-}
module PatternRecogn.ExpectationMax where

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as Lina

import Foreign.C.Types( CInt )


type ClassificationParam =
	Classes

type Matrix = Lina.Matrix Double
type Vector = Lina.Vector Double
type Label = CInt

type Classes = [Class]
data Class
	= Class {
		class_min :: Vector,
		class_cov :: Matrix
	}

calcClassificationParams :: Int -> Matrix -> ClassificationParam
calcClassificationParams classCount set =
	[]

infoStringForParam :: ClassificationParam -> String
infoStringForParam param =
	"classes description: <to be implemented>"

{-
classify :: (Label, Label) -> ClassificationParam -> Matrix -> Lina.Vector Label
classify (labelNeg, labelPos) param input =
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
-}
