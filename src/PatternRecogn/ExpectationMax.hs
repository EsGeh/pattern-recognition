{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module PatternRecogn.ExpectationMax where

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as Lina

import Control.Monad.Random
import Foreign.C.Types( CInt )
import Control.Monad


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

class_prettyShow :: Class -> String
class_prettyShow Class{..} =
	unlines $
	[ concat $ ["average: ", show class_min]
	, concat $ ["cov: ", show $ class_cov]
	]

calcClassificationParams ::
	MonadRandom m =>
	Int -> Matrix -> m ClassificationParam
calcClassificationParams classCount set =
	let
		minMax :: ((R,R),(R,R))
		minMax =
			minMaxVec $
			map vecToTuple $
			toRows set
	in
		uncurry (startExpect classCount) minMax

startExpect :: forall m . MonadRandom m => Int -> (R,R) -> (R,R) -> m Classes
startExpect classCount (minX, minY) (maxX, maxY) =
	do
		
		mins <-
			fmap (map vecFromTuple) $
			liftM2 zip
				(getRandomRs (minX, maxX))
				(getRandomRs (minY, maxY))
			:: m [Vector]
		let covs = repeat $ Lina.ident 2

		return $
			take classCount $
			zipWith Class mins covs

vecFromTuple :: (R,R) -> Vector
vecFromTuple (x,y) =
	Lina.fromList [x,y]

vecToTuple =
	listToTuple .
	Lina.toList
	where
		listToTuple [x,y] = (x,y)
		listToTuple _ = error "error converting list to tuple"

minMaxVec pointList =
	let
		minX = minimum $ map fst pointList
		minY = minimum $ map snd pointList
		maxX = maximum $ map fst pointList
		maxY = maximum $ map snd pointList
	in
		((minX, minY), (maxX, maxY))

infoStringForParam :: ClassificationParam -> String
infoStringForParam =
	unlines .
	(map $ class_prettyShow)

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
