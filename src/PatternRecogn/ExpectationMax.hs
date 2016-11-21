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
import Data.List
import Data.Ord
import Control.Monad.State

class (Monad m) => MonadLog m where
	doLog :: String -> m ()

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
	deriving( Show )

class_prettyShow :: Class -> String
class_prettyShow Class{..} =
	unlines $
	[ concat $ ["average: ", show class_min]
	, concat $ ["cov: ", show $ class_cov]
	]

calcClassificationParams count =
	fmap head
	.
	calcClassificationParams_AllSteps 10 count

calcClassificationParams_AllSteps ::
	(MonadRandom m, MonadLog m) =>
	Int
	-> Int
	-> Matrix -> m [ClassificationParam]
calcClassificationParams_AllSteps maxIterations classCount set =
	fmap reverse $
	(
		iterateWhileM maxIterations (const True) $
		(em_step set)
	)
	=<<
	startExpect classCount set

em_step :: MonadLog m => Matrix -> Classes -> m Classes
em_step set classes =
	--logBeforeAndAfter "em_step" classes $
	(maximize <=<
	expect classes) $
	toRows set

expect :: MonadLog m => Classes -> [Vector] -> m [(Vector, Int)]
expect classes =
	--(\x -> doLog (concat ["expect: ", show $ take 20 $ map (show . snd) x]) >> return x)
	-- <=<
	(
		return .
		map (\v -> (v, nextClass v classes))
	)

maximize :: MonadLog m => [(Vector, Int)] -> m Classes
maximize =
	return .
	map calcClass
	.
	map (map fst) -- [[Vector]]
	.
	partitionBy snd
	where
		calcClass :: [Vector] -> Class
		calcClass points =
			ret
			where
				ret =
					Class {
						class_min = average points,
						class_cov = cov_SAFE (class_min ret) (fromRows points)
					}

partitionBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
partitionBy indexF =
	foldl (concIfEq indexF) []
	.
	sortOn (indexF . head)
	.
	groupBy (\a b -> indexF a == indexF b)

concIfEq :: (Eq b) => (a -> b) -> [[a]] -> [a] -> [[a]]
concIfEq _ a [] = a
concIfEq _ [] a = [a]
concIfEq indexF partitions@(firstPart:restParts) b
	| indexF (head firstPart) == indexF (head b) = (b ++ firstPart):restParts
	| otherwise = b : partitions

-- |calculates the class next to a vector:
nextClass :: Vector -> Classes -> Int
nextClass point =
	maxIndex
	.
	map (\Class{..} -> mahalanobis class_min class_cov point)
	where
		maxIndex :: [Double] -> Int
		maxIndex xs =
			snd $
			maximumBy (comparing fst) (zip xs [0..])

startExpect :: forall m . MonadRandom m => Int -> Matrix -> m Classes
startExpect classCount set  =
	let
		((minX, minY),(maxX, maxY)) =
			minMaxVec $
			map vecToTuple $
			toRows set
	in
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

-- helpers
--------------------------------------------------------------

-- copied from branch gaussClassification:
average x =
	(/ fromIntegral (length x)) $
	sum $
	x

cov_SAFE min set =
	if det cov > 0.01
	then cov
	else cov + alpha * ident (rows cov)
	where
		cov = covariance min set
		alpha = 0.01

covariance :: Vector -> Matrix -> Matrix
covariance min set =
	let
		centeredAroundMin = set - repmat (asRow min) countSamples 1
		countSamples = rows set
	in
		(/ fromIntegral countSamples) $
		sum $
		map (\v -> v `outer` v) $
		toRows $
		centeredAroundMin

mahalanobis :: Vector -> Matrix -> Vector -> R
mahalanobis min cov x =
	let
		centeredX = x - min
		inputDist =
			centeredX `dot` (inv cov #> centeredX)
	in
		1/sqrt (det (2 * pi * cov))
		*
		exp (-1/2 * inputDist)

-- | iterate a function a number of times while a condition is true
iterateWhileM ::
	forall a m .
	(Monad m) =>
	Int -- max iterations
	-> ([a] -> Bool) -- condition to continue
	-> (a -> m a) -> a -- function and start value
	-> m [a]
iterateWhileM maxIt cond f =
	flip evalStateT (maxIt, []) .
	iterateM f'
	where
		f' :: a -> StateT (Int, [a]) m (Maybe a)
		f' x =
			do
				(i, oldVals) <- get
				if i > 0 && cond (x:oldVals)
					then
						do
							put (i-1, x:oldVals)
							lift $
								(Just <$> f x)
					else
						return Nothing

iterateM ::
	(Monad m) =>
	(a -> m (Maybe a)) -> a -> m [a]
iterateM f x =
	do
		maybeNextVal <- f x
		fmap (x :) $
			case maybeNextVal of
				Nothing -> return []
				Just nextVal -> iterateM f nextVal

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
