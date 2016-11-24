{-# LANGUAGE ScopedTypeVariables #-}
module PatternRecogn.Utils where

import PatternRecogn.Types
import qualified PatternRecogn.Lina as Lina

import Control.Monad.State
import Data.List


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

-- | iterate a monadic function
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

-- | sort list elements into buckets based on a property:
partitionBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
partitionBy indexF =
	foldl (concIfEq indexF) []
	.
	sortOn (indexF . head)
	.
	groupBy (\a b -> indexF a == indexF b)

-- | internally used:
concIfEq :: (Eq b) => (a -> b) -> [[a]] -> [a] -> [[a]]
concIfEq _ a [] = a
concIfEq _ [] a = [a]
concIfEq indexF partitions@(firstPart:restParts) b
	| indexF (head firstPart) == indexF (head b) = (b ++ firstPart):restParts
	| otherwise = b : partitions

vecFromTuple :: (Double,Double) -> Vector
vecFromTuple (x,y) =
	Lina.fromList [x,y]

vecToTuple :: Vector -> (Double, Double)
vecToTuple =
	listToTuple .
	Lina.toList
	where
		listToTuple [x,y] = (x,y)
		listToTuple _ = error "error converting list to tuple"
