{-# LANGUAGE ScopedTypeVariables #-}
module PatternRecogn.Utils where

import PatternRecogn.Types
import qualified PatternRecogn.Lina as Lina

import Control.Monad.State
import Data.List


-- | iterate a function a number of times while a condition is true
iterateWhileM_ ::
	forall a m .
	(Monad m) =>
	Int
	-> Int -- max iterations
	-> ([a] -> Bool) -- condition to continue
	-> (a -> m a) -> a -- function and start value
	-> m [a]
iterateWhileM_ maxTemp maxIt cond f =
	flip evalStateT (maxIt, []) .
	iterateM f'
	where
		f' :: a -> StateT (Int, [a]) m (Maybe a)
		f' x =
			do
				(i, oldVals) <- get
				let newBuf = take maxTemp $ (x:oldVals)
				if i > 0 && cond newBuf
					then
						do
							put (i-1, newBuf)
							lift $
								(Just <$> f x)
					else
						return Nothing

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

uncurry3 f (a,b,c) = f a b c

setElemAt i x l =
	take i l
	++ (if i>=0 && i<length l then [x] else [])
	++ drop (i+1) l

vecFromTuple2 :: (Double,Double) -> Vector
vecFromTuple2 (x,y) =
	Lina.fromList [x,y]

vecToTuple2 :: Vector -> (Double, Double)
vecToTuple2 =
	listToTuple .
	Lina.toList
	where
		listToTuple [x,y] = (x,y)
		listToTuple _ = error "error converting list to tuple"

pnorm :: Vector -> R
pnorm x =
	sqrt $ x <.> x

sigmoid x = 1 / (1 + exp (-x))
sigmoidDerivFromRes x = x * (1 - x)
{-
sigmoidDeriv x =
	let s = sigmoid x
	in s * (1 - s)
-}

extendInputData :: Matrix -> Matrix
extendInputData m =
	konst 1 (rows m,1) ||| m
