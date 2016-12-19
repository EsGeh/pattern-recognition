{-# LANGUAGE ScopedTypeVariables #-}
module PatternRecogn.Utils where

import PatternRecogn.Types
import qualified PatternRecogn.Lina as Lina

import Control.Monad.State.Strict
import Data.List

iterateWhileM cond f start =
	fst <$>
	iterateWhileM_ext cond' f start
	where
		cond' it vals = return $
			if cond it vals
			then Nothing
			else Just ()

iterateWhileM_ext ::
	forall a m stopInfo .
	Monad m =>
	(Int -> [a] -> m (Maybe stopInfo))
	-> (Int -> a -> m a)
	-> a
	-> m ([a], stopInfo)
iterateWhileM_ext cond f x =
	flip evalStateT (0, [x]) $
	iterateM f' x
	where
		f' :: a -> StateT (Int, [a]) m (Either stopInfo a)
		f' x =
			get >>= \(it, oldVals) ->
				do
					continue <- lift $ cond it oldVals
					case continue of
						Nothing ->
							do
								newVal <- lift $ f it x
								put (it+1, newVal: oldVals)
								return $ Right $ newVal
						Just stopInfo ->
							return $ Left stopInfo

-- | iterate a monadic function
iterateM ::
	(Monad m) =>
	(a -> m (Either stopInfo a)) -> a -> m ([a], stopInfo)
iterateM f x =
	do
		maybeNextVal <- f x
		case maybeNextVal of
			Left stopInfo -> return ([], stopInfo)
			Right nextVal ->
				do
					(restRet, stopInfo) <- iterateM f nextVal
					return (x: restRet, stopInfo)
		{-
		fmap (x :) $
			case maybeNextVal of
				Nothing -> return []
				Just nextVal -> iterateM f nextVal
		-}

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
