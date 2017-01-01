{-# LANGUAGE ScopedTypeVariables #-}
module PatternRecogn.Utils where

import PatternRecogn.Types hiding( cond )
import qualified PatternRecogn.Lina as Lina hiding( cond )

import qualified Data.Vector as Vec
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.List


iterateWhileM_withCtxt ::
	forall a m .
	(Monad m) =>
	Int
	-> (a -> IterationMonadT [a] m Bool)
	-> (a -> IterationMonadT [a] m a)
	-> a
	-> m a
iterateWhileM_withCtxt numPrevVals cond f =
	iterateWithCtxtM numPrevVals f'
	where
		f' :: a -> IterationMonadT [a] m (Either a a)
		f' x = 
			do
				continue <- cond x
				if continue
				then Right <$> f x
				else return $ Left $ x

iterateWithCtxtM ::
	forall a m stop .
	(Monad m) =>
	Int -- number of previous vals to be stored
	-> (a -> IterationMonadT [a] m (Either stop a))
	-> a
	-> m stop
iterateWithCtxtM numPrevVals f start =
	evalStateT `flip` IterationCtxt 0 (Vec.fromList []) $
	iterateM `flip` start $
	f'
	where
		f' :: a -> StateT (IterationCtxt (InternalPrevVals a)) m (Either stop a)
		f' x =
			(get >>=) $
			(. (\ctxt -> (iteration ctxt, previousVals ctxt))) $
			uncurry f''
			where
				f'' :: Int -> InternalPrevVals a -> StateT (IterationCtxt (InternalPrevVals a)) m (Either stop a)
				f'' it lastVals =
						do
							put $! IterationCtxt (it+1) $! Vec.take numPrevVals $! x `Vec.cons` lastVals
							lift $ runReaderT `flip` (IterationCtxt it $ Vec.toList lastVals) $ f x

withIterationCtxt :: Monad m => (Int -> [a] -> m b) -> IterationMonadT [a] m b
withIterationCtxt f =
	ask >>= \ctxt -> lift $ f (iteration ctxt) (previousVals ctxt)

askIt :: Monad m => IterationMonadT last m Int
askIt = asks iteration
askPreviousVals :: Monad m => IterationMonadT last m last
askPreviousVals = asks previousVals

type IterationMonadT lastVals m a = ReaderT (IterationCtxt lastVals) m a

data IterationCtxt lastVals
	= IterationCtxt {
		iteration :: Int,
		previousVals :: lastVals
	}

type InternalPrevVals a = Vec.Vector a

-- | iterate a monadic function
iterateM ::
	(Monad m) =>
	(a -> m (Either stopInfo a)) -> a -> m stopInfo
iterateM f x =
	do
		maybeNextVal <- f x
		case maybeNextVal of
			Left stopInfo -> return stopInfo
			Right nextVal -> iterateM f nextVal

-- | sort list elements into buckets based on a property:
partitionBy :: (Ord b) => (a -> b) -> [a] -> [[a]]
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

mapToFst f (a,b) = (f a, b)
mapToSnd f (a,b) = (a, f b)
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
