{-# LANGUAGE FlexibleInstances #-}
module PatternRecogn.NeuronalNetworks.TrainingMonad where

import PatternRecogn.NeuronalNetworks.Types
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils
import Control.Monad.Reader


type TrainingMonadT m = ReaderT (Int,[ClassificationParam],[[Matrix]]) m
askIteration :: Monad m => TrainingMonadT m Int
askIteration = asks (\(x,_,_) -> x)
askLastVals :: Monad m => TrainingMonadT m [ClassificationParam]
askLastVals = asks (\(_,x,_) -> x)
askLastGradients :: Monad m => TrainingMonadT m [[Matrix]]
askLastGradients = asks (\(_,_,x) -> x)

instance MonadLog m => MonadLog (ReaderT (Int, [ClassificationParam], [[Matrix]]) m) where
	doLog str = lift $ doLog str

liftToTrainingMonad ::
	(Monad m) =>
	(ClassificationParam -> TrainingMonadT m NetworkTrainingData)
	-> (NetworkTrainingData -> IterationMonadT [NetworkTrainingData] m NetworkTrainingData)
liftToTrainingMonad f NetworkTrainingData{ nwData_weights = weights, nwData_gradients = lastGradients} =
	withIterationCtxt $ \it lastNetworkTrainingData ->
	runReaderT `flip` (it, map nwData_weights lastNetworkTrainingData, lastGradients : map nwData_gradients lastNetworkTrainingData) $
	f weights
