{-# LANGUAGE FlexibleInstances #-}
module PatternRecogn.NeuronalNetworks.TrainingMonad(
	TrainingMonadT,
	askIteration, askLastVals, askLastGradients, askLastStepWidths,
	liftToTrainingMonad,
) where

import PatternRecogn.NeuronalNetworks.Types
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils
import Control.Monad.Reader


type TrainingMonadT m = ReaderT TrainingCtxt m

data TrainingCtxt
	= TrainingCtxt {
		trainingCtxt_it :: Int,
		trainingCtxt_lastVals :: [ClassificationParam],
		trainingCtxt_lastGradients :: [[Matrix]],
		trainingCtxt_lastStepWidths :: [[Matrix]]
	}

askIteration :: Monad m => TrainingMonadT m Int
askIteration = asks trainingCtxt_it
askLastVals :: Monad m => TrainingMonadT m [ClassificationParam]
askLastVals = asks trainingCtxt_lastVals
askLastGradients :: Monad m => TrainingMonadT m [[Matrix]]
askLastGradients = asks trainingCtxt_lastGradients
askLastStepWidths :: Monad m => TrainingMonadT m [[Matrix]]
askLastStepWidths = asks trainingCtxt_lastStepWidths

instance MonadLog m => MonadLog (ReaderT TrainingCtxt m) where
	doLog str = lift $ doLog str

liftToTrainingMonad ::
	(Monad m) =>
	(ClassificationParam -> TrainingMonadT m NetworkTrainingData)
	-> (NetworkTrainingData -> IterationMonadT [NetworkTrainingData] m NetworkTrainingData)
liftToTrainingMonad f NetworkTrainingData{ nwData_weights = weights, nwData_gradients = lastGradients, nwData_stepWidths = lastStepWidth } =
	withIterationCtxt $ \it lastNetworkTrainingData ->
	runReaderT `flip` TrainingCtxt {
		trainingCtxt_it = it,
		trainingCtxt_lastVals =
			map nwData_weights lastNetworkTrainingData,
		trainingCtxt_lastGradients =
			lastGradients : map nwData_gradients lastNetworkTrainingData,
		trainingCtxt_lastStepWidths =
			lastStepWidth : map nwData_stepWidths lastNetworkTrainingData
	} $
	f weights
