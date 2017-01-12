{-# LANGUAGE FlexibleInstances #-}
module PatternRecogn.NeuronalNetworks.TrainingMonad(
	TrainingMonadT,
	askIteration, askLastVals, askLastGradients, askLastStepWidths,
	getState, modifyState, putState,
	liftToTrainingMonad,
) where

import PatternRecogn.NeuronalNetworks.Types
import PatternRecogn.Types hiding( cond )
import PatternRecogn.Utils
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.DeepSeq


type TrainingMonadT state m = ReaderT TrainingCtxt (StateT state m)

data TrainingCtxt
	= TrainingCtxt {
		trainingCtxt_it :: Int,
		trainingCtxt_lastVals :: [ClassificationParam],
		trainingCtxt_lastGradients :: [[Matrix]],
		trainingCtxt_lastStepWidths :: [[Matrix]]
	}

getState :: Monad m => TrainingMonadT state m state
getState = lift $ get

putState :: (NFData state, Monad m) => state -> TrainingMonadT state m ()
putState st = put $!! force st

modifyState :: Monad m => (state -> state) -> TrainingMonadT state m ()
modifyState f = modify' f

askIteration :: Monad m => TrainingMonadT state m Int
askIteration = asks trainingCtxt_it
askLastVals :: Monad m => TrainingMonadT state m [ClassificationParam]
askLastVals = asks trainingCtxt_lastVals
askLastGradients :: Monad m => TrainingMonadT state m [[Matrix]]
askLastGradients = asks trainingCtxt_lastGradients
askLastStepWidths :: Monad m => TrainingMonadT state m [[Matrix]]
askLastStepWidths = asks trainingCtxt_lastStepWidths

instance MonadLog m => MonadLog (StateT st m) where
	doLog str = lift $ doLog str

instance MonadLog m => MonadLog (ReaderT ctxt m) where
	doLog str = lift $ doLog str

liftToTrainingMonad ::
	(Monad m) =>
	(ClassificationParam -> TrainingMonadT state m TrainingState)
	-> (TrainingState -> IterationMonadT [TrainingState] (StateT state m) TrainingState)
liftToTrainingMonad f TrainingState{ nwData_weights = weights, nwData_gradients = lastGradients, nwData_stepWidths = lastStepWidth } =
	withIterationCtxt $ \it lastTrainingState ->
	runReaderT `flip` TrainingCtxt {
		trainingCtxt_it = it,
		trainingCtxt_lastVals =
			map nwData_weights lastTrainingState,
		trainingCtxt_lastGradients =
			lastGradients : map nwData_gradients lastTrainingState,
		trainingCtxt_lastStepWidths =
			lastStepWidth : map nwData_stepWidths lastTrainingState
	} $
	f weights
