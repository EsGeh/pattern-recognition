{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types(
	module Types, 
	module Control.Monad.Except

) where

import Control.Monad.Except
import Control.Monad.Identity

import PatternRecogn.Types

type ErrT m = ExceptT String m

type TestData = TestDataGen TrainingDataBundled
type TestDataBin = TestDataGen TrainingDataBin

data TestDataGen trainingData =
	TestData {
		testData_train :: trainingData,
		testData_input :: Maybe (Matrix, VectorOf Label) -- inputData, expected label
	}
	deriving( Show )

testDataFromBin :: TestDataBin -> TestData
testDataFromBin =
	testData_mapToTrainData $ fromTrainingDataBin

testData_mapToTrainData f = runIdentity . testData_mapToTrainDataM (return . f)
testData_mapToTrainDataM ::
	Monad m =>
	(trainingData -> m newTrainingData)
	-> TestDataGen trainingData
	-> m (TestDataGen newTrainingData)
testData_mapToTrainDataM f x =
	do
		res <- f (testData_train x)
		return $ x{ testData_train = res }

instance MonadLog IO where
	doLog = putStrLn

instance (MonadLog m) => MonadLog (ErrT m) where
	doLog = lift . doLog
