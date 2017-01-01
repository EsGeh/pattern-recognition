{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Types(
	ErrT,
	TestDataBundled,
	TestData,
	TestDataBin,
	TestDataGen(..), testData_mapToTrainData, testData_mapToTrainDataM,
	toBundledTestData, fromBundledTestData,
	testDataFromBin,
	logicalOp_testInput,
	--module Types, 
	module Control.Monad.Except

) where

import PatternRecogn.Types
import PatternRecogn.Lina as Lina

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity


type ErrT m = ExceptT String m

type TestDataBundled = TestDataGen TrainingDataBundled
type TestData = TestDataGen TrainingData
type TestDataBin = TestDataGen TrainingDataBin

data TestDataGen trainingData =
	TestData {
		testData_train :: trainingData,
		testData_input :: Maybe (Matrix, VectorOf Label) -- inputData, expected label
	}
	deriving( Show )

toBundledTestData = testData_mapToTrainData $ toBundled
fromBundledTestData = testData_mapToTrainData $ fromBundled

testDataFromBin :: TestDataBin -> TestDataBundled
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

logicalOp_testInput :: (Bool -> Bool -> Bool) -> TestData
logicalOp_testInput op =
	let
		inputData :: Num a => [(a,a)]
		inputData = liftA2 (,) [0,1] [0,1]
		expectedOutput :: [Int]
		expectedOutput = uncurry (boolOpToIntOp op) <$> inputData
	in
		TestData {
			testData_train = 
				(map (\(x,y)-> Lina.fromList [x,y]) inputData) `zip` (map fromIntegral expectedOutput)
			, testData_input = Nothing
		}

boolOpToIntOp :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
boolOpToIntOp op x y=
	case op (x>0) (y>0) of
		True -> 1
		_ -> 0


instance MonadLog IO where
	doLog = putStrLn

instance (MonadLog m) => MonadLog (ErrT m) where
	doLog = lift . doLog
