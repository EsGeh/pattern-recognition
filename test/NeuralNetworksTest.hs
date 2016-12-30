{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Types
import TestMultiple as Test

import qualified PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.Utils
import PatternRecogn.Types

import PatternRecogn.Lina as Lina

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char
import Control.Applicative
import Data.List( intercalate )

trainingDataFormat =
	CSV.defaultDecodeOptions

inputDataFormat =
	CSV.defaultDecodeOptions{
		CSV.decDelimiter = fromIntegral (ord ' ')
	}


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

main :: IO ()
main =
	handleErrors $
	do

		doLog $ "-------------------------------------------"
		doLog $ "testing with operator \"and\"..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				maxIt = 1000,
				learnRate = 0.1,
				stopConds = [Test.StopIfQualityReached 1, Test.StopIfConverges 0.001],
				networkParams = Test.NetworkParams{
					dims = [2],
					outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "testing with operator \"or\"..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				maxIt = 1000,
				learnRate = 0.1,
				stopConds = [Test.StopIfQualityReached 1, Test.StopIfConverges 0.001],
				networkParams = Test.NetworkParams{
					dims = [2],
					outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (||))

		doLog $ "-------------------------------------------"
		doLog $ "testing with operator \"xor\"..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				maxIt = 1000,
				learnRate = 0.1,
				stopConds = [Test.StopIfQualityReached 1],
				networkParams = Test.NetworkParams{
					dims = [2],
					outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (\x y -> x && not y || y && not x))

		let
			labels = [3,5,7,8]
			paths = map pathFromLabel labels
		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 100,
				maxIt = 1000,
				learnRate = 1,
				stopConds = [Test.StopIfConverges 0.01, Test.StopIfQualityReached 1],
				networkParams = Test.NetworkParams{
					dims = [10],
					outputInterpretation = (NN.outputInterpretationMaximum 10)
				}
			}
			=<< readTestInput (paths `zip` labels)

		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 100,
				maxIt = 1000,
				learnRate = 1,
				stopConds = [Test.StopIfConverges 0.01, Test.StopIfQualityReached 1],
				networkParams = Test.NetworkParams{
					dims = [10,10],
					outputInterpretation = (NN.outputInterpretationMaximum 10)
				}
			}
			=<< readTestInput (paths `zip` labels)

		{-
		forM_ (allPairs [3,5,7,8]) $
			(uncurry4 testWithDataBinary . uncurry testParamsFromLabels)
		-}
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

{-
testWithData :: (Test.AlgorithmInput -> ErrT IO Double) -> [(FilePath, Label)] -> ErrT IO ()
testWithData testFunc l =
	do
		liftIO $ putStrLn $ startToClassifyInfoStr l
		testInput <-
			readTestInput l :: ErrT IO Test.AlgorithmInput
		testFunc testInput
			>>= \quality -> liftIO $ putStrLn $ concat $ ["quality:", show $ quality]
		return ()
-}

{-
testWithDataBinary :: FilePath -> FilePath -> Label -> Label -> ErrT IO ()
testWithDataBinary
		trainingFile1 trainingFile2
		label1 label2
	=
	do
		liftIO $ putStrLn $ concat $
			[ "----------------------------------------------\n"
			, "classifying to labels ", show [label1, label2]
			, " in files ", show [trainingFile1, trainingFile2]
			]
		testInput <-
			readTestInputBinary
				trainingFile1 trainingFile2
				label1 label2
			:: ErrT IO (AlgorithmInput, Vector)
		testPerceptron label1 label2 testInput
			>>= \quality -> liftIO $ putStrLn $ concat $ ["perceptron quality:", show $ quality]
		return ()
-}

startToClassifyInfoStr l =
	concat $
			[ "----------------------------------------------\n"
			, "classifying to labels ", intercalate "," $ map (show . snd) l
			, " in files ", intercalate "," $ map (show . fst) l
			]
-- (helpers: )
-----------------------------------------------------------------

logicalOp_testInput :: (Bool -> Bool -> Bool) -> Test.AlgorithmInput
logicalOp_testInput op =
	let
		inputData :: Num a => [(a,a)]
		inputData = liftA2 (,) [0,1] [0,1]
		expectedOutput :: [Int]
		expectedOutput = uncurry (boolOpToIntOp op) <$> inputData
	in
		Test.AlgorithmInput {
			algInput_train = 
				map toTrainingData $
				partitionBy snd $
				inputData `zip` expectedOutput
			, algInput_input = Nothing
		}

toTrainingData :: [((Int,Int),Int)] -> (Matrix,Label)
toTrainingData l =
	let
		label = snd $ head l
	in
		(,fromIntegral label) $
		Lina.fromLists $
		map (map fromIntegral) $
		map ((\(x,y) -> [x,y]) . fst) l

boolOpToIntOp :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
boolOpToIntOp op x y=
	case op (x>0) (y>0) of
		True -> 1
		_ -> 0

readTestInput :: [(FilePath,Label)] -> ErrT IO Test.AlgorithmInput
readTestInput l =
	let
		paths = map fst l
		labels = map snd l
	in
	do
		trainingSets <-
			mapM (loadMatrixFromFile trainingDataFormat) paths
		(expectedLabels_raw, inputData) <-
			prepareInputData (`elem` (map fromIntegral labels)) <$>
			loadMatrixFromFile inputDataFormat  "resource/zip.test"
		let expectedLabels =
			(cmap truncate) expectedLabels_raw
		return $
			Test.AlgorithmInput {
				algInput_train = (trainingSets `zip` labels),
				algInput_input = Just (inputData, expectedLabels)
			}

{-
readTestInputBinary :: FilePath -> FilePath -> Label -> Label -> ErrT IO (AlgorithmInput, Vector)
readTestInputBinary
		trainingFile1 trainingFile2
		label1 label2
	=
	do
		[trainingSet1, trainingSet2] <-
			mapM (loadMatrixFromFile trainingDataFormat) $
			[ trainingFile1
			, trainingFile2
			]
		(inputLabels, inputData) <-
			prepareInputData (`elem` [fromIntegral label1, fromIntegral label2]) <$>
			loadMatrixFromFile inputDataFormat "resource/zip.test"
		return $ (
			AlgorithmInput {
				algInput_train1 = trainingSet1,
				algInput_train2 = trainingSet2,
				algInput_input = inputData
			}
			,
			inputLabels
			)
-}

-- | given the input data as a matrix the rows are filtered by a condition
prepareInputData :: (Double -> Bool) -> Matrix -> (Vector, Matrix)
prepareInputData filterRowsBy = 
	(\rawInput ->
		(
			flatten $ (rawInput ?? (All, Take 1)),
			rawInput ?? (All, Drop 1)
		)
	)
	.
	fromLists
	.
	filter (filterRowsBy . head)
	.
	toLists

pathFromLabel :: Label -> FilePath
pathFromLabel =
	("resource/train." ++) . show

testParamsFromLabels x y =
	let
		[filePath1, filePath2] = map pathFromLabel [x,y]
	in
		(filePath1, filePath2, x, y)

-----------------------------------------------------------------
-- utils:
-----------------------------------------------------------------

loadMatrixFromFile :: CSV.DecodeOptions -> FilePath -> ErrT IO Matrix
loadMatrixFromFile fmtOpts path =
	(fromRows . Vec.toList) <$>
	(
		ExceptT $
		fmap (CSV.decodeWith fmtOpts CSV.NoHeader) $
		BS.readFile path
	)

instance CSV.FromRecord Vector where
	parseRecord v =
		fmap Lina.fromList $ CSV.parseRecord v

-- return all pairs in a list
allPairs l =
	case l of
		(x:rest) ->
			map (x,) rest ++ allPairs rest
		_ -> []

uncurry4 f (a,b,c,d) = f a b c d
