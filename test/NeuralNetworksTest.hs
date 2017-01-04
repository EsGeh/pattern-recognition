{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified LoadTestData as Load
import Types
import NeuralNetworksTest.TestImpl as Test

import qualified PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.Types

import Data.List( intercalate )


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
				loggingFreq = 1000,
				maxIt = 10000,
				learnRate = 1,
				stopConds = [Test.StopIfQualityReached 1, Test.StopIfConverges 0.0000001],
				networkParams = Test.NetworkParams{
					dims = [2,1],
					outputInterpretation =
						NN.outputInterpretationSingleOutput
						--(NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (\x y -> x && not y || y && not x))

		let
			labels = [3,5,7,8]
			paths = map Load.pathFromLabel labels
		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 100,
				maxIt = 10000,
				learnRate = 1,
				stopConds = [Test.StopIfConverges 0.001, Test.StopIfQualityReached 1],
				networkParams = Test.NetworkParams{
					dims = [10],
					outputInterpretation = (NN.outputInterpretationMaximum 10)
				}
			}
			=<< (fromBundledTestData <$> Load.readTestInput (paths `zip` labels))

		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 100,
				maxIt = 10000,
				learnRate = 1,
				stopConds = [Test.StopIfConverges 0.001, Test.StopIfQualityReached 1],
				networkParams = Test.NetworkParams{
					dims = [10,10],
					outputInterpretation = (NN.outputInterpretationMaximum 10)
				}
			}
			=<< (fromBundledTestData <$> Load.readTestInput (paths `zip` labels))

	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

startToClassifyInfoStr l =
	concat $
			[ "----------------------------------------------\n"
			, "classifying to labels ", intercalate "," $ map (show . snd) l
			, " in files ", intercalate "," $ map (show . fst) l
			]

-- (helpers: )
-----------------------------------------------------------------
