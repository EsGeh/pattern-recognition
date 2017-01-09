{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified LoadTestData as Load
import Types
import NeuralNetworksTest.TestImpl as Test

--import qualified PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.Types

--import Data.List( intercalate )


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

defTestParams dimensions =
	Test.TestFunctionParams{
		loggingFreq = 0,
		learningParams =
			LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 },
		stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.00001],
		networkParams = NN.NetworkParams{
			NN.dims = dimensions,
			NN.outputInterpretation = (NN.outputInterpretationMaximum $ last dimensions)
		}
	}

main :: IO ()
main =
	handleErrors $
	do

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"and\"... with momentum"
		Test.testNeuronalNetworks
			(defTestParams [2])
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"and\"... using silva almeida optimisation"
		Test.testNeuronalNetworks
			(defTestParams [2]){
				learningParams = LearningParamsSilvaAlmeida $ defSilvaAlmeidaParams
			}
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"and\"... using RProp optimisation"
		Test.testNeuronalNetworks
			(defTestParams [2]){
				learningParams =
					LearningParamsRProp $ defRPropParams
			}
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"or\"... with momentum"
		Test.testNeuronalNetworks
			(defTestParams [2])
			(logicalOp_testInput (||))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"or\"... using silva almeida optimisation"
		Test.testNeuronalNetworks
			(defTestParams [2]){
				learningParams = LearningParamsSilvaAlmeida $ defSilvaAlmeidaParams
			}
			(logicalOp_testInput (||))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"or\"... using RProp optimisation"
		Test.testNeuronalNetworks
			(defTestParams [2]){
				learningParams =
					LearningParamsRProp $ defRPropParams
			}
			(logicalOp_testInput (||))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"xor\"... with momentum"
		Test.testNeuronalNetworks
			(defTestParams [2,1]){
				networkParams = NN.NetworkParams{
					NN.dims = [2,1],
					NN.outputInterpretation = NN.outputInterpretationSingleOutput
				},
				loggingFreq = 1000
			}
			(logicalOp_testInput (\x y -> x && not y || y && not x))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"xor\"... using silva almeida optimisation"
		Test.testNeuronalNetworks
			(defTestParams [2,1]){
				learningParams = LearningParamsSilvaAlmeida $ defSilvaAlmeidaParams,
				networkParams = NN.NetworkParams{
					NN.dims = [2,1],
					NN.outputInterpretation = NN.outputInterpretationSingleOutput
				},
				loggingFreq = 1000
			}
			(logicalOp_testInput (\x y -> x && not y || y && not x))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"xor\"... using RProp optimisation"
		Test.testNeuronalNetworks
			(defTestParams [2,1]){
				learningParams = LearningParamsRProp $ defRPropParams,
				networkParams = NN.NetworkParams{
					NN.dims = [2,1],
					NN.outputInterpretation = NN.outputInterpretationSingleOutput
				},
				loggingFreq = 1000
			}
			(logicalOp_testInput (\x y -> x && not y || y && not x))

		let
			labels = [3,5,7,8]
			paths = map Load.pathFromLabel labels

{-
		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			(defTestParams [10]){
				learningParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 1 },
				stopConds = [NN.StopIfConverges 0.001, NN.StopIfQualityReached 1],
				loggingFreq = 100
			}
			=<< (fromBundledTestData <$> Load.readTestInput (paths `zip` labels))
-}

		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			(defTestParams [10,10]){
				learningParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 1 },
				stopConds = [NN.StopIfConverges 0.001, NN.StopIfQualityReached 1],
				loggingFreq = 100
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

{-
startToClassifyInfoStr l =
	concat $
			[ "----------------------------------------------\n"
			, "classifying to labels ", intercalate "," $ map (show . snd) l
			, " in files ", intercalate "," $ map (show . fst) l
			]
-}
