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

main :: IO ()
main =
	handleErrors $
	do

{-
		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"and\"... with momentum"
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				learningParams =
					LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 },
				stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.001],
				networkParams = NN.NetworkParams{
					NN.dims = [2],
					NN.outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"and\"... using silva almeida optimisation"
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				learningParams =
					LearningParamsSilvaAlmeida $ defSilvaAlmeidaParams,
				stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.001],
				networkParams = NN.NetworkParams{
					NN.dims = [2],
					NN.outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "learning operator \"and\"... using RProp optimisation"
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				learningParams =
					LearningParamsRProp $ defRPropParams,
				stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.001],
				networkParams = NN.NetworkParams{
					NN.dims = [2],
					NN.outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (&&))

		doLog $ "-------------------------------------------"
		doLog $ "testing with operator \"or\"..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 0,
				learningParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 },
				stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.001],
				networkParams = NN.NetworkParams{
					NN.dims = [2],
					NN.outputInterpretation = (NN.outputInterpretationMaximum 2)
				}
			}
			(logicalOp_testInput (||))

		doLog $ "-------------------------------------------"
		doLog $ "testing with operator \"xor\"..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 1000,
				learningParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 1 },
				stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.0000001],
				networkParams = NN.NetworkParams{
					NN.dims = [2,1],
					NN.outputInterpretation =
						NN.outputInterpretationSingleOutput
				}
			}
			(logicalOp_testInput (\x y -> x && not y || y && not x))
-}

		let
			labels = [3,5,7,8]
			paths = map Load.pathFromLabel labels
		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 50,
				learningParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 },
				stopConds = [NN.StopIfConverges 0.00001, NN.StopIfQualityReached 1],
				networkParams = NN.NetworkParams{
					NN.dims = [32,10],
					NN.outputInterpretation = (NN.outputInterpretationMaximum 10)
				}
			}
			=<< (fromBundledTestData <$> Load.readTestInput (paths `zip` labels))

{-
		doLog $ "-------------------------------------------"
		doLog $ "testing to classify test data (from file)..."
		Test.testNeuronalNetworks
			Test.TestFunctionParams{
				loggingFreq = 100,
				learningParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 1 },
				stopConds = [NN.StopIfConverges 0.001, NN.StopIfQualityReached 1],
				networkParams = NN.NetworkParams{
					NN.dims = [10,10],
					NN.outputInterpretation = (NN.outputInterpretationMaximum 10)
				}
			}
			=<< (fromBundledTestData <$> Load.readTestInput (paths `zip` labels))
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
startToClassifyInfoStr l =
	concat $
			[ "----------------------------------------------\n"
			, "classifying to labels ", intercalate "," $ map (show . snd) l
			, " in files ", intercalate "," $ map (show . fst) l
			]
-}
