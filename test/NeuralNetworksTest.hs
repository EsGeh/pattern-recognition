{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified LoadTestData as Load
import Types
import qualified Plot
import NeuralNetworksTest.TestImpl as Test

import PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.Types

import System.IO


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

defLearningParams =
	LearningParams{
		learningP_specificParams = LearningParamsDefault $ defDefaultLearningParams,
		learningP_sampleSize = Nothing
	}

defTestParams dimensions =
	Test.TestFunctionParams{
		loggingFreq = 0,
		logProgressFreq = 1,
		learningParams = defLearningParams{
				learningP_specificParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 }
		},
		stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.00001, NN.StopAfterMaxIt 1000],
		networkParams = NN.NetworkParams{
			NN.dims = dimensions,
			NN.outputInterpretation = (NN.outputInterpretationMaximum $ last dimensions)
		}
	}

plotPath descr = ("plots/" ++ descr ++ ".svg")

main :: IO ()
main =
	(hSetBuffering stdout NoBuffering >>) $
	handleErrors $
	do

		testAll "AND" (defTestParams [2]) (logicalOp_testInput (&&))
		testAll "OR" (defTestParams [2]) (logicalOp_testInput (||))
		testAll "XOR"
			(defTestParams [2,1]){
				networkParams = NN.NetworkParams{
					NN.dims = [2,1],
					NN.outputInterpretation = NN.outputInterpretationSingleOutput
				}}
			(logicalOp_testInput (\x y -> x && not y || y && not x))

		let
			labels = [0..9]
			--labels = [3,5,7,8]
			paths = map Load.pathFromLabel labels
		testAll "digits"
			(defTestParams [32,10]){
				loggingFreq = 50,
				logProgressFreq = 1,
				learningParams = defLearningParams{
					learningP_specificParams = LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 }
				},
				stopConds = [NN.StopIfQualityReached 0.9, NN.StopIfConverges 0.00001, NN.StopAfterMaxIt 10000]
			}
			=<< (fromBundledTestData <$> Load.readTestInput (paths `zip` labels))
		liftIO $ putStrLn $ "done"
		return ()
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

testAll dataName testParams testData = 
	let
		measureFreq = logProgressFreq testParams
	in
		plotTests (plotPath dataName) measureFreq $
		[
		( dataName ++ " with momentum",
			replicate 1 $ Test.testNeuronalNetworks
				testParams
				testData 
		)
		, ( dataName ++ " with Silva Almeida",
			replicate 1 $ Test.testNeuronalNetworks
				testParams{
					learningParams = defLearningParams{
						learningP_specificParams = LearningParamsSilvaAlmeida $ defSilvaAlmeidaParams
					}
				}
				testData 
		)
		, ( dataName ++ " with RProp",
			replicate 1 $ Test.testNeuronalNetworks
				testParams{
					learningParams = defLearningParams{
						learningP_specificParams = LearningParamsRProp $ defRPropParams
					}
				}
				testData 
		)
		]

plotTests :: String -> Int -> [(String, [ErrT IO [R]])] -> ErrT IO ()
plotTests plotToPath measureFreq tests =
	do
		testResults <- forM tests $ \(descr, subTests) ->
			fmap (descr,) $
			forM subTests $ \test ->
			do
				doLog $ "-------------------------------------------"
				doLog $ "running " ++ descr
				test
		doLog $ "plotting results to " ++ plotToPath ++ "..."
		Plot.plotProgresses measureFreq plotToPath testResults
