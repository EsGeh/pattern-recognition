{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified LoadTestData as Load
import Types
import qualified Plot
import NeuralNetworksTest.TestImpl as Test

import PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.Types


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

defTestParams dimensions =
	Test.TestFunctionParams{
		loggingFreq = 0,
		logProgressFreq = 1,
		learningParams =
			LearningParamsDefault $ defDefaultLearningParams{ learnRate = 0.1 },
		stopConds = [NN.StopIfQualityReached 1, NN.StopIfConverges 0.00001, NN.StopAfterMaxIt 10000],
		networkParams = NN.NetworkParams{
			NN.dims = dimensions,
			NN.outputInterpretation = (NN.outputInterpretationMaximum $ last dimensions)
		}
	}

plotPath descr = ("plots/" ++ descr ++ ".png")

main :: IO ()
main =
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
			labels = [3,5,7,8]
			paths = map Load.pathFromLabel labels
		testAll "digits (from file)"
			(defTestParams [32,10]){
				loggingFreq = 50
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
	plotTests (plotPath dataName) $ [
	( dataName ++ " with momentum",
		replicate 1 $ Test.testNeuronalNetworks
			testParams
			testData 
	)
	, ( dataName ++ " with Silva Almeida",
		replicate 1 $ Test.testNeuronalNetworks
			testParams{
				learningParams = LearningParamsSilvaAlmeida $ defSilvaAlmeidaParams
			}
			testData 
	)
	, ( dataName ++ " with RProp",
		replicate 1 $ Test.testNeuronalNetworks
			testParams{
				learningParams = LearningParamsRProp $ defRPropParams
			}
			testData 
	)
	]

plotTests plotToPath tests =
	do
		testResults <- forM tests $ \(descr, subTests) ->
			fmap (descr,) $
			forM subTests $ \test ->
			do
				doLog $ "-------------------------------------------"
				doLog $ "running " ++ descr
				test
		doLog $ "plotting results to " ++ plotToPath ++ "..."
		Plot.plotProgresses plotToPath testResults
