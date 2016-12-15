{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Plot
import Types
import qualified TestMultiple as TestMultiple

import qualified PatternRecogn.NeuronalNetworks as NN
import PatternRecogn.Types

import PatternRecogn.Lina as Lina

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char
import Control.Monad.Random as Rand
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
		let
			labels = [3,5,7,8]
			paths = map pathFromLabel labels
		testWithData
			(TestMultiple.testNeuronalNetworks [10])
			(paths `zip` labels)
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

testWithData :: ((TestMultiple.AlgorithmInput,Vector) -> ErrT IO Double) -> [(FilePath, Label)] -> ErrT IO ()
testWithData testFunc l =
	do
		liftIO $ putStrLn $ startToClassifyInfoStr l
		testInput <-
			readTestInput l :: ErrT IO (TestMultiple.AlgorithmInput, Vector)
		testFunc testInput
			>>= \quality -> liftIO $ putStrLn $ concat $ ["quality:", show $ quality]
		return ()

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

readTestInput :: [(FilePath,Label)] -> ErrT IO (TestMultiple.AlgorithmInput, Vector)
readTestInput l
	=
	let
		paths = map fst l
		labels = map snd l
	in
	do
		trainingSets <- mapM (readData trainingDataFormat) paths
		(inputLabels, inputData) <-
			prepareInputData (const True) <$>
			readData inputDataFormat  "resource/zip.test"
		return $ (
			TestMultiple.AlgorithmInput {
				algInput_train = (trainingSets `zip` labels),
				algInput_input = inputData
			}
			,
			inputLabels
			)

{-
readTestInputBinary :: FilePath -> FilePath -> Label -> Label -> ErrT IO (AlgorithmInput, Vector)
readTestInputBinary
		trainingFile1 trainingFile2
		label1 label2
	=
	do
		[trainingSet1, trainingSet2] <-
			mapM (readData trainingDataFormat) $
			[ trainingFile1
			, trainingFile2
			]
		(inputLabels, inputData) <-
			prepareInputData (`elem` [fromIntegral label1, fromIntegral label2]) <$>
			readData inputDataFormat "resource/zip.test"
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

readData :: CSV.DecodeOptions -> FilePath -> ErrT IO Matrix
readData fmtOpts path =
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
