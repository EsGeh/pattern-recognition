{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LoadTestData where

import Types
import PatternRecogn.Types
import PatternRecogn.Lina as Lina

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char

trainingDataFormat =
	CSV.defaultDecodeOptions

inputDataFormat =
	CSV.defaultDecodeOptions{
		CSV.decDelimiter = fromIntegral (ord ' ')
	}

pathFromLabel :: Label -> FilePath
pathFromLabel =
	("resource/train." ++) . show

-----------------------------------------------------------------

readTestInput :: [(FilePath,Label)] -> ErrT IO TestDataBundled
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
			loadMatrixFromFile inputDataFormat "resource/zip.test"
		let expectedLabels =
			(cmap truncate) expectedLabels_raw
		return $
			TestData {
				testData_train = (trainingSets `zip` labels),
				testData_input = Just (inputData, expectedLabels)
			}

readTestInputBin :: FilePath -> FilePath -> Label -> Label -> ErrT IO TestDataBin
readTestInputBin
		trainingFile1 trainingFile2
		label1 label2
	=
		do
			ret <- readTestInput [(trainingFile1,label1), (trainingFile2, label2)]
			maybe (throwError "internal error") return $
				testData_mapToTrainDataM toTrainingDataBin ret

{-
readTestInputBin :: FilePath -> FilePath -> Label -> Label -> ErrT IO TestDataBin
readTestInputBin
		trainingFile1 trainingFile2
		label1 label2
	=
	do
		[trainingSet1, trainingSet2] <-
			mapM (loadMatrixFromFile trainingDataFormat) $
			[trainingFile1, trainingFile2]
		(expectedLabels_raw, inputData) <-
			prepareInputData (`elem` [fromIntegral label1, fromIntegral label2]) <$>
			loadMatrixFromFile inputDataFormat "resource/zip.test"
		let expectedLabels = (cmap truncate) expectedLabels_raw
		let Just trainingBin = toTrainingDataBin [(trainingSet1, label1), (trainingSet2, label2)]
		return $
			TestData {
				testData_train = trainingBin,
				testData_input = Just (inputData, expectedLabels)
			}
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
