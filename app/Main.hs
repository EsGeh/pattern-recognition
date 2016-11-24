{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Plot
import Types

import PatternRecogn.Gauss.Binary
import PatternRecogn.Types

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char
import Data.List( intercalate )
import Control.Monad.Random( evalRandIO )

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
		mapM_
			(uncurry4 testWithData . uncurry testParamsFromLabels) $
			allPairs [3,5,7,8]
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

testWithData :: FilePath -> FilePath -> Label -> Label -> ErrT IO ()
testWithData trainingFile1 trainingFile2 label1 label2 =
	do
		liftIO $ putStrLn $ concat $
			[ "----------------------------------------------\n"
			, "classifying to labels ", show [label1, label2]
			, " in files ", show [trainingFile1, trainingFile2]
			]
		[trainingSet1, trainingSet2] <-
			mapM (readData trainingDataFormat) $
			[ trainingFile1
			, trainingFile2
			]
		(inputLabels, inputData) <-
			prepareInputData (`elem` [fromIntegral label1, fromIntegral label2]) <$>
			readData inputDataFormat "resource/zip.test"
		let classificationParam = calcClassificationParams trainingSet1 trainingSet2
		projectionVec <- liftIO $ evalRandIO $
			findProjectionWithRnd classificationParam
		let
			projectedClassificationParam = projectClasses projectionVec classificationParam
		let classified =
			classifyProjected (label1,label2)
				projectionVec projectedClassificationParam
				inputData
		let result =  calcClassificationQuality (cmap round $ inputLabels) classified
		let trainingProjected =
			(#> projectionVec) $
			(trainingSet1 === trainingSet2)
		liftIO $ putStrLn $
			descriptionString
				trainingSet1
				trainingSet2
				classificationParam
				projectionVec
				projectedClassificationParam
				inputData
				classified
				result
		liftIO $ putStrLn $ "plotting ..."
		plotProjected
			(concat $ ["plots/", show label1, show label2, ".svg"])
			trainingProjected
			(classesFromBinary projectedClassificationParam)

readData :: CSV.DecodeOptions -> FilePath -> ErrT IO Matrix
readData fmtOpts path =
	(fromRows . Vec.toList) <$>
	(
		ExceptT $
		fmap (CSV.decodeWith fmtOpts CSV.NoHeader) $
		BS.readFile path
	)

-- (helpers: )
-----------------------------------------------------------------

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

testParamsFromLabels x y =
	let
		[filePath1, filePath2] = map (("resource/train." ++) . show) [x,y]
	in
		(filePath1, filePath2, x, y)

descriptionString
	set1 set2
	param 
	projectionVec projectedClassificationParam
	inputData classified result
	=
	unlines $
	[ concat $ ["set1 size:", show $ size set1]
	, concat $ ["set2 size:", show $ size set2]
	, infoStringForParam param
	, concat $ ["projectionVec size:", show $ size projectionVec]
	, concat $ ["projected clusters: ----------------------------"]
	, infoStringForParam_1D projectedClassificationParam
	, concat $ ["inputData size:", show $ size inputData]
	, concat $ ["result: --------------------"]
	, concat $ ["classification quality:", show $ result]
	]


infoStringForParam_1D :: ClassificationParam -> String
infoStringForParam_1D ClassificationParam{..} =
	intercalate "\n" $
	[ concat $ ["min1:", show $ min1]
	, concat $ ["min2:", show $ min2 ]
	, concat $ ["cov1:", show $ covariance1 ]
	, concat $ ["cov2:", show $ covariance2 ]
	]

-----------------------------------------------------------------
-- utils:
-----------------------------------------------------------------

instance CSV.FromRecord Vector where
	parseRecord v =
		fmap fromList $ CSV.parseRecord v

-- return all pairs in a list
allPairs l =
	case l of
		(x:rest) ->
			map (x,) rest ++ allPairs rest
		_ -> []

uncurry4 f (a,b,c,d) = f a b c d
