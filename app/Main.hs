{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Plot
import Types

import qualified PatternRecogn.LinearRegression as LinearReg
--import PatternRecogn.Gauss.Binary
import qualified PatternRecogn.Gauss.Binary as Gauss
import PatternRecogn.Types

import PatternRecogn.Lina as Lina
{-
import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
-}

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char
import Data.List( intercalate )
import Control.Monad.Random as Rand

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

data AlgorithmInput =
	AlgorithmInput {
		algInput_train1 :: Matrix,
		algInput_train2 :: Matrix,
		algInput_input :: Matrix
	}
	deriving( Show )

testWithData :: FilePath -> FilePath -> Label -> Label -> ErrT IO ()
testWithData
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
			readTestInput
				trainingFile1 trainingFile2
				label1 label2
			:: ErrT IO (AlgorithmInput, Vector)
		testGauss label1 label2 testInput >>= \quality ->
			liftIO $ putStrLn $ concat $ ["gauss quality:", show $ quality]
		testProjectedGauss label1 label2 testInput >>= \quality ->
			liftIO $ putStrLn $ concat $ ["projected gauss quality:", show $ quality]
		testLinearRegression label1 label2 testInput >>= \quality ->
			liftIO $ putStrLn $ concat $ ["linear regression quality:", show $ quality]
		return ()
		{-
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
		-}

testGauss :: Monad m => Label -> Label -> (AlgorithmInput, Vector) -> m Double
testGauss =
	testWithAlg 
		(\train1 train2 -> return $ Gauss.calcClassificationParams train1 train2)
		Gauss.classify

testLinearRegression :: Monad m => Label -> Label -> (AlgorithmInput, Vector) -> m Double
testLinearRegression =
	testWithAlg 
		(\train1 train2 -> return $ LinearReg.calcClassificationParams train1 train2)
		LinearReg.classify

testProjectedGauss :: MonadRandom m => Label -> Label -> (AlgorithmInput, Vector) -> m Double
testProjectedGauss =
	testWithAlg 
		calcParam
		classify
	where
		calcParam train1 train2 =
			do
				let param = Gauss.calcClassificationParams train1 train2
				projectionVec <-
					Gauss.findProjectionWithRnd param
				let projectedClassificationParam =
					Gauss.projectClasses projectionVec param
				return $
					(projectedClassificationParam, projectionVec) 
		classify (label1, label2) (param, projectionVec) input =
			Gauss.classifyProjected (label1,label2)
				projectionVec param
				input

testWithAlg ::
	Monad m =>
	(Matrix -> Matrix -> m param)
	-> ((Label, Label) -> param -> Matrix -> VectorOf Label)
	-> Label -> Label
	-> (AlgorithmInput, Vector)
	-> m Double
testWithAlg
		calcParam
		classify
		label1 label2
		(AlgorithmInput{..}, inputLabels)
	=
	do
		classificationParam <-
			calcParam algInput_train1 algInput_train2
		let classified =
			classify (label1,label2)
				classificationParam
				algInput_input
		return $
			calcClassificationQuality
				(cmap round $ inputLabels) classified


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

calcClassificationQuality :: VectorOf Label -> VectorOf Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

readTestInput :: FilePath -> FilePath -> Label -> Label -> ErrT IO (AlgorithmInput, Vector)
readTestInput
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
	, Gauss.infoStringForParam param
	, concat $ ["projectionVec size:", show $ size projectionVec]
	, concat $ ["projected clusters: ----------------------------"]
	, infoStringForParam_1D projectedClassificationParam
	, concat $ ["inputData size:", show $ size inputData]
	, concat $ ["result: --------------------"]
	, concat $ ["classification quality:", show $ result]
	]


infoStringForParam_1D :: Gauss.ClassificationParam -> String
infoStringForParam_1D Gauss.ClassificationParam{..} =
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
		fmap Lina.fromList $ CSV.parseRecord v

-- return all pairs in a list
allPairs l =
	case l of
		(x:rest) ->
			map (x,) rest ++ allPairs rest
		_ -> []

uncurry4 f (a,b,c,d) = f a b c d
