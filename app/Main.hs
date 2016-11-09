{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Main where

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as Lina

import qualified Data.Csv as CSV
import Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as Vec
import Data.Char
import Foreign.C.Types( CInt )

type Matrix = Lina.Matrix Double
type Vector = Lina.Vector Double
type Label = CInt

type ErrT m a = ExceptT String m a

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
		let beta = calcBeta trainingSet1 trainingSet2
		let classified = classify (label1,label2) beta inputData
		let result =  calcClassificationQuality (cmap round $ inputLabels) classified
		liftIO $ putStrLn $
			descriptionString
				trainingSet1
				trainingSet2
				beta
				inputData
				classified
				result

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

descriptionString set1 set2 beta inputData classified result =
	unlines $
	[ concat $ ["set1 size:", show $ size set1]
	, concat $ ["set2 size:", show $ size set2]
	, concat $ ["beta size:", show $ size beta]
	, concat $ ["inputData size:", show $ size inputData]
	, concat $ ["result: --------------------"]
	, concat $ ["classification quality:", show $ result]
	]

-----------------------------------------------------------------
-- actual code:
-----------------------------------------------------------------

calcBeta :: Matrix -> Matrix -> Vector
calcBeta set1 set2 =
	flatten $ -- matrix to vector
	let
		x =
			prependOnes $
				set1
				===
				set2
		y =
			konst (-1) (count1,1)
			===
			konst 1 (count2,1)
		count1 = rows set1
		count2 = rows set2
		xTr_times_x_SAFE =
			let
				xTr_times_x = tr' x <> x
			in
				if det xTr_times_x > 0.01
				then xTr_times_x
				else
					xTr_times_x + (alpha * ident (rows xTr_times_x))
					where alpha = 0.01
	in
		inv xTr_times_x_SAFE <> tr' x <> y

calcClassificationQuality :: Lina.Vector Label -> Lina.Vector Label -> Double
calcClassificationQuality expected res =
	(/ fromIntegral (size res)) $
	sum $
	map (\x -> if x/=0 then 0 else 1) $
	toList $
	expected - res

classify :: (Label, Label) -> Vector -> Matrix -> Lina.Vector Label
classify (labelNeg, labelPos) beta input =
	cmap (assignLabels . sgn) $
		prependOnes input #> beta
	where
		sgn :: Double -> Double
		sgn x =
			let temp = signum x in
				if temp /= 0 then temp else 1
		assignLabels x
			| x < 0 = labelNeg
			| otherwise = labelPos

prependOnes m =
	konst 1 (rows m,1) ||| m

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
