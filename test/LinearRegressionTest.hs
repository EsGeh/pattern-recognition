-- {-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified LoadTestData as Load
import Utils( allPairs )
import Types
import qualified TestFunctions as Test

import PatternRecogn.Types

import Control.Monad.Random()
import Data.List( intercalate )


-----------------------------------------------------------------
-- IO stuff:
-----------------------------------------------------------------

main :: IO ()
main =
	handleErrors $
	forM_ (allPairs [3,5,7,8]) $ uncurry test
	where
		handleErrors x =
			do
				valOrErr <- runExceptT x
				either
					(\err -> putStrLn $ "ERROR: " ++ err)
					return
					valOrErr

test :: Label -> Label -> ErrT IO ()
test label1 label2 =
	let
		paths@[path1,path2] = map Load.pathFromLabel labels
		labels = [label1, label2]
	in
		do
			liftIO $ putStrLn $ concat $
				["testing classification of ", intercalate ", " $ map show paths]
			testData <- Load.readTestInputBin path1 path2 label1 label2
			--testData <- Load.readTestInputBin $ paths `zip` labels

			liftIO $ putStrLn $ "testing linear regression classification:"
			Test.testLinearRegression $ testData