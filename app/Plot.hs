{-# LANGUAGE RecordWildCards #-}
module Plot where

import Types
import PatternRecogn.Utils
import PatternRecogn.Lina as Lina
import PatternRecogn.Gauss.Types

import Graphics.Rendering.Chart.Easy as Chart hiding( Matrix, Vector )
--import Graphics.Rendering.Chart as Chart hiding( Matrix )
import Graphics.Rendering.Chart.Backend.Diagrams as Chart hiding( Matrix, Vector )
import Data.Default.Class

import Control.Monad.Trans
import Control.Lens


plot :: FilePath -> Matrix -> Classes -> ErrT IO ()
plot path dots params =
	do
		_ <- lift $ Chart.renderableToFile def path $ Chart.toRenderable diagram
		return ()
	where
		diagram :: EC (Layout Double Double) ()
		diagram =
			do
				layout_title .= path
				Chart.plot $ points "data points" $
					map vecToTuple $
					Lina.toRows dots
				Chart.plot $
					line "classes" $
						map (
							\Class{..} ->
								map vecToTuple $
								lineFromGauss class_min class_cov
						)
						params

lineFromGauss :: Vector -> Matrix -> [Vector]
lineFromGauss min cov =
	toRows $
	(+ asRow min) $ -- shift to center
	(<> cov) $ 			-- multiply with covariance matrix
	circleDots 
	where
		circleDots =
			cmap cos anglesMat
			|||
			cmap sin anglesMat
		anglesMat = 
			((count :: Int)><1) angles
		angles = (/(2*pi)) <$> ([0..(count-1)] :: [Double])
		count :: Num a => a
		count = 100 


{-
vecToTuple = listToTuple . Lina.toList
	where
		listToTuple [x,y] = (x,y)
		listToTuple _ = error "error extracting dots"
-}
