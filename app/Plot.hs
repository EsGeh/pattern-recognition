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


plotProjected :: FilePath -> Vector -> Matrix -> Classes -> ErrT IO ()
plotProjected path projectionVec dots params =
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
				Chart.plot $ vectorField "projectionVector" $
					[ ((0,0), vecToTuple projectionVec) ]
			{-
				Chart.plot $
					line "classes" $
						map (
							\Class{..} ->
								map vecToTuple $
								lineFromGauss class_min class_cov
						)
						params
			-}

vectorField :: String -> [((R,R),(R,R))] -> EC (Layout R R) (Plot R R)
vectorField title vectors =
	fmap plotVectorField $ liftEC $
	do
		c <- takeColor
		plot_vectors_values .= vectors
		plot_vectors_style . vector_line_style . line_color .= c
		plot_vectors_style . vector_head_style . point_color .= c
		plot_vectors_title .= title
		----plot_vectors_grid .= grid

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
