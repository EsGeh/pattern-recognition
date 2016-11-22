{-# LANGUAGE RecordWildCards #-}
module Plot where

import Types
import PatternRecogn.ExpectationMax

import qualified Numeric.LinearAlgebra as Lina
import Numeric.LinearAlgebra hiding( Matrix, Vector )

import Graphics.Rendering.Chart.Easy as Chart hiding( Matrix, Vector )
--import Graphics.Rendering.Chart as Chart hiding( Matrix )
import Graphics.Rendering.Chart.Backend.Diagrams as Chart hiding( Matrix, Vector )
import Data.Default.Class

import Control.Monad.Trans
import Control.Lens


plot :: FilePath -> Matrix -> ClassificationParam -> ErrT IO ()
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
lineFromGauss :: (R,R) -> [(R,R)]
lineFromGauss (x,y) =
	zip
		(map cos angles)
		(map sin angles)
	where
		angles = (/(2*pi)) <$> [0..100]
-}

{-
	where
		diagram =
			Chart.layout_title .~ path $
			Chart.layout_plots .~
				(
					[ toPlot $ pointsPlot]
					++
					classPlot
				)
				$
			def
		pointsPlot =
			plot_points_values .~ points $
			def
			where
				points :: [(Double, Double)]
				points =
					map vecToTuple $
					Lina.toRows dots
		classPlot =
			map (\p -> singleEllipse p `joinPlot` singleCenterDot p)
				params
		singleEllipse Class{..} =
			plotVectorField $
				plot_vectors_values .~
					[(vecToTuple class_min, (1,1))] $
				plot_vectors_scale .~ 0 $ -- no scaling
				plot_vectors_style .~ (vector_head_style .~ (point_shape .~ PointShapeArrowHead 1 $ def) $ def) $
				def
			{-
			toPlot $
			plot_points_style .~ style $
			plot_points_values .~ [vecToTuple class_min]$
			def
			where
				style =
					--point_radius .~ 1 $
					def
			-}
		singleCenterDot Class{..} =
			toPlot $
			plot_points_style .~ style $
			plot_points_values .~ [vecToTuple class_min]$
			def
			where
				style =
					def
-}

{-
vecToTuple = listToTuple . Lina.toList
	where
		listToTuple [x,y] = (x,y)
		listToTuple _ = error "error extracting dots"
-}
