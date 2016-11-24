module PatternRecogn.Lina(
	Matrix, Vector,
	MatrixOf, VectorOf,
	module Lina
) where


import Numeric.LinearAlgebra as Lina hiding( Matrix, Vector )
import qualified Numeric.LinearAlgebra as LinaIntern

type Matrix = LinaIntern.Matrix Double
type Vector = LinaIntern.Vector Double

type MatrixOf = LinaIntern.Matrix
type VectorOf = LinaIntern.Vector
