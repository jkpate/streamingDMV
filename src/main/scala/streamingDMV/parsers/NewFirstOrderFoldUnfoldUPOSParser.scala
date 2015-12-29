package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.labels.{Event,ParserSpec}
import streamingDMV.parameters.UPOSArcFactoredParameters

import breeze.linalg._
import breeze.numerics._

abstract class NewFirstOrderFoldUnfoldUPOSParser[P<:UPOSArcFactoredParameters](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // uposCount:Int,
  // randomSeed:Int = 15,
  // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends NewFoldUnfoldUPOSParser[P](
  // maxLength, rootAlpha, stopAlpha, chooseAlpha, uposCount, randomSeed,
  // reservoirSize
  parserSpec
) {

  // def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha, logSpace )

  def mCellFactor( i:Int, k:Int, j:Int, decoration:MDecoration ) =
    DenseMatrix.eye[Double](uposCount)
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:DenseMatrix[Double] ) = Seq()

}


