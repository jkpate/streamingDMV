package streamingDMV.parsers

import streamingDMV.labels._
// import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.parameters.ArcFactoredParameters

import breeze.linalg._
import breeze.numerics._

// abstract class FirstOrderFoldUnfoldNOPOSParser[P<:NOPOSArcFactoredParameters](
abstract class FirstOrderFoldUnfoldNOPOSParser[P<:ArcFactoredParameters[DMVCounts]](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // randomSeed:Int = 15,
  // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends FoldUnfoldNOPOSParser[DMVCounts,P](
  // maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed, reservoirSize
  parserSpec
) {

    // val rootAlpha = parserSpec.rootAlpha
    // val stopAlpha = parserSpec.stopAlpha
    // val chooseAlpha = parserSpec.chooseAlpha

  def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha, logSpace )

  def mCellFactor( i:Int, k:Int, j:Int, decoration:MDecoration ) = myOne
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:Double ) = Seq()

}


