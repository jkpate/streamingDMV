package streamingDMV.parsers

import streamingDMV.labels._
// import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.parameters.ArcFactoredParameters

import breeze.linalg._
import breeze.numerics._

// abstract class FirstOrderFoldUnfoldNOPOSParser[P<:NOPOSArcFactoredParameters](
abstract class FirstOrderFoldUnfoldNOPOSParser[P<:ArcFactoredParameters[DMVCounts]](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15,
  reservoirSize:Int = 0
) extends FoldUnfoldNOPOSParser[DMVCounts,P](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed, reservoirSize
) {

  def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha, true )

  def mCellFactor( i:Int, k:Int, j:Int, decoration:MDecoration ) = 0D
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:Double ) = Seq()

}


