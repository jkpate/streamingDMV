package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.NOPOSArcFactoredParameters

import breeze.linalg._
import breeze.numerics._

abstract class FirstOrderFoldUnfoldNOPOSParser[P<:NOPOSArcFactoredParameters](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FoldUnfoldNOPOSParser[P](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  def mCellFactor( i:Int, k:Int, j:Int, decoration:MDecoration ) = 1D
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:Double ) = Seq()

}


