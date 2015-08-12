package streamingDMV.parsers

import streamingDMV.labels.{Event,DMVCounts}
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

  def mSplits( i:Int, j:Int ):Iterable[Int] = ( (i+1) to (j-1) by 2 )
  def outsideMWithMarginals( i:Int, k:Int, j:Int ):Seq[Tuple2[Event,Double]] = {
    outsideM( i, k, j )
    Seq()
  }

}


