package streamingDMV.parsers

import streamingDMV.labels.Event
import streamingDMV.parameters.UPOSArcFactoredParameters

import breeze.linalg._
import breeze.numerics._

abstract class FirstOrderFoldUnfoldUPOSParser[P<:UPOSArcFactoredParameters](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  uposCount:Int,
  randomSeed:Int = 15,
  reservoirSize:Int = 0
) extends FoldUnfoldUPOSParser[P](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, uposCount, randomSeed,
  reservoirSize
) {

  def mSplits( i:Int, j:Int ):Iterable[Int] = ( (i+1) to (j-1) by 2 )
  def outsideMWithMarginals( i:Int, k:Int, j:Int ):Seq[Tuple2[Event,DenseMatrix[Double]]] = {
    outsideM( i, k, j )
    Seq()
  }

}

