package streamingDMV.parsers

import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.labels.DMVCounts

abstract class SecondOrderFoldUnfoldParser[P<:NOPOSArcFactoredParameters](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FoldUnfoldNOPOSParser[P](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  // Second-order parsers use directed m-nodes that have undirected m-node children
  def mSplits( i:Int, j:Int ):Seq[Int] = ( (i+1) to (j-1) )

}

