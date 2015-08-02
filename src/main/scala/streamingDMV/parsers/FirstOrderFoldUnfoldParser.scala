package streamingDMV.parsers

import streamingDMV.parameters.ArcFactoredParameters

abstract class FirstOrderFoldUnfoldParser[P<:ArcFactoredParameters](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FoldUnfoldParser[P](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  def mSplits( i:Int, j:Int ):Iterable[Int] = ( (i+1) to (j-1) by 2 )

}

