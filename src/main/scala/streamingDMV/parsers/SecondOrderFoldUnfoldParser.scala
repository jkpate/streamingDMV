package streamingDMV.parsers

// import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.parameters.ArcFactoredParameters
import streamingDMV.labels.DependencyCounts

// abstract class SecondOrderFoldUnfoldParser[C<:DependencyCounts,P<:NOPOSArcFactoredParameters](
abstract class SecondOrderFoldUnfoldParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15,
  reservoirSize:Int = 0
) extends FoldUnfoldNOPOSParser[C,P](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed, reservoirSize
) {

  // // Second-order parsers use directed m-nodes that have undirected m-node children
  // def mSplits( i:Int, j:Int ):Seq[Int] = ( (i+1) to (j-1) )

}

