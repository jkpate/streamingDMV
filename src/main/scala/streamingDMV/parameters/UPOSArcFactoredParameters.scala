package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.MatrixCPT

import collection.mutable.{Set=>MSet}

import breeze.linalg._
import breeze.numerics._

abstract class UPOSArcFactoredParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  uposCount:Int,
  parameterSpec:ParameterSpec
// ) extends ArcFactoredParameters[MatrixDMVCounts]( rootAlpha, stopAlpha, chooseAlpha ) {
) extends ArcFactoredParameters[MatrixDMVCounts]( parameterSpec ) {

  val p_root =
    new MatrixCPT[RootEvent]( DenseMatrix.tabulate(uposCount,1){ (r,_) => rootAlpha / (1+r) }, uposCount, 1 )
  val p_stop =
    new MatrixCPT[StopEvent]( DenseMatrix.ones(1,uposCount), 1, uposCount )
  val p_choose =
    new MatrixCPT[ChooseEvent](
      DenseMatrix.tabulate(uposCount,uposCount){ (r,c) => chooseAlpha / (1+r*c) },
      uposCount,
      uposCount
    )

  def cacheLGammas {
    throw new UnsupportedOperationException( "particle filter for UPOS not implemented!" )
  }

  def toCounts =
    MatrixDMVCounts(
      p_root,
      p_stop,
      p_choose
    )


  def apply( r:RootEvent ) = {
    if( fullyNormalized )
      p_root.normalized( r )
    else
      p_root.expDigammaNormalized( r )
  }

  def apply( s:StopEvent ) = {
    if( fullyNormalized )
      p_stop.normalized( s )
    else
      p_stop.expDigammaNormalized( s )
  }

  def apply( c:ChooseEvent ) = {
    if( fullyNormalized )
      p_choose.normalized( c )
    else
      p_choose.expDigammaNormalized( c )
  }

  def possibleStopEvents( h:Int ):Seq[StopEvent]
  def zerosInit( corpus:List[Utt] ):Unit

  def randomizeCounts( seed:Int, scale:Int ) {
    val r = new util.Random( seed )
    p_root.randomizeCounts( r, scale )
    p_stop.randomizeCounts( r, scale )
    p_choose.randomizeCounts( r, scale )
  }

  def incrementCounts( counts:MatrixDMVCounts, updateEvents:Boolean = true ) {
    p_root.increment( counts.rootCounts )
    p_stop.increment( counts.stopCounts )
    p_choose.increment( counts.chooseCounts )
  }

  def setEvents( counts:MatrixDMVCounts ) {
    p_root.setEvents( counts.rootCounts )
    p_stop.setEvents( counts.stopCounts )
    p_choose.setEvents( counts.chooseCounts )
  }

  def setEventsAndCounts( counts:MatrixDMVCounts ) {
    p_root.setEventsAndCounts( counts.rootCounts )
    p_stop.setEventsAndCounts( counts.stopCounts )
    p_choose.setEventsAndCounts( counts.chooseCounts )
  }


  def decrementCounts( counts:MatrixDMVCounts, integerDec:Boolean ) {
    p_root.decrement( counts.rootCounts, integerDec )
    p_stop.decrement( counts.stopCounts, integerDec )
    p_choose.decrement( counts.chooseCounts, integerDec )
  }

  // def printOut( logSpace:Boolean = false ) {
  //   println( "p_root:" )
  //   p_root.printOut( logSpace )
  //   println( "p_stop:" )
  //   p_stop.printOut( logSpace )
  //   println( "p_choose:" )
  //   p_choose.printOut( logSpace )
  // }

  override def printTotalCountsByType {
    println( s"  > ${p_root.counts.values.map{sum(_)}.sum} root events" )
    println( s"  > ${p_root.denomCounts.values.map{sum(_)}.sum} root denom events" )
    println( s"  > ${p_stop.counts.values.map{sum(_)}.sum} stop events" )
    println( s"  > ${p_stop.denomCounts.values.map{sum(_)}.sum} stop denom events" )
    println( s"  > ${p_choose.counts.values.map{sum(_)}.sum} choose events" )
    println( s"  > ${p_choose.denomCounts.values.map{sum(_)}.sum} choose denom events" )
    println( s"  > ${p_choose.denoms.size} choose LHS" )
  }

}

