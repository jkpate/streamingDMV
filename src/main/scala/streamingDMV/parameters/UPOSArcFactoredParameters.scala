package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.MatrixCPT

import collection.mutable.{Set=>MSet}

abstract class UPOSArcFactoredParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  uposCount:Int
) extends ArcFactoredParameters[MatrixDMVCounts]( rootAlpha, stopAlpha, chooseAlpha ) {

  val p_root = new MatrixCPT[RootEvent]( rootAlpha, uposCount, 1 )
  val p_stop = new MatrixCPT[StopEvent]( stopAlpha, 1, uposCount )
  val p_choose = new MatrixCPT[ChooseEvent]( chooseAlpha, uposCount, uposCount )

  var fullyNormalized:Boolean = false

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

  def incrementCounts( counts:MatrixDMVCounts ) {
    p_root.increment( counts.rootCounts )
    p_stop.increment( counts.stopCounts )
    p_choose.increment( counts.chooseCounts )
  }

  def decrementCounts( counts:MatrixDMVCounts ) {
    p_root.decrement( counts.rootCounts )
    p_stop.decrement( counts.stopCounts )
    p_choose.decrement( counts.chooseCounts )
  }

  // def printOut( logSpace:Boolean = false ) {
  //   println( "p_root:" )
  //   p_root.printOut( logSpace )
  //   println( "p_stop:" )
  //   p_stop.printOut( logSpace )
  //   println( "p_choose:" )
  //   p_choose.printOut( logSpace )
  // }

}

