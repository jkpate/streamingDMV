package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.LogCPT

import collection.mutable.{Set=>MSet}
import math.{exp,log}

// abstract class NOPOSArcFactoredParameters(
abstract class NOPOSArcFactoredParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  squarelyNormalized:Int = 0,
  approximate:Boolean = false,
  randomSeed:Int
) extends ArcFactoredParameters[DMVCounts]( rootAlpha, stopAlpha, chooseAlpha ) {

  val rand = new util.Random( randomSeed )

  // if( approximate )
  //   println( "creating approximate parameters" )
  // else
  //   println( "creating exact parameters" )

  val p_root = new LogCPT[RootEvent](
    rootAlpha,
    squarelyNormalized,
    approximate = approximate,
    eps = 1E-6,
    delta = 3E-2,
    randomSeed = rand.nextInt
  )
  val p_stop =
    new LogCPT[StopEvent](
      stopAlpha,
      approximate = approximate,
      eps = 1E-6,
      delta = 3E-2,
      randomSeed = rand.nextInt
    )
  val p_choose =
    new LogCPT[ChooseEvent](
      chooseAlpha,
      squarelyNormalized,
      approximate = approximate,
      eps = 1E-7,
      delta = 1E-2,
      randomSeed = rand.nextInt
    )

  def toCounts = {
    // val c =
    DMVCounts(
        p_root,
        p_stop,
        p_choose
      )

    // // println( c.chooseCounts.denoms.size + " choose LHS in toCounts" )
    // println( "in toCounts " )
    // c.printTotalCountsByType

    // c
  }


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

  def incrementCounts( counts:DMVCounts, updateEvents:Boolean = true ) {

    p_root.increment( counts.rootCounts, updateEvents )

    p_stop.increment( counts.stopCounts, updateEvents )
    p_choose.increment( counts.chooseCounts, updateEvents  )
  }

  /*
  def setEvents( counts:DMVCounts ) {
    p_root.setEvents( counts.rootCounts )
    p_stop.setEvents( counts.stopCounts )
    p_choose.setEvents( counts.chooseCounts )
  }
  */

  def setEventsAndCounts( counts:DMVCounts ) {
    // println( s"  setting root counts; ${p_root.denoms.values.map{_.size}.sum} rules" )
    p_root.setEventsAndCounts( counts.rootCounts )
    // println( s"  setting stop counts; ${p_stop.denoms.values.map{_.size}.sum} rules" )
    p_stop.setEventsAndCounts( counts.stopCounts )
    // println( s"  setting choose counts; ${p_choose.denoms.values.map{_.size}.sum} rules" )
    p_choose.setEventsAndCounts( counts.chooseCounts )
  }


  def decrementCounts( counts:DMVCounts ) {
    p_root.decrement( counts.rootCounts )
    p_stop.decrement( counts.stopCounts )
    p_choose.decrement( counts.chooseCounts )
  }

  override def printTotalCountsByType {
    println( s"  > ${p_root.counts.values.map{exp(_)}.sum} root events" )
    println( s"  > ${p_root.denomCounts.values.map{exp(_)}.sum} root denom events" )
    println( s"  > ${p_stop.counts.values.map{exp(_)}.sum} stop events" )
    println( s"  > ${p_stop.denomCounts.values.map{exp(_)}.sum} stop denom events" )
    println( s"  > ${p_choose.counts.values.map{exp(_)}.sum} choose events" )
    println( s"  > ${p_choose.denomCounts.values.map{exp(_)}.sum} choose denom events" )
    println( s"  > ${p_choose.denoms.size} choose LHS" )
  }

  override def logSpace = {
    p_root.counts.logSpace && p_stop.counts.logSpace && p_choose.counts.logSpace
  }

  def printOut( logSpace:Boolean = false ) {
    println( "p_root:" )
    p_root.printOut( logSpace )
    println( "p_stop:" )
    p_stop.printOut( logSpace )
    println( "p_choose:" )
    p_choose.printOut( logSpace )
  }

}

