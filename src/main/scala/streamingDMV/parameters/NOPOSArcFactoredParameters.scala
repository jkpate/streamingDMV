package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

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

  val p_root = new CPT[RootEvent](
    rootAlpha,
    squarelyNormalized,
    approximate = approximate,
    eps = 1E-5,
    delta = 3E-2,
    randomSeed = rand.nextInt
  )
  val p_stop =
    new CPT[StopEvent](
      stopAlpha,
      approximate = approximate,
      eps = 1E-5,
      delta = 3E-2,
      randomSeed = rand.nextInt
    )
  val p_choose =
    new CPT[ChooseEvent](
      chooseAlpha,
      squarelyNormalized,
      approximate = approximate,
      eps = 1E-5,
      delta = 1E-4,
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

  def incrementCounts( counts:DMVCounts ) {
    // println( "increment by\n" )
    // counts.rootCounts.printOut()

    p_root.increment( counts.rootCounts )

    // println( "\n\n-------\n\nRESULT\n" )
    // p_root.printOut()

    p_stop.increment( counts.stopCounts )
    p_choose.increment( counts.chooseCounts )
  }

  def setEvents( counts:DMVCounts ) {
    p_root.setEvents( counts.rootCounts )
    p_stop.setEvents( counts.stopCounts )
    p_choose.setEvents( counts.chooseCounts )
  }

  def setEventsAndCounts( counts:DMVCounts ) {
    p_root.setEventsAndCounts( counts.rootCounts )
    p_stop.setEventsAndCounts( counts.stopCounts )
    p_choose.setEventsAndCounts( counts.chooseCounts )
  }


  def decrementCounts( counts:DMVCounts ) {
    p_root.decrement( counts.rootCounts )
    p_stop.decrement( counts.stopCounts )
    p_choose.decrement( counts.chooseCounts )
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

