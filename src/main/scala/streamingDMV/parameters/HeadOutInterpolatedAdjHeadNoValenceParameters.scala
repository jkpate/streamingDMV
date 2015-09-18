package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.{LogCPT,LogBackoffCPT}

import collection.mutable.{Set=>MSet}

class HeadOutInterpolatedAdjHeadNoValenceParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  backoffAlpha:Double,
  notBackoffAlpha:Double,
  squarelyNormalized:Int = 0,
  approximate:Boolean = false,
  randomSeed:Int
// ) extends NOPOSArcFactoredParameters(
) extends ArcFactoredParameters[BackoffChooseDMVCounts](
  rootAlpha,
  stopAlpha,
  chooseAlpha/*,
  squarelyNormalized,
  approximate,
  randomSeed*/
) {

  val rand = new util.Random( randomSeed )

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
  val lambda_choose = new LogBackoffCPT[LambdaChooseEvent](
    Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
  )

  def toCounts = {
    // val c =
    BackoffChooseDMVCounts(
        p_root,
        p_stop,
        p_choose,
        lambda_choose
      )

    // // println( c.chooseCounts.denoms.size + " choose LHS in toCounts" )
    // println( "in toCounts " )
    // c.printTotalCountsByType

    // c
  }

  def randomizeCounts( seed:Int, scale:Int ) {
    val r = new util.Random( seed )
    p_root.randomizeCounts( r, scale )
    p_stop.randomizeCounts( r, scale )
    p_choose.randomizeCounts( r, scale )
    lambda_choose.randomizeCounts( r, scale )
  }

  def apply( l:LambdaChooseEvent ) = {
    if( fullyNormalized )
      lambda_choose.normalized( l )
    else
      lambda_choose.expDigammaNormalized( l )
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

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, NoValence, Stop ),
      StopEvent( h, LeftAtt, NoValence, NotStop ),
      StopEvent( h, RightAtt, NoValence, Stop ),
      StopEvent( h, RightAtt, NoValence, NotStop )
    )
  }

  def zerosInit( corpus:List[Utt] ) {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
    p_root.clear
    p_stop.clear
    p_choose.clear
    lambda_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        p_root.increment( RootEvent( h ), Double.NegativeInfinity )
        p_stop.increment( possibleStopEvents( h ), Double.NegativeInfinity )

        ( 0 until t ).foreach{ i =>
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), Double.NegativeInfinity )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), Double.NegativeInfinity )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), LeftAtt, Backoff ), Double.NegativeInfinity )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), LeftAtt, NotBackoff ), Double.NegativeInfinity )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), Double.NegativeInfinity )

          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), Double.NegativeInfinity )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), RightAtt, Backoff ), Double.NegativeInfinity )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), RightAtt, NotBackoff ), Double.NegativeInfinity )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

  }

  def incrementCounts( counts:BackoffChooseDMVCounts, updateEvents:Boolean = true ) {
    p_root.increment( counts.rootCounts, updateEvents )
    p_stop.increment( counts.stopCounts , updateEvents )
    p_choose.increment( counts.chooseCounts , updateEvents )
    lambda_choose.increment( counts.lambdaChooseCounts , updateEvents )


        // var backoffEvents = 0D
        // var notBackoffEvents = 0D
        // if( notBackoffAlpha > 0 )
        //   counts.chooseCounts.denoms.values.flatten.foreach{ event =>
        //     val count = counts.chooseCounts( event )
        //     event match {
        //       case ChooseEvent( head, context, dir, _ ) =>
        //         if( context >= 0 ) {
        //           lambda_choose.increment(
        //             LambdaChooseEvent( head, context, dir, NotBackoff ),
        //             count
        //           )
        //           notBackoffEvents += count
        //         } else {
        //           lambda_choose.increment(
        //             LambdaChooseEvent( head, context, dir, Backoff ),
        //             count
        //           )
        //           backoffEvents += count
        //         }
        //       case _ =>
        //     }
        //   }

        // println( s"$notBackoffEvents not backoff events" )
        // println( s"$backoffEvents backoff events" )
        // println( s"${notBackoffEvents + backoffEvents} lambda events\n\n" )
  }

  def decrementCounts( counts:BackoffChooseDMVCounts ) {
    p_root.decrement( counts.rootCounts )
    p_stop.decrement( counts.stopCounts )
    p_choose.decrement( counts.chooseCounts )
    lambda_choose.decrement( counts.lambdaChooseCounts )

      // if( notBackoffAlpha > 0 )
      //   // counts.chooseCounts.counts.foreach{ case (event, count) =>
      //   counts.chooseCounts.denoms.values.flatten.foreach{ event =>
      //     val count = counts.chooseCounts( event )
      //     event match {
      //       case ChooseEvent( head, context, dir, _ ) =>
      //         if( context >= 0 ) {
      //           lambda_choose.decrement(
      //             LambdaChooseEvent( head, context, dir, NotBackoff ),
      //             count
      //           )
      //         } else {
      //           lambda_choose.decrement(
      //             LambdaChooseEvent( head, context, dir, Backoff ),
      //             count
      //           )
      //         }
      //       case _ =>
      //     }
      //   }
  }

  def setEvents( counts:BackoffChooseDMVCounts ) {
    p_root.setEvents( counts.rootCounts )
    p_stop.setEvents( counts.stopCounts )
    p_choose.setEvents( counts.chooseCounts )
    lambda_choose.setEvents( counts.lambdaChooseCounts )
        // if( notBackoffAlpha > 0 ) {
        //   lambda_choose.clear
        //   counts.chooseCounts.denoms.values.flatten.foreach{ event =>
        //     val count = counts.chooseCounts( event )
        //     event match {
        //       case ChooseEvent( head, context, dir, _ ) =>
        //         if( context >= 0 ) {
        //           lambda_choose.increment(
        //             LambdaChooseEvent( head, context, dir, NotBackoff ),
        //             count
        //           )
        //         } else {
        //           lambda_choose.increment(
        //             LambdaChooseEvent( head, context, dir, Backoff ),
        //             count
        //           )
        //         }
        //       case _ =>
        //     }
        //   }

        //       // counts.chooseCounts.counts.keys.foreach{ event =>
        //       //   lambda_choose.increment(
        //       //     LambdaChooseEvent( event.head, event.context, event.dir, NotBackoff ),
        //       //     Double.NegativeInfinity
        //       //   )
        //       // }
        // }
  }

  def setEventsAndCounts( counts:BackoffChooseDMVCounts ) {
    p_root.setEventsAndCounts( counts.rootCounts )
    p_stop.setEventsAndCounts( counts.stopCounts )
    p_choose.setEventsAndCounts( counts.chooseCounts )
    lambda_choose.setEventsAndCounts( counts.lambdaChooseCounts )

      // if( notBackoffAlpha > 0 ) {
      //   lambda_choose.clear
      //   // counts.chooseCounts.counts.foreach{ case (event, count) =>
      //   counts.chooseCounts.denoms.values.flatten.foreach{ event =>
      //     val count = counts.chooseCounts( event )
      //     event match {
      //       case ChooseEvent( head, context, dir, _ ) =>
      //         if( context >= 0 ) {
      //           lambda_choose.increment(
      //             LambdaChooseEvent( head, context, dir, NotBackoff ),
      //             count
      //           )
      //         } else {
      //           lambda_choose.increment(
      //             LambdaChooseEvent( head, context, dir, Backoff ),
      //             count
      //           )
      //         }
      //       case _ =>
      //     }
      //   }
      //       // counts.chooseCounts.denoms.values.flatten.foreach{ event =>
      //       //   val count = counts.chooseCounts( event )
      //       //   lambda_choose.increment(
      //       //     LambdaChooseEvent( event.head, event.context, event.dir, NotBackoff ),
      //       //     count
      //       //   )
      //       // }
      // }
  }


  def printOut( logSpace:Boolean = false ) {
    println( "p_root:" )
    p_root.printOut( logSpace )
    println( "p_stop:" )
    p_stop.printOut( logSpace )
    println( "p_choose:" )
    p_choose.printOut( logSpace )
    println( "lambda_choose:" )
    lambda_choose.printOut( logSpace )
  }


}

