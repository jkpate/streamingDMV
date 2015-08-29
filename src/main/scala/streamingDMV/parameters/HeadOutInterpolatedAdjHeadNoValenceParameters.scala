package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.BackoffCPT

import collection.mutable.{Set=>MSet}

class HeadOutInterpolatedAdjHeadNoValenceParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  backoffAlpha:Double,
  notBackoffAlpha:Double
) extends NOPOSArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha ) {

  val lambda_choose = new BackoffCPT[LambdaChooseEvent](
    Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
  )

  def apply( l:LambdaChooseEvent ) = {
    if( fullyNormalized )
      lambda_choose.normalized( l )
    else
      lambda_choose.expDigammaNormalized( l )
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

        p_root.increment( RootEvent( h ), 0D )
        p_stop.increment( possibleStopEvents( h ), 0D )

        ( 0 until t ).foreach{ i =>
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), 0D )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), 0D )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), LeftAtt, Backoff ), 0D )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), LeftAtt, NotBackoff ), 0D )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), 0D )

          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), 0D )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), RightAtt, Backoff ), 0D )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), RightAtt, NotBackoff ), 0D )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

  }

  override def incrementCounts( counts:DMVCounts ) {
    p_root.increment( counts.rootCounts )
    p_stop.increment( counts.stopCounts )
    p_choose.increment( counts.chooseCounts )

    // var backoffEvents = 0D
    // var notBackoffEvents = 0D

    if( notBackoffAlpha > 0 )
      counts.chooseCounts.counts.foreach{ case (event, count) =>
        event match {
          case ChooseEvent( head, context, dir, /*_,*/ dep ) =>
            if( context >= 0 ) {
              lambda_choose.increment(
                LambdaChooseEvent( head, context, dir, NotBackoff ),
                count
              )
            //  notBackoffEvents += count
            } else {
              lambda_choose.increment(
                LambdaChooseEvent( head, context, dir, Backoff ),
                count
              )
            //   backoffEvents += count
            }
          case _ =>
        }
      }

    // println( s"$notBackoffEvents not backoff events" )
    // println( s"$backoffEvents backoff events" )
    // println( s"${notBackoffEvents + backoffEvents} lambda events\n\n" )
  }

  override def decrementCounts( counts:DMVCounts ) {
    p_root.decrement( counts.rootCounts )
    p_stop.decrement( counts.stopCounts )
    p_choose.decrement( counts.chooseCounts )
    if( notBackoffAlpha > 0 )
      counts.chooseCounts.counts.foreach{ case (event, count) =>
        event match {
          case ChooseEvent( head, context, dir, /*_,*/ dep ) =>
            if( context >= 0 ) {
              lambda_choose.decrement(
                LambdaChooseEvent( head, context, dir, NotBackoff ),
                count
              )
            } else {
              lambda_choose.decrement(
                LambdaChooseEvent( head, context, dir, Backoff ),
                count
              )
            }
          case _ =>
        }
      }
  }

  override def setEvents( counts:DMVCounts ) {
    p_root.setEvents( counts.rootCounts )
    p_stop.setEvents( counts.stopCounts )
    p_choose.setEvents( counts.chooseCounts )
    if( notBackoffAlpha > 0 ) {
      lambda_choose.clear
      counts.chooseCounts.counts.keys.foreach{ event =>
        lambda_choose.increment(
          LambdaChooseEvent( event.head, event.context, event.dir, NotBackoff ),
          0D
        )
      }
    }
  }

  override def setEventsAndCounts( counts:DMVCounts ) {
    p_root.setEventsAndCounts( counts.rootCounts )
    p_stop.setEventsAndCounts( counts.stopCounts )
    p_choose.setEventsAndCounts( counts.chooseCounts )
    if( notBackoffAlpha > 0 ) {
      lambda_choose.clear
      counts.chooseCounts.counts.foreach{ case (event, count) =>
        lambda_choose.increment(
          LambdaChooseEvent( event.head, event.context, event.dir, NotBackoff ),
          count
        )
      }
    }
  }


  override def printOut( logSpace:Boolean = false ) {
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

