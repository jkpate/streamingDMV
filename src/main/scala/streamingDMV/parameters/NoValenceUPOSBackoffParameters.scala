package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables._

import breeze.linalg._
import breeze.numerics._

class NoValenceUPOSBackoffParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  // backoffAlpha:Double,
  // notBackoffAlpha:Double,
  uposCount:Int,
  parameterSpec:ParameterSpec
) extends UPOSArcFactoredParameters( uposCount, parameterSpec ) {

  val backoffAlpha = parameterSpec.backoffAlpha
  val notBackoffAlpha = parameterSpec.notBackoffAlpha

  // p_root provides P_r( rootPos )
  val p_rootLex = new CPT[RootEvent]( rootAlpha )
  // p_choose provides P_c( p_d | p_h, w_h, dir ) and P_c( p_d | p_h, dir )
  // p_chooseLex provides P_c( w_d | p_h, w_h, dir ) and P_c( w_d | p_h, dir )
  val p_chooseLex = new MatrixCPT[ChooseEvent]( chooseAlpha, 1, uposCount )

  val lambda_choose = new BackoffCPT[LambdaChooseEvent](
    Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
  )

  def rootLex( obs:Int ) =
    if( fullyNormalized )
      p_rootLex.normalized( RootEvent( obs ) )
    else
      p_rootLex.expDigammaNormalized( RootEvent( obs ) )

  def chooseLex( event:ChooseEvent ) =
    if( fullyNormalized )
      p_chooseLex.normalized( event )
    else
      p_chooseLex.expDigammaNormalized( event )

  // Let's try never conditioning on lexical information for stop -- incorporate stop backoff later
  // if necessary.
  // val lambda_stop = new BackoffCPT[LambdaStopEvent](
  //   Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
  // )



  def apply( l:LambdaChooseEvent ) = {
    if( fullyNormalized )
      lambda_choose.normalized( l )
    else
      lambda_choose.expDigammaNormalized( l )
  }

  // def apply( l:LambdaStopEvent ) = {
  //   if( fullyNormalized )
  //     lambda_stop.normalized( l )
  //   else
  //     lambda_stop.expDigammaNormalized( l )
  // }


  def zerosMatrix(rows:Int = uposCount, cols:Int = uposCount ) =
    DenseMatrix.zeros[Double]( uposCount, uposCount )

  def zerosInit( corpus:List[Utt] ) {

    p_root.clear
    p_stop.clear
    p_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        lambda_choose.increment( LambdaChooseEvent( h, LeftAtt, Backoff ), 0D )
        lambda_choose.increment( LambdaChooseEvent( h, LeftAtt, NotBackoff ), 0D )
        lambda_choose.increment( LambdaChooseEvent( h, RightAtt, Backoff ), 0D )
        lambda_choose.increment( LambdaChooseEvent( h, RightAtt, NotBackoff ), 0D )

        // rootEvents += RootEvent( h )
        // stopEvents ++= possibleStopEvents( h )
        p_root.increment( RootEvent( h ), DenseMatrix.zeros[Double](uposCount, 1 ) )
        p_stop.increment( possibleStopEvents( h ), DenseMatrix.zeros[Double](1, uposCount ) )

        ( 0 until t ).foreach{ i =>
          // chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), zerosMatrix() )
        }
        ( t+1 until s.length ).foreach{ j =>
          // chooseEvents += ChooseEvent( h, RightAtt, s(j) )
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), zerosMatrix() )
        }
      }
    }

    // p_root.setEvents( rootEvents.toSet )
    // p_stop.setEvents( stopEvents.toSet )
    // p_choose.setEvents( chooseEvents.toSet )
  }

  def harmonicCounts( corpus:List[Utt] ) = {
    println( "\n\n\n\nHARMONIC COUNTS NOT IMPLEMENTED YET FOR NoValenceUPOSBackoffParameters!!!\n\n\n\n" )
    MatrixDMVCounts( uposCount, rootAlpha, stopAlpha, chooseAlpha )
  }

  def possibleStopEvents( h:Int ) = {
    Seq(  // let's try never conditioning on lexical information in stop -- try to learn stopping
          // behavior for categories of words.
      // StopEvent( h, LeftAtt, NoValence, Stop ),
      // StopEvent( h, LeftAtt, NoValence, NotStop ),
      // StopEvent( h, RightAtt, NoValence, Stop ),
      // StopEvent( h, RightAtt, NoValence, NotStop )
      StopEvent( LeftAtt, Stop ),
      StopEvent( LeftAtt, NotStop ),
      StopEvent( RightAtt, Stop ),
      StopEvent( RightAtt, NotStop )
    )
  }

  override def incrementCounts( counts:MatrixDMVCounts, updateEvents:Boolean = true ) {
    p_root.increment( counts.rootCounts )
    p_stop.increment( counts.stopCounts )
    p_choose.increment( counts.chooseCounts )
    if( notBackoffAlpha > 0 ) {
      counts.chooseCounts.counts.foreach{ case (event, count) =>
        val h = event.head
        if( h >= 0 ) {
          val totalCount = sum( count )
          lambda_choose.increment(
            LambdaChooseEvent( h, event.dir, NotBackoff ),
            totalCount
          )
        }
      }
      // counts.stopCounts.counts.foreach{ case (event, count) =>
      //   val h = event.head
      //   if( h >= 0 ) {
      //     val totalCount = sum( count )
      //     lambda_stop.increment(
      //       LambdaStopEvent( h, event.dir, NotBackoff ),
      //       totalCount
      //     )
      //   }
      // }
    }
  }

  override def decrementCounts( counts:MatrixDMVCounts, integerDec:Boolean ) {
    p_root.decrement( counts.rootCounts, integerDec )
    p_stop.decrement( counts.stopCounts, integerDec )
    p_choose.decrement( counts.chooseCounts, integerDec )
    if( notBackoffAlpha > 0 ) {
      counts.chooseCounts.counts.foreach{ case (event, count) =>
        val h = event.head
        val totalCount = sum( count )
        if( h >= 0 ) {
          lambda_choose.decrement(
            LambdaChooseEvent( h, event.dir, NotBackoff ),
            totalCount,
            integerDec
          )
        }
      }
        // counts.stopCounts.counts.foreach{ case (event, count) =>
        //   val h = event.head
        //   if( h >= 0 ) {
        //     val totalCount = sum( count )
        //     lambda_stop.decrement(
        //       LambdaStopEvent( h, event.dir, NotBackoff ),
        //       totalCount
        //     )
        //   }
        // }
    }
  }

  def printOut( logSpace:Boolean = false ) {
    // println( "p_root:" )
    // p_root.printOut( logSpace )
    // println( "p_stop:" )
    // p_stop.printOut( logSpace )
    // println( "p_choose:" )
    // p_choose.printOut( logSpace )
    // println( "lambda_choose:" )
    // lambda_choose.printOut( logSpace )
  }

}


