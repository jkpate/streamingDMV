package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.NoValenceParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class NoValenceParser(
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // randomSeed:Int = 15,
  // squarelyNormalized:Int = 0,
  // val approximate:Boolean = false,
  // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends FirstOrderFoldUnfoldNOPOSParser[NoValenceParameters](
  // maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed, reservoirSize
  parserSpec
) {

  // println( s"Creating NoValenceParser with random seed $randomSeed"  )

      // val maxLength = parserSpec.length
      // val randomSeed = parserSpec.randomSeed
      // val rootAlpha = parserSpec.rootAlpha
      // val stopAlpha = parserSpec.stopAlpha
      // val chooseAlpha = parserSpec.chooseAlpha
      // val approximate = parserSpec.approximate
      // val squarelyNormalized = parserSpec.squarelyNormalized

  val theta = new NoValenceParameters(
    // rootAlpha,
    // stopAlpha,
    // chooseAlpha,
    // squarelyNormalized,
    // approximate,
    // randomSeed
    parserSpec.toParameterSpec
  )




  def cellMap( i:Int, j:Int ) = {
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> myZero )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> myZero )
    } else {
      MMap[Decoration,Double]()
    }
  }


  def findLeftRootChild( k:Int, rDec:Decoration ) = headTrace( 0 )( k )( NoValence )
  def findRightRootChild( k:Int, rDec:Decoration ) =
    headTrace( k )( intString.length )( NoValence )

  def findLeftLeftwardChild( i:Int, k:Int, mDV:Decoration ) = headTrace( i )( k )( NoValence )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( k )( j )( PlainM )
  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( i )( k )( PlainM )
  def findRightRightwardChild( k:Int, j:Int, mDV:Decoration ) = headTrace( k )( j )( NoValence )
  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) = headTrace( i )( k )( NoValence )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) = headTrace( k )( j )( NoValence )

  def lexSpecs( index:Int ) = Seq( NoValence )
  def lexCellFactor( index:Int, pDec:Decoration ) = myOne

  // def lexMarginals( index:Int ) = Seq()
  def lexEventCounts( index:Int, pDec:Decoration, marginal:Double ) = Seq()





  // NEW DEFINITIONS
  def rightwardSplitSpecs( i:Int, j:Int ) = {
    Seq(
      (
        NoValence,
        ( (i+2) to (j-1) by 2 ).map{ k =>
          (
            k,
            PlainM,
            NoValence
          )
        }
      )
    )
  }
  def leftwardSplitSpecs( i:Int, j:Int ) = {
    Seq(
      (
        NoValence,
        ( (i+1) to (j-2) by 2 ).map{ k =>
          (
            k,
            PlainM,
            NoValence
          )
        }
      )
    )
  }

  def mSplitSpecs( i:Int, j:Int ) = {
    Seq( ( PlainM, ( (i+1) to (j-1) by 2 ) ) )
  }

  def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).map{ k => ( k, DecorationPair(NoValence, NoValence) ) }
  }


  // val rightwardCache = MMap[Tuple5[Int, Int, Decoration, MDecoration, Decoration],Double]()
  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    val dep = intString( k )

    // if( caching )
    //   leftwardCache.getOrElseUpdate(
    //     ( head, dep, pDec, mDec, cDec ),
    //     { theta( ChooseEvent( head, RightAtt, dep ) ) +
    //       theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) +
    //         theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) +
    //         theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) }
    //   )
    // else
    myTimes(
      theta( ChooseEvent( head, RightAtt, dep ) ),
        theta( StopEvent( head, RightAtt, NoValence, NotStop ) ),
          theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ),
          theta( StopEvent( dep, RightAtt, NoValence, Stop ) )
    )
  }

  // val leftwardCache = MMap[Tuple5[Int, Int, Decoration, MDecoration, Decoration],Double]()
  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    // println( "  " +insideChart.length )
    val head = intString( j )
    val dep = intString( k )

        // if( caching )
        //   leftwardCache.getOrElseUpdate(
        //     ( head, dep, pDec, mDec, cDec ),
        //     { theta( ChooseEvent( head, LeftAtt, dep ) ) +
        //       theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) +
        //         theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) +
        //         theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) }
        //   )
        // else
    myTimes(
      theta( ChooseEvent( head, LeftAtt, dep ) ),
        theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ),
          theta( StopEvent( dep, RightAtt, NoValence, Stop ) ),
          theta( StopEvent( dep, LeftAtt, NoValence, Stop ) )
    )
  }


  def rootCellFactor( k:Int, rDec:DecorationPair ) = {
    val r = intString( k )

    myTimes(
      theta( RootEvent( r ) ),
        theta( StopEvent( r, LeftAtt, NoValence, Stop ) ),
        theta( StopEvent( r, RightAtt, NoValence, Stop ) )
    )
  }


  def rootEventCounts( k:Int, rDec:Decoration, marginal:Double ) = {
    val r = intString( k )
    Seq(
      ( RootEvent( r ), marginal ),
      ( StopEvent( r, LeftAtt, NoValence, Stop ), marginal ),
      ( StopEvent( r, RightAtt, NoValence, Stop ), marginal )
    )
  }

  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( i )
    val dep = intString( k )
    Seq(
      ( ChooseEvent( head, RightAtt, dep ), marginal ),
      ( StopEvent( head, RightAtt, NoValence, NotStop ), marginal ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ), marginal ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ), marginal )
    )
  }

  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( j )
    val dep = intString( k )
    Seq(
      ( ChooseEvent( head, LeftAtt, dep ), marginal ),
      ( StopEvent( head, LeftAtt, NoValence, NotStop ), marginal ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ), marginal ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ), marginal )
    )
  }

  def trueLogProb( counts:DMVCounts ) = {
    // myTimes(
    //   theta.p_root.trueLogProb( counts.rootCounts ),
    //   theta.p_stop.trueLogProb( counts.stopCounts ),
    //   theta.p_choose.trueLogProb( counts.chooseCounts )
    // )
    val rootLogProb = theta.p_root.trueLogProb( counts.rootCounts )
    // println( rootLogProb )
    if( !(
        ( rootLogProb > Double.NegativeInfinity ) &
        ( rootLogProb <= 0.000001 )
      )
    ) {
      counts.rootCounts.printOut()
      println( rootLogProb )
      counts.printTotalCountsByType
    }
    assert( rootLogProb > Double.NegativeInfinity )
    assert( rootLogProb <= 0.000001 )

    val chooseLogProb = theta.p_choose.trueLogProb( counts.chooseCounts )
    // println( chooseLogProb )
    if( !(
        ( chooseLogProb > Double.NegativeInfinity ) &
        ( chooseLogProb <= 0.000001 )
      )
    ) {
      counts.chooseCounts.printOut()
      println( chooseLogProb )
      counts.printTotalCountsByType
    }
    assert( chooseLogProb > Double.NegativeInfinity )
    assert( chooseLogProb <= 0.000001 )

    val stopLogProb = theta.p_stop.trueLogProb( counts.stopCounts )
    // println( stopLogProb )
    if( !(
        ( stopLogProb > Double.NegativeInfinity ) &
        ( stopLogProb <= 0.000001 )
      )
    ) {
      counts.stopCounts.printOut()
      println( stopLogProb )
      counts.printTotalCountsByType
    }
    assert( stopLogProb > Double.NegativeInfinity )
    assert( stopLogProb <= 0.000001 )

    // Always in log-space -- don't use myTimes
    rootLogProb +
    stopLogProb +
    chooseLogProb
  }

}

