package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.TopDownDMVParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class TopDownDMVParser(
    // maxLength:Int,
    // rootAlpha:Double = 1D,
    // stopAlpha:Double = 1D,
    // chooseAlpha:Double = 1D,
    // randomSeed:Int = 15,
    // squarelyNormalized:Int = 0,
    // val approximate:Boolean = false,
    // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends FirstOrderFoldUnfoldNOPOSParser[TopDownDMVParameters](
  // maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed, reservoirSize
  parserSpec
) {

  println( "INITIALIZING TOPDOWNDMVPARSER" )

  val theta = new TopDownDMVParameters(
    // rootAlpha,
    // stopAlpha,
    // chooseAlpha,
    // squarelyNormalized,
    // approximate,
    // randomSeed
    parserSpec.toParameterSpec
  )

      // val insideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
      //   if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      //     MMap( Outermost -> 0D, Inner -> 0D )
      //   else if( i%2 == 1 && j%2 == 1 )
      //     MMap(
      //       DecorationPair(Outermost,Inner) -> 0D,
      //       DecorationPair(Inner,Outermost) -> 0D
      //     )
      //   else
      //     MMap()
      // )

      // val outsideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
      //   if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      //     MMap( Outermost -> 0D, Inner -> 0D )
      //   else if( i%2 == 1 && j%2 == 1 )
      //     MMap(
      //       DecorationPair(Outermost,Inner) -> 0D,
      //       DecorationPair(Inner,Outermost) -> 0D
      //     )
      //   else
      //     MMap()
      // )

  def cellMap( i:Int, j:Int ) = {
    if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      MMap( Outermost -> myZero, Inner -> myZero )
    else if( i%2 == 1 && j%2 == 1 )
      MMap(
        DecorationPair(Outermost,Inner) -> myZero,
        DecorationPair(Inner,Outermost) -> myZero
      )
    else
      MMap()
  }

  def recoverM( mDV:Decoration ) = {
    mDV match {
      case p:DecorationPair => p
    }
  }


  def findLeftRootChild( k:Int ) = headTrace( 0 )( k )( Outermost )
  def findRightRootChild( k:Int ) = headTrace( k )( intString.length )( Outermost )

  def findLeftLeftwardChild( i:Int, k:Int ) = headTrace( i )( k )( Outermost )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) = {
    assert( mDV == Outermost )
    mTrace( k )( j )( DecorationPair( Outermost, Inner ) )
  }
  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) = {
    assert( mDV == Outermost )
    mTrace( i )( k )( DecorationPair( Inner, Outermost ) )
  }
  def findRightRightwardChild( k:Int, j:Int ) = headTrace( k )( j )( ( Outermost ) )
  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) =
    headTrace( i )( k )( decoration.evenLeft )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) =
    headTrace( k )( j )( decoration.evenRight )

      // def lexFill( index:Int ) {
      //   val head = intString( index )
      //   if( index %2 == 0 ) {
      //     insideChart(index)(index+1)( Outermost ) =
      //       theta( StopEvent( head, LeftAtt, Outermost, Stop ) )

      //     if( index > 0 )
      //       insideChart(index)(index+1)( Inner ) =
      //         theta( StopEvent( head, LeftAtt, Inner, Stop ) )
      //   } else {

      //     insideChart(index)(index+1)( Outermost ) =
      //       theta( StopEvent( head, RightAtt, Outermost, Stop ) )

      //     if( index < intString.length-1 )
      //       insideChart(index)(index+1)( Inner ) =
      //         theta( StopEvent( head, RightAtt, Inner, Stop ) )
      //   }
      // }


        // def viterbiLexFill( index:Int ) {
        //   val head = intString( index )
        //   if( index %2 == 0 ) {
        //     insideChart(index)(index+1)( Outermost ) =
        //       theta( StopEvent( head, LeftAtt, Outermost, Stop ) )
        //     headTrace(index)(index+1) += Outermost -> LexEntry( index )

        //     if( index > 0 ) {
        //       insideChart(index)(index+1)( Inner ) =
        //         theta( StopEvent( head, LeftAtt, Inner, Stop ) )
        //       headTrace(index)(index+1) += Inner -> LexEntry( index )
        //     }
        //   } else {
        //     insideChart(index)(index+1)( Outermost ) =
        //       theta( StopEvent( head, RightAtt, Outermost, Stop ) )
        //     headTrace(index)(index+1) += Outermost -> LexEntry( index )

        //     if( index < intString.length-1 ) {
        //       insideChart(index)(index+1)( Inner ) =
        //         theta( StopEvent( head, RightAtt, Inner, Stop ) )
        //       headTrace(index)(index+1) += Inner -> LexEntry( index )
        //     }
        //   }
        // }

  def lexCellFactor( index:Int, pDec:Decoration ) = {
    // println( intString.mkString("{"," ","}") )
    val head = intString( index )
    val score = 
      if( index%2 == 0 ) {
        assert( pDec == Outermost || index > 0 )
        theta( StopEvent( head, LeftAtt, pDec, Stop ) )
      } else {
        assert( pDec == Outermost || index < intString.length-1 )
        theta( StopEvent( head, RightAtt, pDec, Stop ) )
      }
    // if( !( score > Double.NegativeInfinity && score <= 0D ) ) {
      // println( "TopDownDMVParser.lexCellFactor: " + (index,pDec,score) )
    // }
    assert( ( score > myZero && score <= myOne ) )
    score
  }

  def lexSpecs( index:Int ) = {
    if( index%2 == 0 ) {
      if( index > 0 )
        Seq( Inner, Outermost )
      else
        Seq( Outermost )
    } else {
      if( index < intString.length-1 )
        Seq( Inner, Outermost )
      else
        Seq( Outermost )
    }
  }



  def lexEventCounts( index:Int, pDec:Decoration, marginal:Double ) = {
    val w = intString( index )
    if( index%2 == 0 ) {
      Seq( ( StopEvent( w, LeftAtt, pDec, Stop ) , marginal ) )
    } else {
      Seq( ( StopEvent( w, RightAtt, pDec, Stop ) , marginal ) )
    }
  }


  // NEW DEFINITIONS

  def rightwardSplitSpecs( i:Int, j:Int ) = {
    if( j == intString.length ) {
      Seq(
        (
          Outermost,
          ( (i+2) to (j-1) by 2 ).map{ k =>
            ( k, DecorationPair( Inner, Outermost ), Outermost )
          }
        )
      )
    } else {
      Seq(
        (
          Outermost,
          ( (i+2) to (j-1) by 2 ).map{ k =>
            ( k, DecorationPair( Inner, Outermost ), Outermost )
          }
        ),
        (
          Inner,
          ( (i+2) to (j-1) by 2 ).map{ k =>
            ( k, DecorationPair( Inner, Outermost ), Outermost )
          }
        )
      )
    }
  }

  def leftwardSplitSpecs( i:Int, j:Int ) = {
    if( i == 0 ) {
      Seq(
        (
          Outermost,
          ( (i+1) to (j-2) by 2 ).map{ k =>
            ( k, DecorationPair( Outermost, Inner ), Outermost )
          }
        )
      )
    } else {
      Seq(
        (
          Outermost,
          ( (i+1) to (j-2) by 2 ).map{ k =>
            ( k, DecorationPair( Outermost, Inner), Outermost )
          }
        ),
        (
          Inner,
          ( (i+1) to (j-2) by 2 ).map{ k =>
            ( k, DecorationPair( Outermost, Inner ), Outermost )
          }
        )
      )
    }
  }

  val mParents =
    Seq( DecorationPair( Outermost, Inner ) , DecorationPair( Inner, Outermost ) )
  def mSplitSpecs( i:Int, j:Int ) = {
    val ks = ( (i+1) to (j-1) by 2 )
    mParents.map{ mDec => ( mDec, ks ) }
  }

  def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).map{ k =>
      (
        k,
        DecorationPair(
          {
            Outermost
          }, {
            Outermost
          }
        )
      )
    }
  }


  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    val dep = intString( k )

    myTimes(
      theta( ChooseEvent( head, RightAtt, dep ) ),
        theta( StopEvent( head, RightAtt, pDec, NotStop ) )
    )
  }
  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )
    val dep = intString( k )

    myTimes(
      theta( ChooseEvent( head, LeftAtt, dep ) ),
        theta( StopEvent( head, LeftAtt, pDec, NotStop ) )
    )
  }


  def rootCellFactor( k:Int ) = {
    theta( RootEvent( intString( k ) ) )
  }


  def rootEventCounts( k:Int, marginal:Double ) = {
    val r = intString( k )
    Seq(
      ( RootEvent( r ), marginal )
    )
  }
  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( i )
    val dep = intString( k )
    Seq(
      ( ChooseEvent( head, RightAtt, dep ), marginal ),
      ( StopEvent( head, RightAtt, pDec, NotStop ), marginal )
    )
  }
  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( j )
    val dep = intString( k )
    // println( ( ChooseEvent( head, LeftAtt, dep ), marginal ) )

    // if( head == 6 && dep == 3 ) { println( marginal ) }

    Seq(
      ( ChooseEvent( head, LeftAtt, dep ), marginal ),
      ( StopEvent( head, LeftAtt, pDec, NotStop ), marginal )
    )
  }


  def trueLogProb( counts:DMVCounts ) = {
    // trueLogProb is always in log-space, so don't use myTimes
    theta.p_root.trueLogProb( counts.rootCounts ) +
    theta.p_stop.trueLogProb( counts.stopCounts ) +
    theta.p_choose.trueLogProb( counts.chooseCounts )
  }

}


// vim: set ts=2 sw=2 et:
