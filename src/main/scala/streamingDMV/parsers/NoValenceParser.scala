package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.NoValenceParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class NoValenceParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FirstOrderFoldUnfoldNOPOSParser[NoValenceParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  val theta = new NoValenceParameters( rootAlpha, stopAlpha, chooseAlpha )


  val insideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> 0D )
    } else {
      MMap[Decoration,Double]()
    }
  )

  val outsideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> 0D )
    } else {
      MMap[Decoration,Double]()
    }
  )



  def findLeftRootChild( k:Int ) = headTrace( 0 )( k )( NoValence )
  def findRightRootChild( k:Int ) =
    headTrace( k )( intString.length )( NoValence )

  def findLeftLeftwardChild( i:Int, k:Int ) = headTrace( i )( k )( NoValence )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( k )( j )( PlainM )
  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( i )( k )( PlainM )
  def findRightRightwardChild( k:Int, j:Int ) = headTrace( k )( j )( NoValence )
  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) = headTrace( i )( k )( NoValence )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) = headTrace( k )( j )( NoValence )

  def lexFill( index:Int ) {
    val head = intString( index )
    insideChart(index)(index+1)( NoValence ) = 1D
  }

  def lexMarginals( index:Int ) = Seq()

  def outsideRootWithMarginals( k:Int ) = {
    val obs = intString( k )

    val factor =
      theta( RootEvent( obs ) ) *
        theta( StopEvent( obs, LeftAtt, NoValence, Stop ) ) *
        theta( StopEvent( obs, RightAtt, NoValence, Stop ) )

    outsideChart( 0 )( k )( NoValence ) += 
      insideChart( k )( intString.length )( NoValence ) * factor

    outsideChart( k )( intString.length )( NoValence ) += 
      insideChart( 0 )( k )( NoValence ) * factor

    val marginal = 
      insideChart( 0 )( k )( NoValence ) *
        insideChart( k )( intString.length )( NoValence ) *
          factor
    Seq(
      ( RootEvent( obs ) , marginal ),
      ( StopEvent( obs, LeftAtt, NoValence, Stop ) , marginal ),
      ( StopEvent( obs, RightAtt, NoValence, Stop ) , marginal )
    )

  }


  def viterbiLexFill( index:Int ) {
    insideChart(index)(index+1)( NoValence ) = 1D
    headTrace(index)(index+1) += NoValence -> LexEntry( index )
  }


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


  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    val dep = intString( k )

    theta( ChooseEvent( head, RightAtt, dep ) ) *
      theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
        theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
        theta( StopEvent( dep, RightAtt, NoValence, Stop ) )
  }

  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )
    val dep = intString( k )

    theta( ChooseEvent( head, LeftAtt, dep ) ) *
      theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
        theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
        theta( StopEvent( dep, LeftAtt, NoValence, Stop ) )
  }


  def rootCellFactor( k:Int ) = {
    val r = intString( k )

    theta( RootEvent( r ) ) *
      theta( StopEvent( r, LeftAtt, NoValence, Stop ) ) *
      theta( StopEvent( r, RightAtt, NoValence, Stop ) )
  }


  def rootEventCounts( k:Int, marginal:Double ) = {
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

}

