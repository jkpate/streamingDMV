package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.HeadOutAdjHeadNoValenceParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class HeadOutAdjHeadNoValenceParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends SecondOrderFoldUnfoldParser[HeadOutAdjHeadNoValenceParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  val theta = new HeadOutAdjHeadNoValenceParameters( rootAlpha, stopAlpha, chooseAlpha )

  val insideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap(
        PlainM -> 0D,
        LeftwardM -> 0D,
        RightwardM -> 0D
      )
    } else {
      MMap()
    }
  )

  val outsideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap(
        PlainM -> 0D,
        LeftwardM -> 0D,
        RightwardM -> 0D
      )
    } else {
      MMap()
    }
  )

  def findLeftRootChild( k:Int ) =
    headTrace( 0 )( k )( NoValence )
  def findRightRootChild( k:Int ) =
    headTrace( k )( intString.length )( NoValence )

  def findLeftLeftwardChild( i:Int, k:Int ) = {
    // assert( k-i > 1 )
    headTrace( i )( k )( NoValence )
  }
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) = {
    assert( j-k > 1 )
    mTrace( k )( j )( LeftwardM )
  }
  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) = {
    assert( k-i > 1 )
    mTrace( i )( k )( RightwardM )
  }
  def findRightRightwardChild( k:Int, j:Int ) = {
    // assert( j-k > 1 )
    headTrace( k )( j )( NoValence )
  }

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) = {
    if( k%2 == 0 ) {
      headTrace( i )( k )( NoValence )
    } else {
      decoration match {
        case LeftwardM => mTrace( i )( k )( PlainM )
        case RightwardM => mTrace( i )( k )( RightwardM )
      }
    }
  }

  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) = {
    if( k%2 == 0 ) {
      headTrace( k )( j )( NoValence )
    } else {
      decoration match {
        case LeftwardM => mTrace( k )( j )( LeftwardM )
        case RightwardM => mTrace( k )( j )( PlainM )
      }
    }
  }

  def lexFill( index:Int ) {
    val head = intString( index )
    insideChart(index)(index+1)( NoValence ) = 1D
  }

  def nearestArcFactor( head:Int, dir:AttDir, dep:Int ) = {
    theta( ChooseEvent( head, dir, dep ) ) *
    theta( StopEvent( head, dir, NoValence, NotStop ) ) *
      theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) *
      theta( StopEvent( dep, dir, NoValence, Stop ) )
  }
  def outerArcFactor( head:Int, context:Int, dir:AttDir, dep:Int ) = {
    theta( ChooseEvent( head, context, dir, dep ) ) *
      theta( StopEvent( head, dir, NoValence, NotStop ) ) *
        theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) *
        theta( StopEvent( dep, dir, NoValence, Stop ) )
  }


  def lexMarginals( index:Int ) = Seq()


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
            RightwardM,
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
            LeftwardM,
            NoValence
          )
        }
      )
    )
  }

  def mSplitSpecs( i:Int, j:Int ) = {
    Seq(
      (
        PlainM,
        ( (i+1) to (j-1) by 2 )
      ),
      (
        RightwardM,
        ( (i+2) to (j-2) by 2 ) :+ (i+1)
      ),
      (
        LeftwardM,
        ( (i+2) to (j-2) by 2 ) :+ (j-1)
      )
    )
  }
  def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).map{ k => ( k, DecorationPair(NoValence, NoValence) ) }
  }

  def mCellFactor( i:Int, k:Int, j:Int, decoration:MDecoration ) = {
    if( decoration == PlainM ) {
      1D
    } else if( decoration == RightwardM ) {
      if( k-i == 1 )
        nearestArcFactor( intString(i), RightAtt, intString(j) )
      else
        outerArcFactor( intString(i), intString(k), RightAtt, intString(j) )
    } else { // LeftwardM
      if( j-k == 1 )
        nearestArcFactor( intString(j), LeftAtt, intString(i) )
      else
        outerArcFactor( intString(j), intString(k), LeftAtt, intString(i) )
    }
  }
  def rootCellFactor( k:Int ) = {
    val r = intString( k )

    theta( RootEvent( r ) ) *
      theta( StopEvent( r, LeftAtt, NoValence, Stop ) ) *
      theta( StopEvent( r, RightAtt, NoValence, Stop ) )
  }
  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = 1D
  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = 1D

  def rootEventCounts( k:Int, marginal:Double ) = {
    val r = intString( k )
    Seq(
      ( RootEvent( r ), marginal ),
      ( StopEvent( r, LeftAtt, NoValence, Stop ), marginal ),
      ( StopEvent( r, RightAtt, NoValence, Stop ), marginal )
    )
  }
  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = Seq()
  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = Seq()
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:Double ) = {
    if( k%2 == 0 ) { // only Directed children
      // LeftwardM
      if( j-k == 1 && mDecoration == LeftwardM) {
        val lHead = intString(j)
        val lDep = intString(i)
        Seq(
          ( ChooseEvent( lHead, LeftAtt, lDep ), marginal ),
          ( StopEvent( lHead, LeftAtt, NoValence, NotStop ), marginal ),
          ( StopEvent( lDep, RightAtt, NoValence, Stop ), marginal ),
          ( StopEvent( lDep, LeftAtt, NoValence, Stop ), marginal )
        )
      } else if( k-i == 1 && mDecoration == RightwardM ) {
        val rHead = intString(i)
        val rDep = intString(j)
        Seq(
          ( ChooseEvent( rHead, RightAtt, rDep ), marginal ),
          ( StopEvent( rHead, RightAtt, NoValence, NotStop ), marginal ),
          ( StopEvent( rDep, LeftAtt, NoValence, Stop ), marginal ),
          ( StopEvent( rDep, RightAtt, NoValence, Stop ), marginal )
        )
      } else {
        Seq()
      }
    } else { // parent is DirectedM
      assert( mDecoration == LeftwardM || mDecoration == RightwardM )
      val context = intString(k)
      if( mDecoration == RightwardM ) {
        val rHead = intString(i)
        val rDep = intString(j)
        Seq(
          ( ChooseEvent( rHead, context, RightAtt, rDep ), marginal ),
          ( StopEvent( rHead, RightAtt, NoValence, NotStop ), marginal ),
          ( StopEvent( rDep, LeftAtt, NoValence, Stop ), marginal ),
          ( StopEvent( rDep, RightAtt, NoValence, Stop ), marginal )
        )
      } else {// if( mDecoration == LeftwardM )
        val lHead = intString(j)
        val lDep = intString(i)
        Seq(
          ( ChooseEvent( lHead, context, LeftAtt, lDep ), marginal ),
          ( StopEvent( lHead, LeftAtt, NoValence, NotStop ), marginal ),
          ( StopEvent( lDep, RightAtt, NoValence, Stop ), marginal ),
          ( StopEvent( lDep, LeftAtt, NoValence, Stop ), marginal )
        )
      }
    }
  }

}

