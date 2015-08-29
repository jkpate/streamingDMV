package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.OriginalDMVParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class OriginalDMVParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FirstOrderFoldUnfoldNOPOSParser[OriginalDMVParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  // println( s"my random seed is $randomSeed" )
  val theta = new OriginalDMVParameters( rootAlpha, stopAlpha, chooseAlpha )

  // One of the m-node children is a head-child, so they can't both be outermost
  val insideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      if( j-i >= 6 )
        MMap(
          DecorationPair(Outer,Outer) -> 0D,
          DecorationPair(Outer,Innermost) -> 0D,
          DecorationPair(Innermost,Outer) -> 0D
        )
      else if( j-i == 4 )
        MMap(
          DecorationPair(Outer,Innermost) -> 0D,
          DecorationPair(Innermost,Outer) -> 0D
        )
      else// if( j-i == 2 )
        MMap(
          DecorationPair(Innermost,Innermost) -> 0D
        )
    } else if( i%2 != j%2 ) {
      if( j-i > 1 )
        MMap( Outer -> 0D )
      else
        MMap( Innermost -> 0D )
    } else {
      MMap()
    }
  )

  val outsideChart = Array.tabulate[MMap[Decoration,Double]]( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      if( j-i >= 6 )
        MMap(
          DecorationPair(Outer,Outer) -> 0D,
          DecorationPair(Outer,Innermost) -> 0D,
          DecorationPair(Innermost,Outer) -> 0D
        )
      else if( j-i == 4 )
        MMap(
          DecorationPair(Outer,Innermost) -> 0D,
          DecorationPair(Innermost,Outer) -> 0D
        )
      else// if( j-i == 2 )
        MMap(
          DecorationPair(Innermost,Innermost) -> 0D
        )
    } else if( i%2 != j%2 ) {
      if( j-i > 1 )
        MMap( Outer -> 0D )
      else
        MMap( Innermost -> 0D )
    } else {
      MMap()
    }
  )




  private def adj( x:Int, y:Int ) = 
    if( math.abs(x-y) == 1 ) Innermost else Outer

  def findLeftRootChild( k:Int ) = headTrace( 0 )( k )( adj( 0, k ) )
  def findRightRootChild( k:Int ) =
    headTrace( k )( intString.length )( adj( k, intString.length ) )

  def findLeftLeftwardChild( i:Int, k:Int ) = headTrace( i )( k )( adj( i, k ) )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( k )( j )( DecorationPair( mDV, hV ) )
  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( i )( k )( DecorationPair( hV, mDV ) )
  def findRightRightwardChild( k:Int, j:Int ) = headTrace( k )( j )( adj( k, j ) )
  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) =
    headTrace( i )( k )( decoration.evenLeft )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) =
    headTrace( k )( j )( decoration.evenRight )

  // def lexFill( index:Int ) {
  //   val head = intString( index )
  //   insideChart(index)(index+1)( Innermost ) = 1D
  // }
  def lexSpecs( index:Int ) = Seq( Innermost )
  def lexCellFactor( index:Int, pDec:Decoration ) = 1D


  def lexMarginals( index:Int ) = Seq[Tuple2[Event,Double]]()





  // NEW DEFINITIONS

  def rightwardSplitSpecs( i:Int, j:Int ) = {
    // println( "." )
    // val rwSpecs =
      Seq(
        (
          Outer,
                  // if( j-i == 3 )
                  //   Seq(
                  //     (
                  //       i+2,
                  //       DecorationPair( Innermost, Innermost ),
                  //       Innermost
                  //     )
                  //   )
                  // else if( j-i == 5 )
                  //   Seq(
                  //     (
                  //       i+2,
                  //       DecorationPair( Innermost, Innermost ),
                  //       Outer
                  //     ),
                  //     (
                  //       i+4,
                  //       DecorationPair( Outer, Innermost ),
                  //       Innermost
                  //     ),
                  //     (
                  //       i+4,
                  //       DecorationPair( Innermost, Outer ),
                  //       Innermost
                  //     )
                  //   )
                  // else if( j-i == 7 )
                  //   Seq(
                  //     (
                  //       i+2,
                  //       DecorationPair( Innermost, Innermost ),
                  //       Outer
                  //     ),
                  //     (
                  //       i+4,
                  //       DecorationPair( Outer, Innermost ),
                  //       Outer
                  //     ),
                  //     (
                  //       i+4,
                  //       DecorationPair( Innermost, Outer ),
                  //       Outer
                  //     ),
                  //     (
                  //       j-1,
                  //       DecorationPair( Outer, Innermost ),
                  //       Innermost
                  //     ),
                  //     (
                  //       j-1,
                  //       DecorationPair( Innermost, Outer ),
                  //       Innermost
                  //     ),
                  //     (
                  //       j-1,
                  //       DecorationPair( Outer, Outer ),
                  //       Innermost
                  //     )
                  //   )
                  // else // j-i >= 9
                  //   Seq(
                  //     (
                  //       i+2,
                  //       DecorationPair( Innermost, Innermost ),
                  //       Outer
                  //     ),
                  //     (
                  //       i+4,
                  //       DecorationPair( Outer, Innermost ),
                  //       Outer
                  //     ),
                  //     (
                  //       i+4,
                  //       DecorationPair( Innermost, Outer ),
                  //       Outer
                  //     ),
                  //     (
                  //       j-1,
                  //       DecorationPair( Outer, Innermost ),
                  //       Innermost
                  //     ),
                  //     (
                  //       j-1,
                  //       DecorationPair( Innermost, Outer ),
                  //       Innermost
                  //     ),
                  //     (
                  //       j-1,
                  //       DecorationPair( Outer, Outer ),
                  //       Innermost
                  //     )
                  //   ) ++ ( (i+6) to (j-3) by 2 ).flatMap{ k =>
                  //     Seq(
                  //       (
                  //         k,
                  //         DecorationPair( Outer, Innermost ),
                  //         Outer
                  //       ),
                  //       (
                  //         k,
                  //         DecorationPair( Innermost, Outer ),
                  //         Outer
                  //       ),
                  //       (
                  //         k,
                  //         DecorationPair( Outer, Outer ),
                  //         Outer
                  //       )
                  //     )
                  //   }

            ( (i+2) to (j-1) by 2 ).flatMap{ k =>
              val childDec =
                if( j-k == 1 ) Innermost else Outer
              if( k-i >= 6 ) {
                Seq(
                  (
                    k,
                    DecorationPair( Innermost, Outer ),
                    childDec
                  ),
                  (
                    k,
                    DecorationPair( Outer, Innermost ),
                    childDec
                  ),
                  (
                    k,
                    DecorationPair( Outer, Outer ),
                    childDec
                  )
                )
              } else if( k-i == 4 ) {
                Seq(
                  (
                    k,
                    DecorationPair( Innermost, Outer ),
                    childDec
                  ),
                  (
                    k,
                    DecorationPair( Outer, Innermost ),
                    childDec
                  )
                )
              } else { // k-i == 2
                assert( k-i == 2 )
                Seq(
                  (
                    k,
                    DecorationPair( Innermost, Innermost ),
                    childDec
                  )
                )
              }
            }
        )
      )

    // println( rwSpecs.mkString("\n\t","\n\t","\n\n" ) )
    // rwSpecs
  }

  def leftwardSplitSpecs( i:Int, j:Int ) = {
    Seq(
      (
        Outer,
          ( (i+1) to (j-2) by 2 ).flatMap{ k =>
            val childDec =
              if( k-i == 1 ) Innermost else Outer
            if( j-k >= 6 ) {
              Seq(
                (
                  k,
                  DecorationPair( Innermost, Outer ),
                  childDec
                ),
                (
                  k,
                  DecorationPair( Outer, Innermost ),
                  childDec
                ),
                (
                  k,
                  DecorationPair( Outer, Outer ),
                  childDec
                )
              )
            } else if( j-k == 4 ) {
              Seq(
                (
                  k,
                  DecorationPair( Innermost, Outer ),
                  childDec
                ),
                (
                  k,
                  DecorationPair( Outer, Innermost ),
                  childDec
                )
              )
            } else { // j-k == 2
              assert( j-k == 2 )
              Seq(
                (
                  k,
                  DecorationPair( Innermost, Innermost ),
                  childDec
                )
              )
            }
          }
      )
    )
  }

  def mSplitSpecs( i:Int, j:Int ):Seq[Tuple2[MDecoration,Seq[Int]]] = {

    if( j-i >= 6 ) {
      Seq(
        (
          DecorationPair( Innermost, Outer ),
          Seq( i+1 )
        ),
        (
          DecorationPair( Outer, Innermost ),
          Seq( j-1 )
        ),
        (
          DecorationPair( Outer, Outer ),
          ( (i+3) to (j-3) by 2 )
        )
      )
    } else if( j-i == 4 ) {
      Seq(
        (
          DecorationPair( Innermost, Outer ),
          Seq( i+1 )
        ),
        (
          DecorationPair( Outer, Innermost ),
          Seq( j-1 )
        )
      )
    } else { // j-i == 2
      assert( j-i == 2 )
      Seq(
        (
          DecorationPair( Innermost, Innermost ),
          Seq( j-1 )
        )
      )
    }
  }

  def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).map{ k =>
      val leftV = if( k == 1 ) Innermost else Outer
      val rightV = if( k == intString.length-1 ) Innermost else Outer
      (k,DecorationPair(leftV,rightV))
    }
  }



  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    val dep = intString( k )

    theta( ChooseEvent( head, RightAtt, dep ) ) *
      theta( StopEvent( head, RightAtt, mDec.evenLeft, NotStop ) ) *
        theta( StopEvent( dep, LeftAtt, mDec.evenRight, Stop ) ) *
        theta( StopEvent( dep, RightAtt, cDec, Stop ) )
  }

  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )
    val dep = intString( k )

    theta( ChooseEvent( head, LeftAtt, dep ) ) *
      theta( StopEvent( head, LeftAtt, mDec.evenRight, NotStop ) ) *
        theta( StopEvent( dep, RightAtt, mDec.evenLeft, Stop ) ) *
        theta( StopEvent( dep, LeftAtt, cDec, Stop ) )
  }


  def rootCellFactor( k:Int ) = {
    val r = intString( k )

    val leftV = if( k == 1 ) Innermost else Outer
    val rightV = if( k == intString.length-1 ) Innermost else Outer

    theta( RootEvent( r ) ) *
      theta( StopEvent( r, LeftAtt, leftV, Stop ) ) *
      theta( StopEvent( r, RightAtt, rightV, Stop ) )
  }


  def rootEventCounts( k:Int, marginal:Double ) = {
    val r = intString( k )
    val leftV = if( k == 1 ) Innermost else Outer
    val rightV = if( k == intString.length-1 ) Innermost else Outer
    Seq(
      ( RootEvent( r ), marginal ),
      ( StopEvent( r, LeftAtt, leftV, Stop ), marginal ),
      ( StopEvent( r, RightAtt, rightV, Stop ), marginal )
    )
  }

  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( i )
    val dep = intString( k )
    Seq(
      ( ChooseEvent( head, RightAtt, dep ), marginal ),
      ( StopEvent( head, RightAtt, mDec.evenLeft, NotStop ), marginal ),
      ( StopEvent( dep, LeftAtt, mDec.evenRight, Stop ), marginal ),
      ( StopEvent( dep, RightAtt, cDec, Stop ), marginal )
    )
  }

  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( j )
    val dep = intString( k )
    Seq(
      ( ChooseEvent( head, LeftAtt, dep ), marginal ),
      ( StopEvent( head, LeftAtt, mDec.evenRight, NotStop ), marginal ),
      ( StopEvent( dep, RightAtt, mDec.evenLeft, Stop ), marginal ),
      ( StopEvent( dep, LeftAtt, cDec, Stop ), marginal )
    )
  }

  def trueLogProb( counts:DMVCounts ) = {
    theta.p_root.trueLogProb( counts.rootCounts ) +
    theta.p_stop.trueLogProb( counts.stopCounts ) +
    theta.p_choose.trueLogProb( counts.chooseCounts )
  }


}

