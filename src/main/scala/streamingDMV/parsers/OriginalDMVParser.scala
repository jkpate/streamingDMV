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

  def lexFill( index:Int ) {
    val head = intString( index )
    insideChart(index)(index+1)( Innermost ) = 1D
  }


  def lexMarginals( index:Int ) = Seq[Tuple2[Event,Double]]()
      // def outsideRootWithMarginals( k:Int ) = {
      //   val obs = intString( k )

      //   val leftV = adj( 0, k )
      //   val rightV = adj( k, intString.length )
      //   val factor =
      //     theta( RootEvent( obs ) ) *
      //       theta( StopEvent( obs, LeftAtt, leftV, Stop ) ) *
      //       theta( StopEvent( obs, RightAtt, rightV, Stop ) )

      //   outsideChart( 0 )( k )( leftV ) += 
      //     insideChart( k )( intString.length )( rightV ) * factor

      //   outsideChart( k )( intString.length )( rightV ) += 
      //     insideChart( 0 )( k )( leftV ) * factor

      //   val marginal = 
      //     insideChart( 0 )( k )( leftV ) *
      //       insideChart( k )( intString.length )( rightV ) *
      //         factor
      //   Seq[Tuple2[Event,Double]](
      //     ( RootEvent( obs ) , marginal ),
      //     ( StopEvent( obs, LeftAtt, leftV, Stop ) , marginal ),
      //     ( StopEvent( obs, RightAtt, rightV, Stop ) , marginal )
      //   )

      // }

      // def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) = {
      //   val head = intString( j )
      //   if( j-i >= 3 ) {
      //     // this is an Arc cell -- compute outside probs for children and return arc marginals

      //     val dep = intString( k )
      //     val cDV = adj( k, i )

      //     // wonky type inference with the scala compiler
      //     val valences = mValences( j-k )

      //     valences.map{ vs =>
      //       val DecorationPair( mDV, hV ) = vs
      //       val factorAndOutside =
      //         theta( ChooseEvent( head, LeftAtt, dep ) ) *
      //         theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
      //           theta( StopEvent( dep, RightAtt, mDV, Stop ) ) *
      //           theta( StopEvent( dep, LeftAtt, cDV, Stop ) ) *
      //             outsideChart( i )( j )( Outer )

      //       // First, send messages to left child -- that is, the leftward-looking dependent
      //       // label.
      //       outsideChart( i )( k )( cDV ) +=
      //           insideChart( k )( j )( vs ) * factorAndOutside

      //       // Now, send messages to right child -- that is, the M-label
      //       outsideChart( k )( j )( vs ) +=
      //           insideChart( i )( k )( cDV ) * factorAndOutside

      //       val marginal = 
      //         insideChart( i )( k )( cDV ) *
      //           insideChart( k )( j )( vs ) *
      //             factorAndOutside

      //       Seq[Tuple2[Event,Double]](
      //         ( ChooseEvent( head, LeftAtt, dep ) , marginal ),
      //         ( StopEvent( head, LeftAtt, hV, NotStop ) , marginal ),
      //         ( StopEvent( dep, RightAtt, mDV, Stop ) , marginal ),
      //         ( StopEvent( dep, LeftAtt, cDV, Stop ) , marginal )
      //       )
      //     }.reduce( _++_ )
      //   } else {
      //     // this is a (pre-)terminal cell -- do nothing
      //     Seq()
      //   }
      // }

      // def outsideRightWithMarginals( i:Int, k:Int, j:Int ) = {
      //   val head = intString( i )
      //   if( j-i >= 3 ) {
      //     // this is an Arc cell -- compute outside probs for children and return arc marginals

      //     val dep = intString( k )
      //     val cDV = adj( j, k )

      //     // wonky type inference with the scala compiler
      //     val valences = mValences( k-i )

      //     valences.map{ vs  =>
      //       val DecorationPair( hV, mDV ) = vs
      //       val factorAndOutside =
      //         theta( ChooseEvent( head, RightAtt, dep ) ) *
      //         theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
      //           theta( StopEvent( dep, LeftAtt, mDV, Stop ) ) *
      //           theta( StopEvent( dep, RightAtt, cDV, Stop ) ) *
      //           outsideChart( i )( j )( Outer )

      //       // First, send messages to right child -- that is, the rightward-looking dependent
      //       // label.
      //       outsideChart( k )( j )( cDV ) +=
      //           insideChart( i )( k )( vs ) * factorAndOutside

      //       // Now, send messages to right child -- that is, the M-label
      //       outsideChart( i )( k )( vs ) +=
      //           insideChart( k )( j )( cDV ) * factorAndOutside

      //       val marginal = 
      //         insideChart( k )( j )( cDV ) *
      //           insideChart( i )( k )( vs ) *
      //             factorAndOutside

      //       Seq[Tuple2[Event,Double]](
      //         ( ChooseEvent( head, RightAtt, dep ) , marginal ),
      //         ( StopEvent( head, RightAtt, hV, NotStop ) , marginal ),
      //         ( StopEvent( dep, LeftAtt, mDV, Stop ) , marginal ),
      //         ( StopEvent( dep, RightAtt, cDV, Stop ) , marginal )
      //       )
      //     }.reduce(_++_)
      //   } else {
      //     // this is a (pre-)terminal cell -- do nothing
      //     Seq()
      //   }
      // }

      // def populateRightwardCell( i:Int, k:Int, j:Int ) {
      //   val head = intString( i )
      //   val dep = intString( k )

      //   val cDV = adj( j, k )

      //   val valences = mValences( k-i )

      //   valences.foreach{ vs =>
      //     val DecorationPair( hV, mDV ) = vs
      //     insideChart( i )( j )( Outer ) +=
      //       insideChart( k )( j )( cDV ) *
      //         insideChart( i )( k )( vs ) *
      //           theta( ChooseEvent( head, RightAtt, dep ) ) *
      //           theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
      //             theta( StopEvent( dep, LeftAtt, mDV, Stop ) ) *
      //             theta( StopEvent( dep, RightAtt, cDV, Stop ) )
      //   }
      // }



  def populateRootCell( k:Int ) {
    val r = intString( k )

    val leftV = adj( 0, k )
    val rightV = adj( k, intString.length )

    stringProb +=
      insideChart(0)(k)(leftV) *
        insideChart(k)(intString.length)(rightV) *
          theta( RootEvent( r ) ) *
          theta( StopEvent( r, LeftAtt, leftV, Stop ) ) *
          theta( StopEvent( r, RightAtt, rightV, Stop ) )
  }

  def viterbiLexFill( index:Int ) {
    insideChart(index)(index+1)( Innermost ) = 1D
    headTrace(index)(index+1) += Innermost -> LexEntry( index )
  }



  // NEW DEFINITIONS

  def rightwardSplitSpecs( i:Int, j:Int ) = {
    Seq(
      (
        Outer,
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
        )
      ) ++ Seq(
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
      ( StopEvent( head, RightAtt, mDec.evenRight, NotStop ), marginal ),
      ( StopEvent( dep, LeftAtt, mDec.evenLeft, Stop ), marginal ),
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

}

