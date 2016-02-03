package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.TwoValenceUPOSParameters

import breeze.linalg._
import breeze.numerics._

import scala.collection.mutable.{Map=>MMap}


class NewTwoValenceUPOSParser(
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  parserSpec:ParserSpec
  // randomSeed:Int = 15
) extends NewFirstOrderFoldUnfoldUPOSParser[TwoValenceUPOSParameters](
  //maxLength, rootAlpha, stopAlpha, chooseAlpha, uposCount, randomSeed
  parserSpec
) {

  // val theta = new NoValenceUPOSParameters( rootAlpha, stopAlpha, chooseAlpha, uposCount )
  val theta = new TwoValenceUPOSParameters( uposCount, parserSpec.toParameterSpec )


  def cellMap( i:Int, j:Int ) = {
    if( ( i%2 != j%2 ) ) {
      MMap(
        Outermost -> DenseMatrix.zeros[Double](uposCount, 1),
        Inner -> DenseMatrix.zeros[Double](uposCount, 1)
      )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap(
        DecorationPair(Outermost,Inner) -> DenseMatrix.zeros[Double](uposCount,uposCount),
        DecorationPair(Inner,Outermost) -> DenseMatrix.zeros[Double](uposCount,uposCount)
      )
    } else {
      MMap()
    }
  }

  def findLeftRootChild( k:Int, rootPos:Int ) = 
    headTrace( 0 )( k )( Outermost, rootPos )
  def findRightRootChild( k:Int, rootPos:Int ) = 
    headTrace( k )( intString.length )( Outermost, rootPos )

  def findLeftLeftwardChild( i:Int, k:Int, dPos:Int ) =
    headTrace( i )( k )( Outermost, dPos )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    mTrace( k )( j )( ( DecorationPair( Outermost, Inner ), dPos, hPos ) )

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    mTrace( i )( k )( ( DecorationPair( Inner, Outermost ), hPos, dPos ) )
  def findRightRightwardChild( k:Int, j:Int, dPos:Int ) =
    headTrace( k )( j )( Outermost, dPos )

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration, lPos:Int ) =
    headTrace( i )( k )( decoration.evenLeft, lPos )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration, rPos:Int ) =
    headTrace( k )( j )( decoration.evenRight, rPos )


  def arcFactor( head:Int, dir:AttDir, dep:Int, pDec:Decoration ) = {
    val factor = theta( ChooseEvent( head, dir, dep ) )

      // factor :*= (
      //   (
      //     theta( StopEvent( dep, dir.flip, Outermost, Stop ) ) :*
      //     theta( StopEvent( dep, dir, Outermost, Stop ) )
      //   ).t *
      //   theta( StopEvent( head, dir, pDec, NotStop ) )
      // )

    factor( *, :: ) :* theta( StopEvent( head, dir, pDec, NotStop ) ).toDenseVector
  }

  def leftwardCellFactor(
    i:Int, k:Int, j:Int,
    pDec:Decoration, mDec:MDecoration, cDec:Decoration
  ) = {
    val head = intString( j )
    val dep = intString( k )
    assert( cDec == Outermost )
    arcFactor( head, LeftAtt, dep, pDec )
  }
  def rightwardCellFactor(
    i:Int, k:Int, j:Int,
    pDec:Decoration, mDec:MDecoration, cDec:Decoration
  ) = {
    val head = intString( i )
    val dep = intString( k )
    assert( cDec == Outermost )
    arcFactor( head, RightAtt, dep, pDec )
  }

  def rootCellFactor( k:Int ) = {
    val r = intString( k )
    theta( RootEvent( r ) ) /* :*
      theta( StopEvent( r, LeftAtt, NoValence, Stop ) ).t :*
      theta( StopEvent( r, RightAtt, NoValence, Stop ) ).t */
  }

  def lexCellFactor( index:Int, pDec:Decoration ) = {
    // DenseMatrix.ones[Double]( uposCount, 1 )
    val head = intString( index )
    if( index%2 == 0 ) {
      theta( StopEvent( head, LeftAtt, pDec, Stop ) ).t
    } else {
      theta( StopEvent( head, RightAtt, pDec, Stop ) ).t
    }
  }



  def rootEventCounts( k:Int, marginal:DenseMatrix[Double] ) = {
    val r = intString( k )
    Seq(
      ( RootEvent( r ), marginal )// ,
        // ( StopEvent( r, LeftAtt, NoValence, Stop ), marginal.t ),
        // ( StopEvent( r, RightAtt, NoValence, Stop ), marginal.t )
    )
  }

  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:DenseMatrix[Double] ) = {
    val head = intString( i )
    val dep = intString( k )
    val notStopCount = sum( marginal(::,*) ).t.toDenseMatrix
      // val stopCount = sum( marginal(*,::) ).toDenseMatrix

    // println( (i,k,j, head, dep ) )
    // println( marginal )

    Seq(
      ( ChooseEvent( head, RightAtt, dep ), marginal.t ),
      ( StopEvent( head, RightAtt, NoValence, NotStop ), notStopCount )//,
        // ( StopEvent( dep, LeftAtt, NoValence, Stop ), stopCount ),
        // ( StopEvent( dep, RightAtt, NoValence, Stop ), stopCount )
    )
  }

  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:DenseMatrix[Double] ) = {
    val head = intString( j )
    val dep = intString( k )
    val notStopCount = sum( marginal(::,*) ).t.toDenseMatrix
      // val stopCount = sum( marginal(*,::) ).toDenseMatrix
    Seq(
      ( ChooseEvent( head, LeftAtt, dep ), marginal ),
      ( StopEvent( head, LeftAtt, NoValence, NotStop ), notStopCount )//,
      // ( StopEvent( dep, RightAtt, NoValence, Stop ), stopCount ),
      // ( StopEvent( dep, LeftAtt, NoValence, Stop ), stopCount )
    )
  }

  // def lexEventCounts( index:Int, pDec:Decoration, marginal:DenseMatrix[Double] ) = Seq()
  def lexEventCounts( index:Int, pDec:Decoration, marginal:DenseMatrix[Double] ) = {
    val w = intString( index )
    if( index%2 == 0 ) {
      Seq( ( StopEvent( w, LeftAtt, pDec, Stop ) , marginal.t ) )
    } else {
      Seq( ( StopEvent( w, RightAtt, pDec, Stop ) , marginal.t ) )
    }
  }



  // TODO: split specs might generalize across UPOS and NOPOS parsers

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

      // def rightwardSplitSpecs( i:Int, j:Int ) = {
      //   Seq(
      //     (
      //       NoValence,
      //       ( (i+2) to (j-1) by 2 ).map{ k =>
      //         (
      //           k,
      //           PlainM,
      //           NoValence
      //         )
      //       }
      //     )
      //   )
      // }

      // def leftwardSplitSpecs( i:Int, j:Int ) = {
      //   Seq(
      //     (
      //       NoValence,
      //       ( (i+1) to (j-2) by 2 ).map{ k =>
      //         (
      //           k,
      //           PlainM,
      //           NoValence
      //         )
      //       }
      //     )
      //   )
      // }

      // def mSplitSpecs( i:Int, j:Int ) = {
      //   Seq( ( PlainM, ( (i+1) to (j-1) by 2 ) ) )
      // }

      // def rootSplitSpecs() = {
      //   ( 1 to (intString.length-1) by 2 ).map{ k => ( k, DecorationPair(NoValence, NoValence) ) }
      // }

      // def lexSpecs( index:Int ) = Seq( NoValence )

}


