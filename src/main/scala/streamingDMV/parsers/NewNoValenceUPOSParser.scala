package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.NoValenceUPOSParameters

import breeze.linalg._
import breeze.numerics._

import scala.collection.mutable.{Map=>MMap}


class NewNoValenceUPOSParser(
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  parserSpec:ParserSpec
  // randomSeed:Int = 15
) extends NewFirstOrderFoldUnfoldUPOSParser[NoValenceUPOSParameters](
  //maxLength, rootAlpha, stopAlpha, chooseAlpha, uposCount, randomSeed
  parserSpec
) {

  // val theta = new NoValenceUPOSParameters( rootAlpha, stopAlpha, chooseAlpha, uposCount )
  val theta = new NoValenceUPOSParameters( uposCount, parserSpec.toParameterSpec )

  def trueLogProb(counts: streamingDMV.labels.MatrixDMVCounts): Double = {
    throw new UnsupportedOperationException( "haven't implemented true Dirichlet probability yet." )
  }

  def cellMap( i:Int, j:Int ) = {
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> DenseMatrix.zeros[Double](uposCount, 1) )
    } else if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> DenseMatrix.zeros[Double](uposCount,uposCount) )
    } else {
      MMap()
    }
  }

  def findLeftRootChild( k:Int, rootPos:Int ) = 
    headTrace( 0 )( k )( NoValence, rootPos )
  def findRightRootChild( k:Int, rootPos:Int ) = 
    headTrace( k )( intString.length )( NoValence, rootPos )

  def findLeftLeftwardChild( i:Int, k:Int, dPos:Int ) =
    headTrace( i )( k )( NoValence, dPos )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    mTrace( k )( j )( ( PlainM, dPos, hPos ) )

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    mTrace( i )( k )( ( PlainM, hPos, dPos ) )
  def findRightRightwardChild( k:Int, j:Int, dPos:Int ) =
    headTrace( k )( j )( NoValence, dPos )

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration, lPos:Int ) =
    headTrace( i )( k )( NoValence, lPos )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration, rPos:Int ) =
    headTrace( k )( j )( NoValence, rPos )


  def arcFactor( head:Int, dir:AttDir, dep:Int ) = {
    val factor = theta( ChooseEvent( head, dir, dep ) )

    factor :*= (
      (
        theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) :*
        theta( StopEvent( dep, dir, NoValence, Stop ) )
      ).t *
      theta( StopEvent( head, dir, NoValence, NotStop ) )
    )

    factor
  }

  def leftwardCellFactor(
    i:Int, k:Int, j:Int,
    pDec:Decoration, mDec:MDecoration, cDec:Decoration
  ) = {
    val head = intString( j )
    val dep = intString( k )
    arcFactor( head, LeftAtt, dep )
  }
  def rightwardCellFactor(
    i:Int, k:Int, j:Int,
    pDec:Decoration, mDec:MDecoration, cDec:Decoration
  ) = {
    val head = intString( i )
    val dep = intString( k )
    arcFactor( head, RightAtt, dep )
  }

  def rootCellFactor( k:Int ) = {
    val r = intString( k )
    theta( RootEvent( r ) ) :*
      theta( StopEvent( r, LeftAtt, NoValence, Stop ) ).t :*
      theta( StopEvent( r, RightAtt, NoValence, Stop ) ).t
  }

  def lexCellFactor( index:Int, pDec:Decoration ) = {
    DenseMatrix.ones[Double]( uposCount, 1 )
  }

  def rootEventCounts( k:Int, marginal:DenseMatrix[Double] ) = {
    val r = intString( k )
    Seq(
      ( RootEvent( r ), marginal ),
      ( StopEvent( r, LeftAtt, NoValence, Stop ), marginal.t ),
      ( StopEvent( r, RightAtt, NoValence, Stop ), marginal.t )
    )
  }

  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:DenseMatrix[Double] ) = {
    val head = intString( i )
    val dep = intString( k )
    val notStopCount = sum( marginal(::,*) ).t.toDenseMatrix
    val stopCount = sum( marginal(*,::) ).toDenseMatrix

    // println( (i,k,j, head, dep ) )
    // println( marginal )

    Seq(
      ( ChooseEvent( head, RightAtt, dep ), marginal.t ),
      ( StopEvent( head, RightAtt, NoValence, NotStop ), notStopCount ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ), stopCount ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ), stopCount )
    )
  }

  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:DenseMatrix[Double] ) = {
    val head = intString( j )
    val dep = intString( k )
    val notStopCount = sum( marginal(::,*) ).t.toDenseMatrix
    val stopCount = sum( marginal(*,::) ).toDenseMatrix
    Seq(
      ( ChooseEvent( head, LeftAtt, dep ), marginal ),
      ( StopEvent( head, LeftAtt, NoValence, NotStop ), notStopCount ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ), stopCount ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ), stopCount )
    )
  }

  def lexEventCounts( index:Int, pDec:Decoration, marginal:DenseMatrix[Double] ) = Seq()




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

  def lexSpecs( index:Int ) = Seq( NoValence )

}

