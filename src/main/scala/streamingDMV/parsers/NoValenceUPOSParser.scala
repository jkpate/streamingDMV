package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.NoValenceUPOSParameters

import breeze.linalg._
import breeze.numerics._

import scala.collection.mutable.{Map=>MMap}


class NoValenceUPOSParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  uposCount:Int,
  randomSeed:Int = 15
) extends FirstOrderFoldUnfoldUPOSParser[NoValenceUPOSParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, uposCount, randomSeed
) {

  val theta = new NoValenceUPOSParameters( rootAlpha, stopAlpha, chooseAlpha, uposCount )

  def leftArcParentVs( i:Int ) = Set[Decoration]( NoValence )
  def rightArcParentVs( j:Int ) = Set[Decoration]( NoValence )

  def lexMarginals( index:Int ) = Seq()

  def mNodeParentVs( i:Int, j:Int ) = Set( PlainM )

  def zerosMatrix = DenseMatrix.zeros[Double]( uposCount, uposCount )
  def zerosVector = DenseVector.zeros[Double]( uposCount )

  val insideHeads:Array[Array[MMap[Decoration,DenseVector[Double]]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> zerosVector )
    } else {
      MMap[Decoration,DenseVector[Double]]()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val insideM:Array[Array[MMap[MDecoration,DenseMatrix[Double]]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> zerosMatrix )
    } else {
      MMap[MDecoration,DenseMatrix[Double]]()
    }
  )

  val outsideHeads:Array[Array[MMap[Decoration,DenseVector[Double]]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> zerosVector )
    } else {
      MMap[Decoration,DenseVector[Double]]()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val outsideM:Array[Array[MMap[MDecoration,DenseMatrix[Double]]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> zerosMatrix )
    } else {
      MMap[MDecoration,DenseMatrix[Double]]()
    }
  )

  def findLeftRootChild( k:Int, rootPos:Int ) = 
    headTrace( 0 )( k )( NoValence, rootPos )
  def findRightRootChild( k:Int, rootPos:Int ) = 
    headTrace( k )( intString.length )( NoValence, rootPos )

  def findLeftLeftwardChild( i:Int, k:Int, dPos:Int ) =
    headTrace( i )( k )( NoValence, dPos )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    mTrace( k )( j )( PlainM, dPos, hPos )

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    mTrace( i )( k )( PlainM, hPos, dPos )
  def findRightRightwardChild( k:Int, j:Int, dPos:Int ) =
    headTrace( k )( j )( NoValence, dPos )

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration, lPos:Int ) =
    headTrace( i )( k )( NoValence, lPos )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration, rPos:Int ) =
    headTrace( k )( j )( NoValence, rPos )

  def lexFill( index:Int ) {
    val head = intString( index )
    insideHeads(index)(index+1)( NoValence ) = DenseVector.ones[Double](uposCount)
  }

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

  def rootFactor( k:Int ) = {
    val r = intString( k )
    theta( RootEvent( r ) ) :*
      theta( StopEvent( r, LeftAtt, NoValence, Stop ) ).t :*
      theta( StopEvent( r, RightAtt, NoValence, Stop ) ).t
  }

  def populateRightwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )
    insideHeads( i )( j )( NoValence ) :+= (
          insideHeads( k )( j )( NoValence ).t * (
        insideM( i )( k )( PlainM ).t :* arcFactor( head, RightAtt, dep )
      )
    ).t
  }

  def populateLeftwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )
    insideHeads( i )( j )( NoValence ) :+= (
          insideHeads( i )( k )( NoValence ).t * (
        insideM( k )( j )( PlainM ) :* arcFactor( head, LeftAtt, dep )
      )
    ).t
  }

  def populateMCell( i:Int, k:Int, j:Int ) {
    insideM( i )( j )( PlainM ) :+=
      insideHeads( i )( k )( NoValence ) * insideHeads( k )( j )( NoValence ).t
  }

  def populateRootCell( k:Int ) {
    val r = intString( k )

    stringProb +=
      sum( 
        insideHeads( 0 )( k )( NoValence ) :*
          insideHeads( k )( intString.length )( NoValence ) :*
            rootFactor( k ).toDenseVector
            // theta( RootEvent( r ) ).toDenseVector :*
            //   theta( StopEvent( r, LeftAtt, NoValence, Stop ) ).toDenseVector :*
            //   theta( StopEvent( r, RightAtt, NoValence, Stop ) ).toDenseVector
      )
  }

  def outsideRoot( k:Int ) {
    val r = intString( k )

    val factor = rootFactor( k ).toDenseVector
      // theta( RootEvent( r ) ).toDenseVector :*
      //   theta( StopEvent( r, LeftAtt, NoValence, Stop ) ).toDenseVector :*
      //   theta( StopEvent( r, RightAtt, NoValence, Stop ) ).toDenseVector

    outsideHeads( 0 )( k )( NoValence ) :+= 
      insideHeads( k )( intString.length )( NoValence ) :* factor

    outsideHeads( k )( intString.length )( NoValence ) :+= 
      insideHeads( 0 )( k )( NoValence ) :* factor
  }

  def outsideRootWithMarginals( k:Int ) = {
    val r = intString( k )

    val factor = (
      rootFactor( k )
    ).toDenseVector

    outsideHeads( 0 )( k )( NoValence ) :+= 
      insideHeads( k )( intString.length )( NoValence ) :* factor

    outsideHeads( k )( intString.length )( NoValence ) :+= 
      insideHeads( 0 )( k )( NoValence ) :* factor

    val marginal = (
      insideHeads( k )( intString.length )( NoValence ) :*
        insideHeads( 0 )( k )( NoValence ) :* factor
    ).asDenseMatrix

    Seq(
      ( RootEvent( r ) , marginal.t ),
      ( StopEvent( r, LeftAtt, NoValence, Stop ) , marginal ),
      ( StopEvent( r, RightAtt, NoValence, Stop ) , marginal )
    )
  }

  def outsideLeft( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )

    val factorAndOutside = 
      arcFactor( head, LeftAtt, dep )(*,::) :* outsideHeads( i )( j )( NoValence )

    val leftwardMessage = factorAndOutside :* insideM( k )( j )( PlainM )
    outsideHeads( i )( k )( NoValence ) :+=
      sum( leftwardMessage(*,::) )

    val rightwardMessage = factorAndOutside
    rightwardMessage( ::, * ) :*= insideHeads( i )( k )( NoValence )

    outsideM( k )( j )( PlainM ) :+=
      rightwardMessage
  }

  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( j )
    val dep = intString( k )

    val factorAndOutside = 
      arcFactor( head, LeftAtt, dep )(*,::) :* outsideHeads( i )( j )( NoValence )

    val leftwardMessage = factorAndOutside :* insideM( k )( j )( PlainM )
    outsideHeads( i )( k )( NoValence ) :+=
      sum( leftwardMessage(*,::) )

    val rightwardMessage = factorAndOutside
    rightwardMessage( ::, * ) :*= insideHeads( i )( k )( NoValence )

    outsideM( k )( j )( PlainM ) :+= rightwardMessage

    assert( insideM( k )( j )( PlainM ).rows == uposCount )
    assert( insideM( k )( j )( PlainM ).cols == uposCount )
    // val marginal = (
    //   insideM( k )( j )( PlainM )( ::, * ) :* insideHeads( i )( k )( NoValence )
    // )

    // marginal :*= factorAndOutside

    // val marginal = factorAndOutside :* insideM( k )( j )( PlainM )
    // marginal( *, :: ) :*= insideHeads( i )( k )( NoValence )

    val marginal =
      (
        insideHeads(i)(k)(NoValence) *
        outsideHeads(i)(j)(NoValence).t
      ) :* arcFactor( head, LeftAtt, dep ) :* insideM(k)(j)(PlainM)

    val headStopMarginal = sum( marginal( ::,* ) ).t.asDenseMatrix
    val depStopMarginal = sum( marginal( *,:: ) ).toDenseMatrix

    Seq(
      ( ChooseEvent( head, LeftAtt, dep ) , marginal ),
      ( StopEvent( head, LeftAtt, NoValence, NotStop ) , headStopMarginal ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ) , depStopMarginal ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ) , depStopMarginal )
    )
  }

  def outsideRight( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    val factorAndOutside = (
      arcFactor( head, RightAtt, dep )(*,::) :* outsideHeads( i )( j )( NoValence )
    )

    val leftwardMessage = factorAndOutside :* insideM( k )( j )( PlainM ).t
    outsideHeads( k )( j )( NoValence ) :+=
      sum( leftwardMessage(*,::) )

    // because we transposed PlainM, heads vary across columns for Right as well
    val rightwardMessage = factorAndOutside
    rightwardMessage( ::, * ) :*= insideHeads( i )( k )( NoValence )

    outsideM( i )( k )( PlainM ) :+=
      rightwardMessage
  }

  def outsideRightWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( i )
    val dep = intString( k )

    // println( arcFactor( head, RightAtt, dep ) )
    // println( "\n\n" )

    val factorAndOutside = 
      arcFactor( head, RightAtt, dep )(*,::) :* outsideHeads( i )( j )( NoValence )

    val rightwardMessage = factorAndOutside :* insideM( i )( k )( PlainM ).t
    outsideHeads( k )( j )( NoValence ) :+=
      sum( rightwardMessage(*,::) )

    val leftwardMessage = factorAndOutside
    leftwardMessage( ::, * ) :*= insideHeads( k )( j )( NoValence )

    outsideM( i )( k )( PlainM ) :+= leftwardMessage.t

    // val marginal = (
    //   insideM( i )( k )( PlainM )( *,:: ) :* insideHeads( k )( j )( NoValence )
    // ).t
    //   // (insideM( i )( k )( PlainM ).t)( ::, * ) :* insideHeads( k )( j )( NoValence )

    // marginal :*= factorAndOutside

    val marginal =
      (
        insideHeads(k)(j)(NoValence) *
        outsideHeads(i)(j)(NoValence).t
      ) :* arcFactor( head, RightAtt, dep ) :* insideM(i)(k)(PlainM).t

    // val marginal = factorAndOutside :* insideM( i )( k )( PlainM )
    // marginal( *, :: ) :*= insideHeads( k )( j )( NoValence )

    val headStopMarginal = sum( marginal( ::,* ) ).t.asDenseMatrix
    val depStopMarginal = sum( marginal( *,:: ) ).toDenseMatrix

    Seq(
      ( ChooseEvent( head, RightAtt, dep ) , marginal.t ),
      ( StopEvent( head, RightAtt, NoValence, NotStop ) , headStopMarginal ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ) , depStopMarginal ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ) , depStopMarginal )
    )
  }

  def outsideM( i:Int, k:Int, j:Int ) {

    outsideHeads( i )( k )( NoValence ) :+=
      (
        insideHeads( k )( j )( NoValence ).t *
          outsideM( i )( j )( PlainM ).t
      ).t

    outsideHeads( k )( j )( NoValence ) :+=
      (
        insideHeads( i )( k )( NoValence ).t *
          outsideM( i )( j )( PlainM )
      ).t
  }

  def viterbiLexFill( index:Int ) {
    insideHeads(index)(index+1)( NoValence ) = DenseVector.ones( uposCount )
    (0 until uposCount ).foreach{ hPos =>
      headTrace(index)(index+1) += ( NoValence, hPos ) -> LexEntry( index )
    }
  }

  def viterbiLeftRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( j )
    ( (i+1) to (j-2) by 2 ).map{ k =>
      val dep = intString( k )

      val scores = insideM( k )( j )( PlainM ) :* arcFactor( head, LeftAtt, dep )

      scores( ::, * ) :*= insideHeads( i )( k )( NoValence )

      SplitSpec(k,NoValence,NoValence) -> scores.toDenseMatrix
    }// .toMap
  }

  def viterbiRightRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( i )
    ( (i+2) to (j-1) by 2 ).map{ k =>
      val dep = intString( k )

      val scores = insideM( i )( k )( PlainM ).t :* arcFactor( head, RightAtt, dep )

      scores( ::, * ) :*= insideHeads( k )( j )( NoValence )

      SplitSpec(k,NoValence,NoValence) -> scores.toDenseMatrix
    }// .toMap
  }

  def viterbiMRank( i:Int, j:Int, decoration:MDecoration ) = {
    ( (i+1) to (j-1) by 2 ).map{ k =>
      SplitSpec( k, NoValence, NoValence )-> {
        insideHeads( i )( k )( NoValence ) *
          insideHeads( k )( j )( NoValence ).t
      }
    }
  }

  def viterbiRootRank = {
    (1 to (intString.length-1) by 2).map{ k =>
      val r = intString( k )

      SplitSpec( k, NoValence, NoValence ) -> {
        insideHeads(0)(k)(NoValence).toDenseMatrix.t :*
          insideHeads(k)(intString.length)(NoValence).toDenseMatrix.t :*
            rootFactor( k )
            // theta( RootEvent( r ) ) :*
            //   theta( StopEvent( r, LeftAtt, NoValence, Stop ) ).t :*
            //   theta( StopEvent( r, RightAtt, NoValence, Stop ) ).t
      }
    }
  }




}

