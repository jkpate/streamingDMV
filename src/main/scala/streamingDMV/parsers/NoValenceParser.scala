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

  // val PlainM = DecorationPair(NoValence, NoValence)

  val insideHeads:Array[Array[MMap[Decoration,Double]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else {
      MMap[Decoration,Double]()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val insideM:Array[Array[MMap[MDecoration,Double]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> 0D )
    } else {
      MMap[MDecoration,Double]()
    }
  )
  // For the original DMV, valence is redundant with span indices
  val outsideHeads:Array[Array[MMap[Decoration,Double]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else {
      MMap[Decoration,Double]()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val outsideM:Array[Array[MMap[MDecoration,Double]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      MMap( PlainM -> 0D )
    } else {
      MMap()
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
    insideHeads(index)(index+1)( NoValence ) = 1D
  }

  def outsideRoot( k:Int ) {
    val obs = intString( k )

    val factor =
      theta( RootEvent( obs ) ) *
        theta( StopEvent( obs, LeftAtt, NoValence, Stop ) ) *
        theta( StopEvent( obs, RightAtt, NoValence, Stop ) )

    outsideHeads( 0 )( k )( NoValence ) += 
      insideHeads( k )( intString.length )( NoValence ) * factor

    outsideHeads( k )( intString.length )( NoValence ) += 
      insideHeads( 0 )( k )( NoValence ) * factor
  }

  def outsideLeft( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )

    // Only do this calculation once for this loop
    val outsideChooseAndDepStop = 
      theta( ChooseEvent( head, LeftAtt, dep ) ) *
        theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
          outsideHeads( i )( j )( NoValence )

    val factorAndOutside =
      theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
        theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
          outsideChooseAndDepStop

    // First, send messages to left child -- that is, the leftward looking dependent
    // label.
    outsideHeads( i )( k )( NoValence ) +=
        insideM( k )( j )( PlainM ) * factorAndOutside

    // Now, send messages to right child -- that is, the M-label
    outsideM( k )( j )( PlainM ) +=
        insideHeads( i )( k )( NoValence ) * factorAndOutside

  }

  def outsideRight( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    // Only do this calculation once for this loop
    val outsideChooseAndDepStop = 
      theta( ChooseEvent( head, RightAtt, dep ) ) *
        theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
          outsideHeads( i )( j )( NoValence )

    val factorAndOutside =
      theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
        theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
          outsideChooseAndDepStop

    // First, send messages to right child -- that is, the rightward looking dependent
    // label.
    outsideHeads( k )( j )( NoValence ) +=
        insideM( i )( k )( PlainM ) * factorAndOutside

    // Now, send messages to left child -- that is, the M-label
    outsideM( i )( k )( PlainM ) +=
        insideHeads( k )( j )( NoValence ) * factorAndOutside
  }

  def outsideM( i:Int, k:Int, j:Int ) {
    outsideHeads( i )( k )( NoValence ) +=
      outsideM( i )( j )( PlainM ) *
        insideHeads( k )( j )( NoValence )

    outsideHeads( k )( j )( NoValence ) +=
      outsideM( i )( j )( PlainM ) *
        insideHeads( i )( k )( NoValence )
  }

  def lexMarginals( index:Int ) = Seq()

  def outsideRootWithMarginals( k:Int ) = {
    val obs = intString( k )

    val factor =
      theta( RootEvent( obs ) ) *
        theta( StopEvent( obs, LeftAtt, NoValence, Stop ) ) *
        theta( StopEvent( obs, RightAtt, NoValence, Stop ) )

    outsideHeads( 0 )( k )( NoValence ) += 
      insideHeads( k )( intString.length )( NoValence ) * factor

    outsideHeads( k )( intString.length )( NoValence ) += 
      insideHeads( 0 )( k )( NoValence ) * factor

    val marginal = 
      insideHeads( 0 )( k )( NoValence ) *
        insideHeads( k )( intString.length )( NoValence ) *
          factor
    Seq(
      ( RootEvent( obs ) , marginal ),
      ( StopEvent( obs, LeftAtt, NoValence, Stop ) , marginal ),
      ( StopEvent( obs, RightAtt, NoValence, Stop ) , marginal )
    )

  }


  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( j )
    if( j-i >= 3 ) {
      // this is an Arc cell -- compute outside probs for children and return arc marginals

      val dep = intString( k )

      val factorAndOutside =
        theta( ChooseEvent( head, LeftAtt, dep ) ) *
        theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
          theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
          theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
            outsideHeads( i )( j )( NoValence )

      // First, send messages to left child -- that is, the leftward-looking dependent
      // label.
      outsideHeads( i )( k )( NoValence ) +=
          insideM( k )( j )( PlainM ) * factorAndOutside

      // Now, send messages to right child -- that is, the M-label
      outsideM( k )( j )( PlainM ) +=
          insideHeads( i )( k )( NoValence ) * factorAndOutside

      val marginal = 
        insideHeads( i )( k )( NoValence ) *
          insideM( k )( j )( PlainM ) *
            factorAndOutside

      Seq(
        ( ChooseEvent( head, LeftAtt, dep ) , marginal ),
        ( StopEvent( head, LeftAtt, NoValence, NotStop ) , marginal ),
        ( StopEvent( dep, RightAtt, NoValence, Stop ) , marginal ),
        ( StopEvent( dep, LeftAtt, NoValence, Stop ) , marginal )
      )
    } else {
      // this is a (pre-)terminal cell -- do nothing
      Seq()
    }
  }

  def outsideRightWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( i )
    // this is an Arc cell -- compute outside probs for children and return arc marginals

    val dep = intString( k )

    // wonky type inference with the scala compiler

    val factorAndOutside =
      theta( ChooseEvent( head, RightAtt, dep ) ) *
      theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
        theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
        theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
        outsideHeads( i )( j )( NoValence )

    // First, send messages to right child -- that is, the rightward-looking dependent
    // label.
    outsideHeads( k )( j )( NoValence ) +=
        insideM( i )( k )( PlainM ) * factorAndOutside

    // Now, send messages to right child -- that is, the M-label
    outsideM( i )( k )( PlainM ) +=
        insideHeads( k )( j )( NoValence ) * factorAndOutside

    val marginal = 
      insideHeads( k )( j )( NoValence ) *
        insideM( i )( k )( PlainM ) *
          factorAndOutside

    Seq(
      ( ChooseEvent( head, RightAtt, dep ) , marginal ),
      ( StopEvent( head, RightAtt, NoValence, NotStop ) , marginal ),
      ( StopEvent( dep, LeftAtt, NoValence, Stop ) , marginal ),
      ( StopEvent( dep, RightAtt, NoValence, Stop ) , marginal )
    )
  }

  def arcFactor( head:Int, dir:AttDir, dep:Int ) = {
    theta( ChooseEvent( head, dir, dep ) ) *
    theta( StopEvent( head, dir, NoValence, NotStop ) ) *
      theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) *
      theta( StopEvent( dep, dir, NoValence, Stop ) )
  }

  def populateRightwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    insideHeads( i )( j )( NoValence ) +=
      insideHeads( k )( j )( NoValence ) *
        insideM( i )( k )( PlainM ) *
          arcFactor( head, RightAtt, dep )
          // theta( ChooseEvent( head, RightAtt, dep ) ) *
          // theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
          //   theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
          //   theta( StopEvent( dep, RightAtt, NoValence, Stop ) )
  }

  def populateLeftwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )


    insideHeads( i )( j )( NoValence ) +=
      insideHeads( i )( k )( NoValence ) *
        insideM( k )( j )( PlainM ) *
          arcFactor( head, LeftAtt, dep )
          // theta( ChooseEvent( head, LeftAtt, dep ) ) *
          // theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
          //   theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
          //   theta( StopEvent( dep, LeftAtt, NoValence, Stop ) )
  }

  def populateMCell( i:Int, k:Int, j:Int ) {
    insideM( i )( j )( PlainM ) +=
      insideHeads( i )( k )( NoValence ) *
        insideHeads( k )( j )( NoValence )
  }

  def populateRootCell( k:Int ) {
    val r = intString( k )

    stringProb +=
      insideHeads(0)(k)(NoValence) *
        insideHeads(k)(intString.length)(NoValence) *
          theta( RootEvent( r ) ) *
          theta( StopEvent( r, LeftAtt, NoValence, Stop ) ) *
          theta( StopEvent( r, RightAtt, NoValence, Stop ) )
  }


  def viterbiLexFill( index:Int ) {
    insideHeads(index)(index+1)( NoValence ) = 1D
    headTrace(index)(index+1) += NoValence -> LexEntry( index )
  }

  def leftArcParentVs( i:Int ) = Set[Decoration]( NoValence )
  def rightArcParentVs( j:Int ) = Set[Decoration]( NoValence )

  def mNodeParentVs( i:Int, j:Int ) = Set( PlainM )

  def viterbiRightRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( i )
    ( (i+2) to (j-1) by 2 ).map{ k =>
      val dep = intString( k )

      SplitSpec(k,NoValence,NoValence) -> {
        insideHeads( k )( j )( NoValence ) *
          insideM( i )( k )( PlainM ) *
            theta( ChooseEvent( head, RightAtt, dep ) ) *
            theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
            theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
            theta( StopEvent( dep, RightAtt, NoValence, Stop ) )
      }
    }.toMap
  }

  def viterbiLeftRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( j )
    ( (i+1) to (j-2) by 2 ).map{ k =>
      val dep = intString( k )

      SplitSpec(k,NoValence,NoValence) -> {
        insideHeads( i )( k )( NoValence ) *
          insideM( k )( j )( PlainM ) *
            theta( ChooseEvent( head, LeftAtt, dep ) ) *
            theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
            theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
            theta( StopEvent( dep, LeftAtt, NoValence, Stop ) )
      }
    }.toMap
  }

  def viterbiMRank( i:Int, j:Int, decoration:MDecoration ) = {
    ( (i+1) to (j-1) by 2 ).map{ k =>
      SplitSpec( k, NoValence, NoValence )-> {
        insideHeads( i )( k )( NoValence ) *
          insideHeads( k )( j )( NoValence )
      }
    }
  }

  def viterbiRootRank = {
    (1 to (intString.length-1) by 2).map{ k =>
      val r = intString( k )

      SplitSpec( k, NoValence, NoValence ) -> {
        insideHeads(0)(k)(NoValence) *
          insideHeads(k)(intString.length)(NoValence) *
            theta( RootEvent( r ) ) *
            theta( StopEvent( r, LeftAtt, NoValence, Stop ) ) *
            theta( StopEvent( r, RightAtt, NoValence, Stop ) )
      }
    }
  }



}

