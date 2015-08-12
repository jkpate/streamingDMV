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

  // For the original DMV, valence is redundant with span indices
  val insideHeads:Array[Array[MMap[Decoration,Double]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      if( j-i > 1 )
        MMap( Outer -> 0D )
      else
        MMap( Innermost -> 0D )
    } else {
      MMap[Decoration,Double]()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val insideM:Array[Array[MMap[MDecoration,Double]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
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
    } else {
      MMap()
    }
  )
  // For the original DMV, valence is redundant with span indices
  val outsideHeads:Array[Array[MMap[Decoration,Double]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      if( j-i > 1 )
        MMap( Outer -> 0D )
      else
        MMap( Innermost -> 0D )
    } else {
      MMap[Decoration,Double]()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val outsideM:Array[Array[MMap[MDecoration,Double]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
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
  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) = headTrace( i )( k )( decoration.left )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) = headTrace( k )( j )( decoration.right )

  def lexFill( index:Int ) {
    val head = intString( index )
    insideHeads(index)(index+1)( Innermost ) = 1D
  }

  def outsideRoot( k:Int ) {
    val obs = intString( k )

    val leftV = adj( 0, k )
    val rightV = adj( k, intString.length )
    val factor =
      theta( RootEvent( obs ) ) *
        theta( StopEvent( obs, LeftAtt, leftV, Stop ) ) *
        theta( StopEvent( obs, RightAtt, rightV, Stop ) )

    outsideHeads( 0 )( k )( leftV ) += 
      insideHeads( k )( intString.length )( rightV ) * factor

    outsideHeads( k )( intString.length )( rightV ) += 
      insideHeads( 0 )( k )( leftV ) * factor
  }

  def outsideLeft( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )

    val cDV = adj( k, i )

    val valences = mValences( j-k )

    // Only do this calculation once for this loop
    val outsideChooseAndDepStop = 
      theta( ChooseEvent( head, LeftAtt, dep ) ) *
        theta( StopEvent( dep, LeftAtt, cDV, Stop ) ) *
          outsideHeads( i )( j )( Outer )

    valences.foreach{ vs =>
      val DecorationPair( mDV, hV ) = vs
      val factorAndOutside =
        theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
          theta( StopEvent( dep, RightAtt, mDV, Stop ) ) *
            outsideChooseAndDepStop

      // First, send messages to left child -- that is, the leftward looking dependent
      // label.
      outsideHeads( i )( k )( cDV ) +=
          insideM( k )( j )( vs ) * factorAndOutside

      // Now, send messages to right child -- that is, the M-label
      outsideM( k )( j )( vs ) +=
          insideHeads( i )( k )( cDV ) * factorAndOutside
    }
  }

  def outsideRight( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    val cDV = adj( j, k )

    // wonky type inference with the scala compiler
    val valences = mValences( k-i )

    // Only do this calculation once for this loop
    val outsideChooseAndDepStop = 
      theta( ChooseEvent( head, RightAtt, dep ) ) *
        theta( StopEvent( dep, RightAtt, cDV, Stop ) ) *
          outsideHeads( i )( j )( Outer )

    valences.foreach{ vs =>
      val DecorationPair(hV, mDV) = vs
      val factorAndOutside =
        theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
          theta( StopEvent( dep, LeftAtt, mDV, Stop ) ) *
            outsideChooseAndDepStop

      // First, send messages to right child -- that is, the rightward looking dependent
      // label.
      outsideHeads( k )( j )( cDV ) +=
          insideM( i )( k )( DecorationPair(hV, mDV) ) * factorAndOutside

      // Now, send messages to left child -- that is, the M-label
      outsideM( i )( k )( DecorationPair(hV, mDV) ) +=
          insideHeads( k )( j )( cDV ) * factorAndOutside
    }
  }

  def outsideM( i:Int, k:Int, j:Int ) {
    // everyone's valence is determined by the split
    val leftV = adj( k, i )
    val rightV = adj( j, k )

    val vs = DecorationPair(leftV, rightV)

    outsideHeads( i )( k )( leftV ) +=
      outsideM( i )( j )( vs ) *
        insideHeads( k )( j )( rightV )

    outsideHeads( k )( j )( rightV ) +=
      outsideM( i )( j )( vs ) *
        insideHeads( i )( k )( leftV )
  }


  def lexMarginals( index:Int ) = Seq[Tuple2[Event,Double]]()
  def outsideRootWithMarginals( k:Int ) = {
    val obs = intString( k )

    val leftV = adj( 0, k )
    val rightV = adj( k, intString.length )
    val factor =
      theta( RootEvent( obs ) ) *
        theta( StopEvent( obs, LeftAtt, leftV, Stop ) ) *
        theta( StopEvent( obs, RightAtt, rightV, Stop ) )

    outsideHeads( 0 )( k )( leftV ) += 
      insideHeads( k )( intString.length )( rightV ) * factor

    outsideHeads( k )( intString.length )( rightV ) += 
      insideHeads( 0 )( k )( leftV ) * factor

    val marginal = 
      insideHeads( 0 )( k )( leftV ) *
        insideHeads( k )( intString.length )( rightV ) *
          factor
    Seq[Tuple2[Event,Double]](
      ( RootEvent( obs ) , marginal ),
      ( StopEvent( obs, LeftAtt, leftV, Stop ) , marginal ),
      ( StopEvent( obs, RightAtt, rightV, Stop ) , marginal )
    )

  }

  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( j )
    if( j-i >= 3 ) {
      // this is an Arc cell -- compute outside probs for children and return arc marginals

      val dep = intString( k )
      val cDV = adj( k, i )

      // wonky type inference with the scala compiler
      val valences = mValences( j-k )

      valences.map{ vs =>
        val DecorationPair( mDV, hV ) = vs
        val factorAndOutside =
          theta( ChooseEvent( head, LeftAtt, dep ) ) *
          theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
            theta( StopEvent( dep, RightAtt, mDV, Stop ) ) *
            theta( StopEvent( dep, LeftAtt, cDV, Stop ) ) *
              outsideHeads( i )( j )( Outer )

        // First, send messages to left child -- that is, the leftward-looking dependent
        // label.
        outsideHeads( i )( k )( cDV ) +=
            insideM( k )( j )( vs ) * factorAndOutside

        // Now, send messages to right child -- that is, the M-label
        outsideM( k )( j )( vs ) +=
            insideHeads( i )( k )( cDV ) * factorAndOutside

        val marginal = 
          insideHeads( i )( k )( cDV ) *
            insideM( k )( j )( vs ) *
              factorAndOutside

        Seq[Tuple2[Event,Double]](
          ( ChooseEvent( head, LeftAtt, dep ) , marginal ),
          ( StopEvent( head, LeftAtt, hV, NotStop ) , marginal ),
          ( StopEvent( dep, RightAtt, mDV, Stop ) , marginal ),
          ( StopEvent( dep, LeftAtt, cDV, Stop ) , marginal )
        )
      }.reduce( _++_ )
    } else {
      // this is a (pre-)terminal cell -- do nothing
      Seq()
    }
  }

  def outsideRightWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( i )
    if( j-i >= 3 ) {
      // this is an Arc cell -- compute outside probs for children and return arc marginals

      val dep = intString( k )
      val cDV = adj( j, k )

      // wonky type inference with the scala compiler
      val valences = mValences( k-i )

      valences.map{ vs  =>
        val DecorationPair( hV, mDV ) = vs
        val factorAndOutside =
          theta( ChooseEvent( head, RightAtt, dep ) ) *
          theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
            theta( StopEvent( dep, LeftAtt, mDV, Stop ) ) *
            theta( StopEvent( dep, RightAtt, cDV, Stop ) ) *
            outsideHeads( i )( j )( Outer )

        // First, send messages to right child -- that is, the rightward-looking dependent
        // label.
        outsideHeads( k )( j )( cDV ) +=
            insideM( i )( k )( vs ) * factorAndOutside

        // Now, send messages to right child -- that is, the M-label
        outsideM( i )( k )( vs ) +=
            insideHeads( k )( j )( cDV ) * factorAndOutside

        val marginal = 
          insideHeads( k )( j )( cDV ) *
            insideM( i )( k )( vs ) *
              factorAndOutside

        Seq[Tuple2[Event,Double]](
          ( ChooseEvent( head, RightAtt, dep ) , marginal ),
          ( StopEvent( head, RightAtt, hV, NotStop ) , marginal ),
          ( StopEvent( dep, LeftAtt, mDV, Stop ) , marginal ),
          ( StopEvent( dep, RightAtt, cDV, Stop ) , marginal )
        )
      }.reduce(_++_)
    } else {
      // this is a (pre-)terminal cell -- do nothing
      Seq()
    }
  }

  def populateRightwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    val cDV = adj( j, k )

    val valences = mValences( k-i )

    valences.foreach{ vs =>
      val DecorationPair( hV, mDV ) = vs
      insideHeads( i )( j )( Outer ) +=
        insideHeads( k )( j )( cDV ) *
          insideM( i )( k )( vs ) *
            theta( ChooseEvent( head, RightAtt, dep ) ) *
            theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
              theta( StopEvent( dep, LeftAtt, mDV, Stop ) ) *
              theta( StopEvent( dep, RightAtt, cDV, Stop ) )
    }
  }

  val longSpanVs = Set[MDecoration](
    DecorationPair(Innermost,Outer),
    DecorationPair(Outer,Outer),
    DecorationPair(Outer,Innermost)
  )
  val midSpanVs = Set[MDecoration](
    DecorationPair(Innermost,Outer),
    DecorationPair(Outer,Innermost)
  )
  val shortSpanVs = Set[MDecoration]( DecorationPair(Innermost,Innermost))

  def mValences( span:Int ):Set[MDecoration] =
    if( span >= 6 )
      longSpanVs
    else if( span == 4 )
      midSpanVs
    else // span == 2
      shortSpanVs

  def populateLeftwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )

    val cDV = adj( i, k )

    val valences = mValences( j-k )

    valences.foreach{ vs =>
      val DecorationPair( mDV, hV ) = vs
      insideHeads( i )( j )( Outer ) +=
        insideHeads( i )( k )( cDV ) *
          insideM( k )( j )( vs ) *
            theta( ChooseEvent( head, LeftAtt, dep ) ) *
            theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
              theta( StopEvent( dep, RightAtt, mDV, Stop ) ) *
              theta( StopEvent( dep, LeftAtt, cDV, Stop ) )
    }
  }

  def populateMCell( i:Int, k:Int, j:Int ) {
    val leftV = adj( i, k )
    val rightV = adj( k, j )

    insideM( i )( j )( DecorationPair(leftV, rightV) ) +=
      insideHeads( i )( k )( leftV ) *
        insideHeads( k )( j )( rightV )
  }

  def populateRootCell( k:Int ) {
    val r = intString( k )

    val leftV = adj( 0, k )
    val rightV = adj( k, intString.length )

    stringProb +=
      insideHeads(0)(k)(leftV) *
        insideHeads(k)(intString.length)(rightV) *
          theta( RootEvent( r ) ) *
          theta( StopEvent( r, LeftAtt, leftV, Stop ) ) *
          theta( StopEvent( r, RightAtt, rightV, Stop ) )
  }

  def viterbiLexFill( index:Int ) {
    insideHeads(index)(index+1)( Innermost ) = 1D
    headTrace(index)(index+1) += Innermost -> LexEntry( index )
  }

  def leftArcParentVs( i:Int ) = Set[Decoration]( Outer )
  def rightArcParentVs( j:Int ) = Set[Decoration]( Outer )

  def mNodeParentVs( i:Int, j:Int ) = mValences( j-i )

  def viterbiRightRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( i )
    ( (i+2) to (j-1) by 2 ).flatMap{ k =>
      val cDV = adj( k, j )
      val dep = intString( k )
      val valences = mValences( k-i )

      valences.map{ vs =>
        val DecorationPair(hV, mDV) = vs
        SplitSpec(k,hV,mDV) -> {
          insideHeads( k )( j )( cDV ) *
            insideM( i )( k )( vs ) *
              theta( ChooseEvent( head, RightAtt, dep ) ) *
              theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
              theta( StopEvent( dep, LeftAtt, mDV, Stop ) ) *
              theta( StopEvent( dep, RightAtt, cDV, Stop ) )
        }
      }
    }
  }

  def viterbiLeftRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( j )
    ( (i+1) to (j-2) by 2 ).flatMap{ k =>
      val cDV = adj( i, k )
      val dep = intString( k )
      val valences = mValences( j-k )
      valences.map{ vs =>
        val DecorationPair(mDV, hV) = vs
        SplitSpec(k,mDV,hV) -> {
          insideHeads( i )( k )( cDV ) *
            insideM( k )( j )( vs ) *
              theta( ChooseEvent( head, LeftAtt, dep ) ) *
              theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
              theta( StopEvent( dep, RightAtt, mDV, Stop ) ) *
              theta( StopEvent( dep, LeftAtt, cDV, Stop ) )
        }
      }
    }
  }

  def viterbiMRank( i:Int, j:Int, decoration:MDecoration ) = {

    val DecorationPair( leftV, rightV ) = decoration

    // avoid looping over indices inconsistent with the valence
    val indices =
      if( leftV == Innermost )
        ( i+1 to i+1)
      else if( rightV == Innermost )
        ( j-1 to j-1)
      else
        ( (i+3) to (j-3) by 2 )

    // ( (i+1) to (j-1) by 2 )
    indices.map{ k =>
      SplitSpec( k, leftV, rightV )-> {
        insideHeads( i )( k )( leftV ) *
          insideHeads( k )( j )( rightV )
      }
    }
  }

  def viterbiRootRank = {
    (1 to (intString.length-1) by 2).map{ k =>
      val r = intString( k )
      val leftV = adj( 0, k )
      val rightV = adj( k, intString.length )

      SplitSpec( k, leftV, rightV ) -> {
        insideHeads(0)(k)(leftV) *
          insideHeads(k)(intString.length)(rightV) *
            theta( RootEvent( r ) ) *
            theta( StopEvent( r, LeftAtt, leftV, Stop ) ) *
            theta( StopEvent( r, RightAtt, rightV, Stop ) )
      }
    }
  }


}

