package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.TopDownDMVParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class TopDownDMVParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FoldUnfoldParser[TopDownDMVParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  val theta = new TopDownDMVParameters( rootAlpha, stopAlpha, chooseAlpha )

  // any directed head can be outermost or not
  val insideHeads = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      MMap( Outermost -> 0D, Inner -> 0D )
    else
      MMap[Valence,Double]()
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val insideM = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 )
      MMap(
        // (Outermost,Outermost) -> 0D,
        (Outermost,Inner) -> 0D,
        (Inner,Outermost) -> 0D
      )
    else
      MMap[Tuple2[Valence,Valence],Double]()
  )
  // any directed head can be outermost or not
  val outsideHeads = Array.fill( 2*maxLength, (2*maxLength)+1 )(
    MMap( Outermost -> 0D, Inner -> 0D )
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val outsideM = Array.fill( 2*maxLength, 2*maxLength )(
    MMap(
      (Outermost,Outermost) -> 0D,
      (Outermost,Inner) -> 0D,
      (Inner,Outermost) -> 0D
    )
  )


  def findLeftRootChild( k:Int ) = headTrace( 0 )( k )( Outermost )
  def findRightRootChild( k:Int ) = headTrace( k )( intString.length )( Outermost )

  def findLeftLeftwardChild( i:Int, k:Int ) = headTrace( i )( k )( Outermost )
  def findRightLeftwardChild( k:Int, j:Int, hV:Valence, mDV:Valence ) =
    mTrace( k )( j )( ( Outermost, Inner ) )
  def findLeftRightwardChild( i:Int, k:Int, hV:Valence, mDV:Valence ) =
    mTrace( i )( k )( ( Inner, Outermost ) )
  def findRightRightwardChild( k:Int, j:Int ) = headTrace( k )( j )( ( Outermost ) )
  def findLeftMChild( i:Int, k:Int, leftV:Valence ) = headTrace( i )( k )( leftV )
  def findRightMChild( k:Int, j:Int, rightV:Valence ) = headTrace( k )( j )( rightV )

  def lexFill( index:Int ) {
    val head = intString( index )
    if( index %2 == 0 ) {
      insideHeads(index)(index+1)( Outermost ) =
        theta( StopEvent( head, LeftAtt, Outermost, Stop ) )

      if( index > 0 )
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, LeftAtt, Inner, Stop ) )
    } else {

      insideHeads(index)(index+1)( Outermost ) =
        theta( StopEvent( head, RightAtt, Outermost, Stop ) )

      if( index < intString.length-1 )
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, RightAtt, Inner, Stop ) )
    }
  }

  def populateRightwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    // hV must be Outermost if right edge index is string length
    val hVs = if( j == intString.length ) Set( Outermost ) else Set( Outermost, Inner )

    hVs.foreach{ hV =>
      insideHeads( i )( j )( hV ) +=
        insideHeads( k )( j )( Outermost ) *
          insideM( i )( k )( (Inner, Outermost) ) * // head child is always Inner
            theta( ChooseEvent( head, RightAtt, dep ) ) *
            theta( StopEvent( head, RightAtt, hV, NotStop ) )
    }
  }

  def populateLeftwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )

    // hV must be Outermost if left edge index is 0
    val hVs = if( i == 0 ) Set( Outermost ) else Set( Outermost, Inner )

    hVs.foreach{ hV =>
      insideHeads( i )( j )( hV ) +=
        insideHeads( i )( k )( Outermost ) *
          insideM( k )( j )( (Outermost, Inner) ) * // head child is always Inner
            theta( ChooseEvent( head, LeftAtt, dep ) ) *
            theta( StopEvent( head, LeftAtt, hV, NotStop ) )
    }
  }

  def populateMCell( i:Int, k:Int, j:Int ) {
    // We can't have both children be Outermost or Inner -- one must be the dependent, one must
    // be the head child.
    insideM( i )( j )( Outermost, Inner ) +=
      insideHeads( i )( k )( Outermost ) *
        insideHeads( k )( j )( Inner )

    insideM( i )( j )( Inner, Outermost ) +=
      insideHeads( i )( k )( Inner ) *
        insideHeads( k )( j )( Outermost )
  }

  def populateRootCell( k:Int ) {
    val r = intString( k )

    stringProb +=
      insideHeads(0)(k)(Outermost) *
        insideHeads(k)(intString.length)(Outermost) *
          theta( RootEvent( r ) )
  }


  def rightArcParentVs( j:Int ) =
    if( j == intString.length-1 )
      Set[Valence]( Outermost )
    else
      Set[Valence]( Outermost, Inner )
  def leftArcParentVs( i:Int ) =
    if( i == 0 )
      Set[Valence]( Outermost )
    else
      Set[Valence]( Outermost, Inner )

  val mValences =
    Set(
      ( Inner, Outermost ),
      ( Outermost, Inner )
    )
  def mNodeParentVs( i:Int, j:Int ) = mValences

  def viterbiLexFill( index:Int ) {
    val head = intString( index )
    if( index %2 == 0 ) {
      insideHeads(index)(index+1)( Outermost ) =
        theta( StopEvent( head, LeftAtt, Outermost, Stop ) )
      headTrace(index)(index+1) += Outermost -> LexEntry( index )

      if( index > 0 ) {
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, LeftAtt, Inner, Stop ) )
        headTrace(index)(index+1) += Inner -> LexEntry( index )
      }
    } else {
      insideHeads(index)(index+1)( Outermost ) =
        theta( StopEvent( head, RightAtt, Outermost, Stop ) )
      headTrace(index)(index+1) += Outermost -> LexEntry( index )

      if( index < intString.length-1 ) {
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, RightAtt, Inner, Stop ) )
        headTrace(index)(index+1) += Inner -> LexEntry( index )
      }
    }
  }


  def viterbiRightRank( i:Int, j:Int, parentV:Valence ) = {
    val head = intString( i )
    ( (i+2) to (j-1) by 2 ).map{ k =>
      val dep = intString( k )
      // For top down parser, head child is always Inner, dependent children are always Outer
      SplitSpec(k,Inner,Outermost) -> {
        insideHeads( k )( j )( Outermost ) *
          insideM( i )( k )( (Inner, Outermost) ) *
            theta( ChooseEvent( head, RightAtt, dep ) ) *
            theta( StopEvent( head, RightAtt, parentV, NotStop ) )
      }
    }
  }

  def viterbiLeftRank( i:Int, j:Int, parentV:Valence ) = {
    val head = intString( j )
    ( (i+1) to (j-2) by 2 ).map{ k =>
      val dep = intString( k )
      // For top down parser, head child is always Inner, dependent children are always Outer
      SplitSpec( k, Outermost, Inner ) -> {
        insideHeads( i )( k )( Outermost ) *
          insideM( k )( j )( (Outermost, Inner) ) *
            theta( ChooseEvent( head, LeftAtt, dep ) ) *
            theta( StopEvent( head, LeftAtt, parentV, NotStop ) )
      }
    }
  }

  def viterbiMRank( i:Int, j:Int, leftV:Valence, rightV:Valence ) = {
  // def viterbiMRank( i:Int, j:Int ) = {
    ( (i+1) to (j-1) by 2 ).map{ k =>
      SplitSpec( k, leftV, rightV ) -> {
        insideHeads( i )( k )( leftV ) *
          insideHeads( k )( j )( rightV )
      }
    }
  }

  def viterbiRootRank = {
    (1 to (intString.length-1) by 2).map{ k =>
      val r = intString( k )

      SplitSpec( k, Outermost, Outermost ) -> {
        insideHeads(0)(k)(Outermost) *
          insideHeads(k)(intString.length)(Outermost) *
            theta( RootEvent( r ) )
      }
    }
  }


  def outsideRoot( k:Int ) {
    val obs = intString( k )

    val factor = theta( RootEvent( obs ) )

    outsideHeads( 0 )( k )( Outermost ) += 
      insideHeads( k )( intString.length )( Outermost ) * factor

    outsideHeads( k )( intString.length )( Outermost ) += 
      insideHeads( 0 )( k )( Outermost ) * factor
  }

  def outsideLeft( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )
    val hVs = if( j <= 3 ) Set( Outermost ) else Set( Outermost, Inner )

    hVs.foreach{ hV =>
      val factorAndOutside =
        theta( ChooseEvent( head, LeftAtt, dep ) ) *
          theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
            outsideHeads( i )( j )( hV )

      // First, send messages to left child -- that is, the leftward looking dependent
      // label.
      outsideHeads( i )( k )( Outermost ) +=
          insideM( k )( j )( (Outermost, Inner ) ) * factorAndOutside
      // Now, send messages to right child -- that is, the M-label

      outsideM( k )( j )( (Outermost, Inner) ) +=
          insideHeads( i )( k )( Outermost ) * factorAndOutside
    }
  }

  def outsideRight( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )
    val hVs = if( i >= ( intString.length-3) ) Set( Outermost ) else Set( Outermost, Inner )

    hVs.foreach{ hV =>
      val factorAndOutside =
        theta( ChooseEvent( head, RightAtt, dep ) ) *
          theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
            outsideHeads( i )( j )( hV )

      // First, send messages to left child -- that is, the leftward looking dependent
      // label.
      outsideHeads( k )( j )( Outermost ) +=
          insideM( i )( k )( (Inner, Outermost) ) * factorAndOutside

      // Now, send messages to right child -- that is, the M-label
      outsideM( i )( k )( (Inner, Outermost) ) +=
          insideHeads( k )( j )( Outermost ) * factorAndOutside
    }
  }

  def outsideM( i:Int, k:Int, j:Int ) {
    mValences.foreach{ vs =>
      val (leftV, rightV) = vs
      val outM = outsideM( i )( j )( vs )

      outsideHeads( i )( k )( leftV ) += outM * insideHeads( k )( j )( rightV )
      outsideHeads( k )( j )( rightV ) += outM * insideHeads( i )( k )( leftV )
    }
  }

  def outsideRootWithMarginals( k:Int ) = {
    val obs = intString( k )

    val factor = theta( RootEvent( obs ) )

    outsideHeads( 0 )( k )( Outermost ) += 
      insideHeads( k )( intString.length )( Outermost ) * factor

    outsideHeads( k )( intString.length )( Outermost ) += 
      insideHeads( 0 )( k )( Outermost ) * factor

    Map[Event,Double](
      RootEvent( obs ) ->
      insideHeads( 0 )( k )( Outermost ) *
        insideHeads( k )( intString.length )( Outermost ) *
          factor
    )

  }

  def lexMarginals( index:Int ) = {
    val head = intString( index )
    if( index%2 == 0 ) {
      Seq( Outermost, Inner ).map{ hV =>
        StopEvent( head, LeftAtt, hV, Stop ) ->
          insideHeads( index )( index +1 )( hV ) * outsideHeads( index )( index +1 )( hV )
      }.toMap
    } else {
      Seq( Outermost, Inner ).map{ hV =>
        StopEvent( head, RightAtt, hV, Stop ) ->
          insideHeads( index )( index +1 )( hV ) * outsideHeads( index )( index +1 )( hV )
      }.toMap
    }
  }

  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( j )
    if( j-i >= 3 ) {
      // this is an Arc cell -- compute outside probs for children and return arc marginals

      val dep = intString( k )
      val hVs = if( j <= 3 ) Set( Outermost ) else Set( Outermost, Inner )
      hVs.flatMap{ hV =>
        val factorAndOutside =
          theta( ChooseEvent( head, LeftAtt, dep ) ) *
            theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
              outsideHeads( i )( j )( hV )

        // First, send messages to left child -- that is, the leftward looking dependent
        // label.
        outsideHeads( i )( k )( Outermost ) +=
            insideM( k )( j )( (Outermost, Inner ) ) * factorAndOutside
        // Now, send messages to right child -- that is, the M-label

        outsideM( k )( j )( (Outermost, Inner) ) +=
            insideHeads( i )( k )( Outermost ) * factorAndOutside

        val marginal = 
          insideHeads( i )( k )( Outermost ) *
            insideM( k )( j )( (Outermost, Inner ) ) *
              factorAndOutside

        Seq(
          ChooseEvent( head, LeftAtt, dep ) -> marginal,
          StopEvent( head, LeftAtt, hV, NotStop ) -> marginal
        )
      }
    } else {
      // this is a (pre-)terminal cell -- just return stop marginals
      Seq( Outermost, Inner ).map{ hV =>
        StopEvent( head, LeftAtt, hV, Stop ) ->
          insideHeads( i )( j )( hV ) * outsideHeads( i )( j )( hV )
      }
    }
  }.toMap


  def outsideRightWithMarginals( i:Int, k:Int, j:Int ) = {
    val head = intString( i )
    if( j-i >= 3 ) {
      // this is an Arc cell -- compute outside probs for children and return arc marginals

      val dep = intString( k )
      val hVs = if( i >= intString.length-3 ) Set( Outermost ) else Set( Outermost, Inner )
      hVs.flatMap{ hV =>
        val factorAndOutside =
          theta( ChooseEvent( head, RightAtt, dep ) ) *
            theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
              outsideHeads( i )( j )( hV )

        // First, send messages to right child -- that is, the rightward looking dependent
        // label.
        outsideHeads( k )( j )( Outermost ) +=
            insideM( i )( k )( (Outermost, Inner ) ) * factorAndOutside

        // Now, send messages to left child -- that is, the M-label
        outsideM( i )( k )( (Outermost, Inner) ) +=
            insideHeads( k )( j )( Outermost ) * factorAndOutside

        val marginal = 
          insideHeads( k )( j )( Outermost ) *
            insideM( i )( k )( (Outermost, Inner ) ) *
              factorAndOutside

        Seq(
          ChooseEvent( head, RightAtt, dep ) -> marginal,
          StopEvent( head, RightAtt, hV, NotStop ) -> marginal
        )
      }
    } else {
      // this is a (pre-)terminal cell -- just return stop marginals
      Seq( Outermost, Inner ).map{ hV =>
        StopEvent( head, RightAtt, hV, Stop ) ->
          insideHeads( i )( j )( hV ) * outsideHeads( i )( j )( hV )
      }
    }
  }.toMap

}


// vim: set ts=2 sw=2 et:
