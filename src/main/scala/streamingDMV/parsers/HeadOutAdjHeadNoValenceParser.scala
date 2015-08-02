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
) extends FoldUnfoldParser[HeadOutAdjHeadNoValenceParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  val theta = new HeadOutAdjHeadNoValenceParameters( rootAlpha, stopAlpha, chooseAlpha )


  val insideHeads:Array[Array[MMap[Decoration,Double]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else {
      MMap()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val insideM:Array[Array[MMap[MDecoration,Double]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      MMap(
        PlainM -> 0D,
        LeftwardM -> 0D,
        RightwardM -> 0D
      )
    } else {
      MMap()
    }
  )
  // For the original DMV, valence is redundant with span indices
  val outsideHeads:Array[Array[MMap[Decoration,Double]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    if( ( i%2 != j%2 ) ) {
      MMap( NoValence -> 0D )
    } else {
      MMap()
    }
  )
  // One of the m-node children is a head-child, so they can't both be outermost
  val outsideM:Array[Array[MMap[MDecoration,Double]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    if( i%2 == 1 && j%2 == 1 ) {
      MMap(
        PlainM -> 0D,
        LeftwardM -> 0D,
        RightwardM -> 0D
      )
    } else {
      MMap()
    }
  )


  def findLeftRootChild( k:Int ) = headTrace( 0 )( k )( NoValence )
  def findRightRootChild( k:Int ) =
    headTrace( k )( intString.length )( NoValence )

  def findLeftLeftwardChild( i:Int, k:Int ) = {
    assert( k-i > 1 )
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
    assert( j-k > 1 )
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

    Map[Event,Double](
      RootEvent( obs ) -> marginal,
      StopEvent( obs, LeftAtt, NoValence, Stop ) -> marginal,
      StopEvent( obs, RightAtt, NoValence, Stop ) -> marginal
    )
  }

  def outsideLeft( i:Int, k:Int, j:Int ) {
    // index handling for L and R nodes is the same as in the first-order grammar
    val head = intString( j )
    val dep = intString( k )

    val factor =
      theta( SecondOrderChooseEvent( head, LeftAtt, dep ) ) *
        theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
          theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
          theta( StopEvent( dep, RightAtt, NoValence, Stop ) )

    val factorAndOutside =
      factor * outsideHeads( i )( j )( NoValence )

    // First, send messages to left child -- that is, the leftward-looking dependent label.
    outsideHeads( i )( k )( NoValence ) +=
      insideM( k )( j )( LeftwardM ) * factorAndOutside

    // First, send messages to rightward child -- that is, the leftward-looking M-label.
    outsideM( k )( j )( LeftwardM ) +=
        insideHeads( i )( k )( NoValence ) * factorAndOutside

    val marginal = 
      insideHeads( 0 )( k )( NoValence ) *
        insideHeads( k )( intString.length )( NoValence ) *
          factor
  }

  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) {
    val head = intString( j )
    val dep = intString( k )

    val factorAndOutside =
      theta( ChooseEvent( head, LeftAtt, dep ) ) *
        theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
          theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
          theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
            outsideHeads( i )( j )( NoValence )

    // First, send messages to left child -- that is, the leftward-looking dependent label.
    outsideHeads( i )( k )( NoValence ) +=
      insideM( k )( j )( LeftwardM ) * factorAndOutside

    // First, send messages to rightward child -- that is, the leftward-looking M-label.
    outsideM( k )( j )( LeftwardM ) +=
        insideHeads( i )( k )( NoValence ) * factorAndOutside

    val marginal = 
      insideHeads( i )( k )( NoValence ) *
        insideM( k )( j )( LeftwardM ) *
          factorAndOutside

    Map[Event,Double](
      ChooseEvent( head, LeftAtt, dep ) -> marginal,
      StopEvent( head, LeftAtt, NoValence, NotStop ) -> marginal,
      StopEvent( dep, RightAtt, NoValence, Stop ) -> marginal,
      StopEvent( dep, LeftAtt, NoValence, Stop ) -> marginal
    )
  }

  def outsideRight( i:Int, k:Int, j:Int ) {
    // index handling for L and R nodes is the same as in the first-order grammar
    val head = intString( i )
    val dep = intString( k )

    val factorAndOutside =
      theta( ChooseEvent( head, RightAtt, dep ) ) *
        theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
          theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
          theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
            outsideHeads( i )( j )( NoValence )

    // First, send messages to right child -- that is, the rightward-looking dependent label.
    outsideHeads( k )( j )( NoValence ) +=
      insideM( i )( k )( RightwardM ) * factorAndOutside

    outsideM( i )( k )( RightwardM ) +=
        insideHeads( k )( j )( NoValence ) * factorAndOutside
  }

  val mDecorations:Set[MDecoration] = Set( PlainM, LeftwardM, RightwardM )
  def mNodeParentVs( i:Int, j:Int ) = mDecorations
  def outsideM( i:Int, k:Int, j:Int ) {
    if( k%2 == 0 ) { // only Directed children
      mDecorations.foreach{ parentM =>
        outsideHeads( i )( k )( NoValence ) +=
          outsideM( i )( j )( parentM ) *
            insideHeads( k )( j )( NoValence )

        outsideHeads( k )( j )( NoValence ) +=
          outsideM( i )( j )( parentM ) *
            insideHeads( i )( k )( NoValence )
      }
    } else { // parent is DirectedM
      // Leftward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val lHead = s(j)
      val context = s(k) // switch context and lDep for top-down second-order
      val lDep = s(i)

      val lFactorAndOutside =
        theta( SecondOrderChooseEvent( lHead, context, LeftAtt, lDep ) *
          theta( StopEvent( lHead, LeftAtt, NoValence, NoStop ) ) *
            theta( StopEvent( lDep, RightAtt, NoValence, Stop ) ) *
            theta( StopEvent( lDep, LeftAtt, NoValence, Stop ) ) *
              outsideM( i )( j )( LeftwardM )
      // message to left child, a PlainM node
      outsideM( i )( k )( PlainM ) +=
        insideM( k )( j )( LeftwardM ) * lFactorAndOutside

      // message to right child, a LeftwardM node
      outsideM( k )( j )( LeftwardM ) +=
        insideM( i )( k )( PlainM ) * lFactorAndOutside

      // Rightward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val rHead = s(i)
      val context = s(k)
      val rDep = s(j)

      val rFactorAndOutside =
        theta( SecondOrderChooseEvent( rHead, context, RightAtt, rDep ) *
          theta( StopEvent( rHead, RightAtt, NoValence, NoStop ) ) *
            theta( StopEvent( rDep, LeftAtt, NoValence, Stop ) ) *
            theta( StopEvent( rDep, RightAtt, NoValence, Stop ) ) *
              outsideM( i )( j )( RightwardM )
      // message to right child, a PlainM node
      outsideM( k )( j )( PlainM ) +=
        insideM( i )( k )( RightwardM ) * rFactorAndOutside

      // message to left child, a RightwardM node
      outsideM( i )( k )( RightwardM ) +=
        insideM( k )( j )( PlainM ) *
          outsideM( i )( j )( RightwardM ) * rFactorAndOutside
    }
  }

  def outsideMWithMarginal( i:Int, k:Int, j:Int ) = {
    if( k%2 == 0 ) { // only Directed children
      mDecorations.foreach{ parentM =>
        outsideHeads( i )( k )( NoValence ) +=
          outsideM( i )( j )( parentM ) *
            insideHeads( k )( j )( NoValence )

        outsideHeads( k )( j )( NoValence ) +=
          outsideM( i )( j )( parentM ) *
            insideHeads( i )( k )( NoValence )
      }
      MMap()
    } else { // parent is DirectedM
      // Leftward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val lHead = s(j)
      val context = s(k) // switch context and lDep for top-down second-order
      val lDep = s(i)

      val lFactor = 
        theta( SecondOrderChooseEvent( lHead, context, LeftAtt, lDep ) *
          theta( StopEvent( lHead, LeftAtt, NoValence, NoStop ) ) *
            theta( StopEvent( lDep, RightAtt, NoValence, Stop ) ) *
            theta( StopEvent( lDep, LeftAtt, NoValence, Stop ) )
      val lFactorAndOutside =
              outsideM( i )( j )( LeftwardM ) * lFactor
      // message to left child, a PlainM node
      outsideM( i )( k )( PlainM ) +=
        insideM( k )( j )( LeftwardM ) * lFactorAndOutside

      // message to right child, a LeftwardM node
      outsideM( k )( j )( LeftwardM ) +=
        insideM( i )( k )( PlainM ) * lFactorAndOutside

      // Rightward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val rHead = s(i)
      val context = s(k)
      val rDep = s(j)

      val rFactor =
        theta( SecondOrderChooseEvent( rHead, context, RightAtt, rDep ) *
          theta( StopEvent( rHead, RightAtt, NoValence, NoStop ) ) *
            theta( StopEvent( rDep, LeftAtt, NoValence, Stop ) ) *
            theta( StopEvent( rDep, RightAtt, NoValence, Stop ) )
      val rFactorAndOutside =
              outsideM( i )( j )( RightwardM ) * rFactor
      // message to right child, a PlainM node
      outsideM( k )( j )( PlainM ) +=
        insideM( i )( k )( RightwardM ) * rFactorAndOutside

      // message to left child, a RightwardM node
      outsideM( i )( k )( RightwardM ) +=
        insideM( k )( j )( PlainM ) *
          outsideM( i )( j )( RightwardM ) * rFactorAndOutside
    }
  }

  def lexMarginals( index:Int ) = Map[Event,Double]()


}

