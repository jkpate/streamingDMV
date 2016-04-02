package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ThreeValenceParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class ThreeValenceParser(
    // maxLength:Int,
    // rootAlpha:Double = 1D,
    // stopAlpha:Double = 1D,
    // chooseAlpha:Double = 1D,
    // randomSeed:Int = 15,
    // squarelyNormalized:Int = 0,
    // val approximate:Boolean = false,
    // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends FirstOrderFoldUnfoldNOPOSParser[ThreeValenceParameters](
  // maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed, reservoirSize
  parserSpec
) {
  // println( "INITIALIZING THREEVALENCEPARSER" )

  val theta = new ThreeValenceParameters( parserSpec.toParameterSpec )

  def cellMap( i:Int, j:Int ) = {
    if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      MMap(
        // NoDependentValence means I haven't generated a dependent yet TOP-DOWN i.e. I am a root or
        // the *immediate* dependent child of another head.
        NoDependentValence -> myZero,
        OneDependentValence -> myZero,
        TwoDependentValence -> myZero
      )
    else if( i%2 == 1 && j%2 == 1 )
      MMap(
        // Because NoDependentValence means I haven't generated a dependent yet, the dependent child
        // *must* be NoDependentValence, and the head child must *not* be NoDependentValence
        DecorationPair(NoDependentValence,OneDependentValence) -> myZero,
        DecorationPair(NoDependentValence,TwoDependentValence) -> myZero,
        DecorationPair(OneDependentValence,NoDependentValence) -> myZero,
        DecorationPair(TwoDependentValence,NoDependentValence) -> myZero
      )
    else
      MMap()
  }


      // def cellMap( i:Int, j:Int ) = {
      //   if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      //     if( j-i >= 5 )
      //       // if j-i >= 5, we dominate at least two children
      //       MMap(
      //         TwoDependentValence -> myZero
      //       )
      //     else if( j-i == 3 )
      //       MMap(
      //         // If j-i == 3, we must dominate one child
      //         OneDependentValence -> myZero
      //       )
      //     else // j-i == 1
      //       MMap(
      //         NoDependentValence -> myZero
      //       )
      //   else if( i%2 == 1 && j%2 == 1 )
      //     // For j-i == 2, there are no intervening dependents for either child to dominate.
      //     // For j-i == 4, there is exactly one intervening child that must be dominated by exactly one
      //     // head child.
      //     // For j-i == 6, there are two intervening children that must be dominated by one head child.
      //     // For j-i == 8, there are three intervening children that must be dominated by one head
      //     // child. (and the TwoDependentValence child can actually dominate more than two, since
      //     // TwoDependentValence is really two or more dependent valence).
      //     // For j-i >= 10, there are four intervening children that must be dominated by one head, so
      //     // both children can dominate at least two.
      //     // Will need to be careful in LeftwardSplitSpecs and RightwardSplitSpecs to enforce j-i >= 8
      //     // properly (since those are the cases where TwoDependentValence can mean ``two or more'').
      //     if( j-i >= 10 )
      //       MMap(
      //         // M-child of left attachments first. TwoDependentValence spans at least 5
      //         DecorationPair( NoDependentValence, TwoDependentValence ) -> myZero,
      //         DecorationPair( OneDependentValence, TwoDependentValence ) -> myZero,
      //         DecorationPair( TwoDependentValence, TwoDependentValence ) -> myZero,
      //         DecorationPair( TwoDependentValence, OneDependentValence ) -> myZero,
      //         DecorationPair( TwoDependentValence, NoDependentValence ) -> myZero
      //       )
      //     else if( j-i == 8 )
      //       MMap(
      //         // M-child of left attachments first. TwoDependentValence spans at least 5
      //         DecorationPair( NoDependentValence, TwoDependentValence ) -> myZero,
      //         DecorationPair( OneDependentValence, TwoDependentValence ) -> myZero,
      //         DecorationPair( TwoDependentValence, OneDependentValence ) -> myZero,
      //         DecorationPair( TwoDependentValence, NoDependentValence ) -> myZero
      //       )
      //     else if( j-i == 6 )
      //       MMap(
      //         // M-child of left attachments first. TwoDependentValence spans at least 5
      //         DecorationPair( NoDependentValence, TwoDependentValence ) -> myZero,
      //         DecorationPair( OneDependentValence, OneDependentValence ) -> myZero,
      //         DecorationPair( NoDependentValence, TwoDependentValence ) -> myZero
      //       )
      //     else if( j-i == 4 )
      //       MMap(
      //         // M-child of left attachment (the dependent spans 3 and so has a child)
      //         DecorationPair(OneDependentValence,NoDependentValence) -> myZero,
      //         // M-child of right attachment (the dependent spans 3 and so has a child)
      //         DecorationPair(NoDependentValence,OneDependentValence) -> myZero
      //       )
      //     else // j-i == 2
      //       MMap(
      //         // M-child of left and right attachments
      //         DecorationPair(NoDependentValence,NoDependentValence) -> myZero
      //       )
      //   else
      //     MMap()
      // }

      // val spanToV = Map(
      //   3 -> OneDependentValence
      //   1 -> NoDependentValence
      // ).withDefault( TwoDependentValence )

      // def recoverM( mDV:Decoration ) = {
      //   mDV match {
      //     case p:DecorationPair => p
      //   }
      // }

      // val incHeadV = Map[Decoration,Decoration](
      //     NoDependentValence -> OneDependentValence
      //   ).withDefaultValue( TwoDependentValence )

  // def incHeadV( v:Decoration ) =
  //   if( v == NoDependentValence ) OneDependentValence else TwoDependentValence

  def findLeftRootChild( k:Int, rDec:Decoration ) = {
    headTrace( 0 )( k )( NoDependentValence )
  }

  def findRightRootChild( k:Int, rDec:Decoration ) = {
    val span = intString.length - k
    headTrace( k )( intString.length )( NoDependentValence )
  }

  def findLeftLeftwardChild( i:Int, k:Int, mDV:Decoration ) = headTrace( i )( k )( NoDependentValence )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( k )( j )( DecorationPair( NoDependentValence, hV ) )

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) =
    mTrace( i )( k )( DecorationPair( hV, NoDependentValence ) )
  def findRightRightwardChild( k:Int, j:Int, mDV:Decoration ) = headTrace( k )( j )( NoDependentValence )

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) =
    headTrace( i )( k )( decoration.evenLeft )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) =
    headTrace( k )( j )( decoration.evenRight )

  def lexCellFactor( index:Int, pDec:Decoration ) = {
    // println( intString.mkString("{"," ","}") )
    val head = intString( index )
    val score = 
      if( index%2 == 0 ) {
        assert( pDec == NoDependentValence || index >= 2 )
        assert( pDec != TwoDependentValence || index >= 4 )
        theta( StopEvent( head, LeftAtt, pDec, Stop ) )
      } else {
        assert( pDec == NoDependentValence || index <= intString.length-3 )
        assert( pDec != TwoDependentValence || index <= intString.length-5 )
        theta( StopEvent( head, RightAtt, pDec, Stop ) )
      }
    // if( !( score > Double.NegativeInfinity && score <= 0D ) ) {
      // println( "TopDownDMVParser.lexCellFactor: " + (index,pDec,score) )
    // }
    assert( ( score > myZero && score <= myOne + 0.00001 ) )
    score
  }

  def lexSpecs( index:Int ) = {
    if( index%2 == 0 ) {
      // println( s"visiting $index: ${(index,intString.length)}" )
      if( index > 2 )
        Seq( NoDependentValence, OneDependentValence, TwoDependentValence )
      else if( index > 0 ) {
        // println( s"using two valences" )
        Seq( NoDependentValence, OneDependentValence )
      } else
        Seq( NoDependentValence )
    } else {
      if( index <= intString.length-5 )
        Seq( NoDependentValence, OneDependentValence, TwoDependentValence )
      else if( index <= intString.length-3 )
        Seq( NoDependentValence, OneDependentValence )
      else
        Seq( NoDependentValence )
    }
  }

  def lexEventCounts( index:Int, pDec:Decoration, marginal:Double ) = {
    val w = intString( index )
    if( index%2 == 0 ) {
      Seq( ( StopEvent( w, LeftAtt, pDec, Stop ) , marginal ) )
    } else {
      Seq( ( StopEvent( w, RightAtt, pDec, Stop ) , marginal ) )
    }
  }

  def rightwardSplitSpecs( i:Int, j:Int ) = {
    val ks = ( (i+2) to (j-1) by 2 )
    if( j == intString.length ) {
        // can't have any dependents (top-down) to the right yet
      Seq(
        (
          NoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( OneDependentValence, NoDependentValence ), NoDependentValence )
          }
        )
      )
    } else if( j == intString.length-2 ) {
        // can have at most *one* dependent (top-down) to the right
      Seq(
        (
          NoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( OneDependentValence, NoDependentValence ), NoDependentValence )
          }
        ),
        (
          OneDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( TwoDependentValence, NoDependentValence ), NoDependentValence )
          }
        )
      )
    } else {
        // can have zero, one, two, or more dependents (top-down) to the right
      Seq(
        (
          NoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( OneDependentValence, NoDependentValence ), NoDependentValence )
          }
        ),
        (
          OneDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( TwoDependentValence, NoDependentValence ), NoDependentValence )
          }
        ),
        (
          TwoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( TwoDependentValence, NoDependentValence ), NoDependentValence )
          }
        )
      )
    }
  }

  def leftwardSplitSpecs( i:Int, j:Int ) = {
    val ks = ( (i+1) to (j-2) by 2 )
    if( i == 0 ) {
        // can't have any dependents (top-down) to the left yet
      Seq(
        (
          NoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( NoDependentValence, OneDependentValence ), NoDependentValence )
          }
        )
      )
    } else if( i == 2 ) {
        // can have at most *one* dependent (top-down) to the left
      Seq(
        (
          NoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( NoDependentValence, OneDependentValence ), NoDependentValence )
          }
        ),
        (
          OneDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( NoDependentValence, TwoDependentValence ), NoDependentValence )
          }
        )
      )
    } else {
        // can have zero, one, two, or more dependents (top-down) to the left
      Seq(
        (
          NoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( NoDependentValence, OneDependentValence ), NoDependentValence )
          }
        ),
        (
          OneDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( NoDependentValence, TwoDependentValence ), NoDependentValence )
          }
        ),
        (
          TwoDependentValence,
          ks.map{ k =>
            ( k, DecorationPair( NoDependentValence, TwoDependentValence ), NoDependentValence )
          }
        )
      )
    }
  }

  val mParents =
    Seq(
      DecorationPair( NoDependentValence, OneDependentValence ),
      DecorationPair( NoDependentValence, TwoDependentValence ),
      DecorationPair( NoDependentValence, NoDependentValence ),
      DecorationPair( OneDependentValence, NoDependentValence ),
      DecorationPair( TwoDependentValence, NoDependentValence )
    )

  val fullMParents =
    Seq(
      DecorationPair( NoDependentValence, OneDependentValence ),
      DecorationPair( NoDependentValence, TwoDependentValence ),
      DecorationPair( NoDependentValence, NoDependentValence ),
      DecorationPair( OneDependentValence, NoDependentValence ),
      DecorationPair( TwoDependentValence, NoDependentValence )
    )
  val leftBoundedMParents =
    Seq(
      DecorationPair( NoDependentValence, OneDependentValence ),
      DecorationPair( NoDependentValence, TwoDependentValence ),
      DecorationPair( OneDependentValence, NoDependentValence )
    )
  val rightBoundedMParents =
    Seq(
      DecorationPair( NoDependentValence, OneDependentValence ),
      DecorationPair( OneDependentValence, NoDependentValence ),
      DecorationPair( TwoDependentValence, NoDependentValence )
    )
  val bothBoundedMParents =
    Seq(
      DecorationPair( NoDependentValence, OneDependentValence ),
      DecorationPair( OneDependentValence, NoDependentValence )
    )

  def min( a:Int, b:Int ) = if( a < b ) a else b
  def max( a:Int, b:Int ) = if( a > b ) a else b
  def mSplitSpecs( i:Int, j:Int ) = {
        // val ks = ( (i+1) to (j-1) by 2 )

        // val mParents =
        //   if( j > 3 && i < intString.length-4 )
        //     fullMParents
        //   else if( i < intString.length-4 )
        //     rightBoundedMParents
        //   else if( j > 3 )
        //     leftBoundedMParents
        //   else
        //     bothBoundedMParents

        // mParents.map{ mDec =>
        //   (
        //     mDec,
        //     ks
        //   )
        // }

    val ks = ( (i+1) to (j-1) by 2 )

    val leftMParents =
      if( i >= 3 )
        Seq(
          DecorationPair( NoDependentValence, OneDependentValence ),
          DecorationPair( NoDependentValence, TwoDependentValence )
        )
      else
        Seq(
          DecorationPair( NoDependentValence, OneDependentValence )
        )

    val rightMParents =
      // if( i >= 5 )
      //   Seq(
      //     DecorationPair( OneDependentValence, NoDependentValence ),
      //     DecorationPair( TwoDependentValence, NoDependentValence )
      //   )
      // else
     if( j <= intString.length-3 )
        Seq(
          DecorationPair( OneDependentValence, NoDependentValence ),
          DecorationPair( TwoDependentValence, NoDependentValence )
        )
      else
        Seq(
          DecorationPair( OneDependentValence, NoDependentValence )
        )

    (
      leftMParents ++
      rightMParents
    ).map{ mDec => ( mDec, ks ) }



        // {
        //   if( j >= 4 && i <= intString.length -5 ) {
        //     Seq(
        //       (
        //         DecorationPair( NoDependentValence, TwoDependentValence ),
        //         DecorationPair( NoDependentValence, OneDependentValence ),
        //         DecorationPair( NoDependentValence, NoDependentValence ),
        //         DecorationPair( TwoDependentValence, NoDependentValence ),
        //         DecorationPair( OneDependentValence, NoDependentValence ),
        //         DecorationPair( TwoDependentValence, NoDependentValence ),
        //         ks
        //       )
        //     )
        //   } else {
        //     Seq()
        //   }
        // } ++ {
        //   if( j >= 2 && i <= intString.length -5 ) {
        //     Seq(
        //       (
        //         DecorationPair( NoDependentValence, OneDependentValence ),
        //         DecorationPair( TwoDependentValence, OneDependentValence )
        //         ks
        //       )
        //     )
        //   } else {
        //     Seq()
        //   }
        // } ++ {
        //   if( j >= 4 && i <= intString.length -3 ) {
        //     Seq(
        //       (
        //         DecorationPair( OneDependentValence, TwoDependentValence ),
        //         ks
        //       )
        //     )
        //   } else {
        //     Seq()
        //   }
        // } ++ {
        //   if( j >= 2 && i <= intString.length -3 ) {
        //     Seq(
        //       (
        //         DecorationPair( OneDependentValence, OneDependentValence ),
        //         ks
        //       )
        //     )
        //   } else {
        //     Seq()
        //   }
        // } ++ {
        //   if( i <= intString.length -3 ) {
        //     Seq(
        //       (
        //         DecorationPair( OneDependentValence, NoDependentValence ),
        //         ks
        //       )
        //     )
        //   } else {
        //     Seq()
        //   }
        // } ++ {
        //   if( j >= 2 ) {
        //     Seq(
        //       (
        //         DecorationPair( NoDependentValence, OneDependentValence ),
        //         ks
        //       )
        //     )
        //   } else {
        //     Seq()
        //   }
        // } ++
        // Seq(
        //   (
        //     DecorationPair( NoDependentValence, NoDependentValence ),
        //     ks
        //   )
        // )
  }

  def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).map{ k =>
      (
        k,
        DecorationPair(
          {
            NoDependentValence
          }, {
            NoDependentValence
          }
        )
      )
    }
  }

  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    val dep = intString( k )

    myTimes(
      theta( ChooseEvent( head, RightAtt, dep ) ),
        theta( StopEvent( head, RightAtt, pDec, NotStop ) )
    )
  }

  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )
    val dep = intString( k )

    myTimes(
      theta( ChooseEvent( head, LeftAtt, dep ) ),
        theta( StopEvent( head, LeftAtt, pDec, NotStop ) )
    )
  }


  def rootCellFactor( k:Int, rDec:DecorationPair ) = {
    theta( RootEvent( intString( k ) ) )
  }

  def rootEventCounts( k:Int, rDec:Decoration, marginal:Double ) = {
    val r = intString( k )
    Seq(
      ( RootEvent( r ), marginal )
    )
  }
  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( i )
    val dep = intString( k )
    Seq(
      ( ChooseEvent( head, RightAtt, dep ), marginal ),
      ( StopEvent( head, RightAtt, pDec, NotStop ), marginal )
    )
  }
  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( j )
    val dep = intString( k )
    // println( ( ChooseEvent( head, LeftAtt, dep ), marginal ) )

    // if( head == 6 && dep == 3 ) { println( marginal ) }

    Seq(
      ( ChooseEvent( head, LeftAtt, dep ), marginal ),
      ( StopEvent( head, LeftAtt, pDec, NotStop ), marginal )
    )
  }


  def trueLogProb( counts:DMVCounts ) = {
    // trueLogProb is always in log-space, so don't use myTimes
    theta.p_root.trueLogProb( counts.rootCounts ) +
    theta.p_stop.trueLogProb( counts.stopCounts ) +
    theta.p_choose.trueLogProb( counts.chooseCounts )
  }



}

