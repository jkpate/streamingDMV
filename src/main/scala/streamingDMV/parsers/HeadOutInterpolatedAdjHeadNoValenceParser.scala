package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.HeadOutInterpolatedAdjHeadNoValenceParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class HeadOutInterpolatedAdjHeadNoValenceParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  backoffAlpha:Double = 1D,
  notBackoffAlpha:Double = 10D,
  randomSeed:Int = 15
) extends SecondOrderFoldUnfoldParser[HeadOutInterpolatedAdjHeadNoValenceParameters](
  maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed
) {

  val theta = 
    new HeadOutInterpolatedAdjHeadNoValenceParameters(
      rootAlpha,
      stopAlpha,
      chooseAlpha,
      backoffAlpha,
      notBackoffAlpha
    )


  val insideHeads:Array[Array[MMap[Decoration,Double]]] =
    Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
      if( ( i%2 != j%2 ) ) {
        MMap( NoValence -> 0D )
      } else {
        MMap()
      }
    )
  // One of the m-node children is a head-child, so they can't both be outermost
  val insideM:Array[Array[MMap[MDecoration,Double]]] =
    Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
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
  val outsideHeads:Array[Array[MMap[Decoration,Double]]] =
    Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
      if( ( i%2 != j%2 ) ) {
        MMap( NoValence -> 0D )
      } else {
        MMap()
      }
    )
  // One of the m-node children is a head-child, so they can't both be outermost
  val outsideM:Array[Array[MMap[MDecoration,Double]]] =
    Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
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


  def findLeftRootChild( k:Int ) =
    headTrace( 0 )( k )( NoValence )
  def findRightRootChild( k:Int ) =
    headTrace( k )( intString.length )( NoValence )

  def findLeftLeftwardChild( i:Int, k:Int ) = {
    // assert( k-i > 1 )
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
    // assert( j-k > 1 )
    headTrace( k )( j )( NoValence )
  }

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) = {
    if( k%2 == 0 ) {
      // assert(
      //   !( decoration == RightwardM && (k-i > 1 ) )
      // )
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
      // assert(
      //   !( decoration == LeftwardM && (j-k > 1 ) )
      // )
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

  def leftArcParentVs( i:Int ) = Set[Decoration]( NoValence )
  def rightArcParentVs( j:Int ) = Set[Decoration]( NoValence )

  def populateRootCell( k:Int ) {
    val r = intString( k )

    stringProb +=
      insideHeads(0)(k)(NoValence) *
        insideHeads(k)(intString.length)(NoValence) *
          theta( RootEvent( r ) ) *
            theta( StopEvent( r, LeftAtt, NoValence, Stop ) ) *
            theta( StopEvent( r, RightAtt, NoValence, Stop ) )
  }

  def populateRightwardCell( i:Int, k:Int, j:Int ) {
    val head = intString( i )
    val dep = intString( k )

    insideHeads( i )( j )( NoValence ) +=
      insideHeads( k )( j )( NoValence ) *
        insideM( i )( k )( RightwardM )
          // *
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
        insideM( k )( j )( LeftwardM )
          // *
          // theta( ChooseEvent( head, LeftAtt, dep ) ) *
          // theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
          //   theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
          //   theta( StopEvent( dep, LeftAtt, NoValence, Stop ) )
  }

  def nearestArcFactor( head:Int, dir:AttDir, dep:Int ) = {
    theta( ChooseEvent( head, dir, dep ) ) *
    theta( StopEvent( head, dir, NoValence, NotStop ) ) *
      theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) *
      theta( StopEvent( dep, dir, NoValence, Stop ) )
  }

  def backoffChoose( head:Int, context:Int, dir:AttDir, dep:Int ) = {
    theta( LambdaChooseEvent( head, context, dir, Backoff ) ) *
      theta( ChooseEvent( head, dir, dep ) )
  }
  def notBackoffChoose( head:Int, context:Int, dir:AttDir, dep:Int ) = {
    theta( LambdaChooseEvent( head, context, dir, NotBackoff ) ) *
      theta( ChooseEvent( head, context, dir, dep ) )
  }

  def stopFactors( head:Int, dir:AttDir, dep:Int ) = {
    theta( StopEvent( head, dir, NoValence, NotStop ) ) *
    theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) *
    theta( StopEvent( dep, dir, NoValence, Stop ) )
  }

  def outerArcFactor( head:Int, context:Int, dir:AttDir, dep:Int ) = {
    (
      backoffChoose( head, context, dir, dep ) +
      notBackoffChoose( head, context, dir, dep )
    ) * stopFactors( head, dir, dep )
  }

  def populateMCell( i:Int, k:Int, j:Int ) {
    if( k%2 == 0 ) {
      // only directed children
      insideM( i )( j )( PlainM ) +=
        insideHeads( i )( k )( NoValence ) *
          insideHeads( k )( j )( NoValence )

      if( k-i == 1 )
        insideM( i )( j )( RightwardM ) +=
          insideHeads( i )( k )( NoValence ) *
            insideHeads( k )( j )( NoValence ) *
              nearestArcFactor( intString(i), RightAtt, intString(j) )

      if( j-k == 1 )
        insideM( i )( j )( LeftwardM ) +=
          insideHeads( i )( k )( NoValence ) *
            insideHeads( k )( j )( NoValence ) *
              nearestArcFactor( intString(j), LeftAtt, intString(i) )
    } else {
      val context = intString(k)

      // LeftwardM first
      val lHead = intString(j)
      val lDep = intString(i)
      // left child of LeftwardM is a PlainM, right child is a LeftwardM
      insideM( i )( j )( LeftwardM ) +=
        insideM( i )( k )( PlainM ) *
          insideM( k )( j )( LeftwardM ) *
            outerArcFactor( lHead, context, LeftAtt, lDep )

      // Now RightwardM
      val rHead = intString(i)
      val rDep = intString(j)
      // left child of RightwardM is a RightwardM, right child is a PlainM
      insideM( i )( j )( RightwardM ) +=
        insideM( i )( k )( RightwardM ) *
          insideM( k )( j )( PlainM ) *
            outerArcFactor( rHead, context, RightAtt, rDep )

    }
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

    Seq(
      ( RootEvent( obs ) , marginal ),
      ( StopEvent( obs, LeftAtt, NoValence, Stop ) , marginal ),
      ( StopEvent( obs, RightAtt, NoValence, Stop ) , marginal )
    )
  }

  def outsideLeft( i:Int, k:Int, j:Int ) {
    // First, send messages to left child -- that is, the leftward-looking dependent label.
    outsideHeads( i )( k )( NoValence ) +=
      insideM( k )( j )( LeftwardM ) * // factorAndOutside
        outsideHeads( i )( j )( NoValence )

    // First, send messages to rightward child -- that is, the leftward-looking M-label.
    outsideM( k )( j )( LeftwardM ) +=
        insideHeads( i )( k )( NoValence ) * // factorAndOutside
        outsideHeads( i )( j )( NoValence )
  }

  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ) = {
    // All attachments in second order parser happen in M nodes
    assert( j-i >= 3 )

    // First, send messages to left child -- that is, the leftward-looking dependent label.
    outsideHeads( i )( k )( NoValence ) +=
      insideM( k )( j )( LeftwardM ) *
        outsideHeads( i )( j )( NoValence )

    // First, send messages to rightward child -- that is, the leftward-looking M-label.
    outsideM( k )( j )( LeftwardM ) +=
      insideHeads( i )( k )( NoValence ) *
        outsideHeads( i )( j )( NoValence )

    Seq()
  }

  def outsideRight( i:Int, k:Int, j:Int ) {
    // First, send messages to right child -- that is, the rightward-looking dependent label.
    outsideHeads( k )( j )( NoValence ) +=
      insideM( i )( k )( RightwardM ) * // factorAndOutside
        outsideHeads( i )( j )( NoValence )

    outsideM( i )( k )( RightwardM ) +=
      insideHeads( k )( j )( NoValence ) * // factorAndOutside
        outsideHeads( i )( j )( NoValence )
  }

  def outsideRightWithMarginals( i:Int, k:Int, j:Int ) = {
    // First, send messages to right child -- that is, the rightward-looking dependent label.
    outsideHeads( k )( j )( NoValence ) +=
      insideM( i )( k )( RightwardM ) *
        outsideHeads( i )( j )( NoValence )

    outsideM( i )( k )( RightwardM ) +=
      insideHeads( k )( j )( NoValence ) *
        outsideHeads( i )( j )( NoValence )

    Seq()
  }

  val mDecorations:Set[MDecoration] = Set( PlainM, LeftwardM, RightwardM )
  def mNodeParentVs( i:Int, j:Int ) = mDecorations
  def outsideM( i:Int, k:Int, j:Int ) {
    if( k%2 == 0 ) { // only Directed children
      // PlainM
      outsideHeads( i )( k )( NoValence ) +=
        outsideM( i )( j )( PlainM ) *
          insideHeads( k )( j )( NoValence )

      outsideHeads( k )( j )( NoValence ) +=
        outsideM( i )( j )( PlainM ) *
          insideHeads( i )( k )( NoValence )

      // LeftwardM
      if( j-k == 1 ) {
        val head = intString(j)
        val dep = intString(i)

        val factorAndOutside = 
          outsideM( i )( j )( LeftwardM ) *
            nearestArcFactor( head, LeftAtt, dep )

        outsideHeads( i )( k )( NoValence ) +=
          insideHeads( k )( j )( NoValence ) *
            factorAndOutside

        outsideHeads( k )( j )( NoValence ) +=
          insideHeads( i )( k )( NoValence ) *
            factorAndOutside
      }

      // RightwardM
      if( k-i == 1 ) {
        val head = intString(i)
        val dep = intString(j)

        val factorAndOutside = 
          outsideM( i )( j )( RightwardM ) *
            nearestArcFactor( head, RightAtt, dep )

        outsideHeads( i )( k )( NoValence ) +=
          insideHeads( k )( j )( NoValence ) *
            factorAndOutside

        outsideHeads( k )( j )( NoValence ) +=
          insideHeads( i )( k )( NoValence ) *
            factorAndOutside
      }
    } else { // parent is DirectedM
      val context = intString(k) // switch context and dep for top-down second-order

      // Leftward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val lHead = intString(j)
      val lDep = intString(i)

      val lFactorAndOutside =
        outerArcFactor( lHead, context, LeftAtt, lDep ) *
          outsideM( i )( j )( LeftwardM )
      // message to left child, a PlainM node
      outsideM( i )( k )( PlainM ) +=
        insideM( k )( j )( LeftwardM ) * lFactorAndOutside

      // message to right child, a LeftwardM node
      outsideM( k )( j )( LeftwardM ) +=
        insideM( i )( k )( PlainM ) * lFactorAndOutside


      // Rightward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val rHead = intString(i)
      val rDep = intString(j)

      val rFactorAndOutside =
        outerArcFactor( rHead, context, RightAtt, rDep ) *
          outsideM( i )( j )( RightwardM )

      // message to right child, a PlainM node
      outsideM( k )( j )( PlainM ) +=
        insideM( i )( k )( RightwardM ) * rFactorAndOutside

      // message to left child, a RightwardM node
      outsideM( i )( k )( RightwardM ) +=
        insideM( k )( j )( PlainM ) * rFactorAndOutside
    }
  }

  def outsideMWithMarginals( i:Int, k:Int, j:Int ) = {
    if( k%2 == 0 ) { // only Directed children
      // mDecorations.foreach{ parentM =>

      // PlainM
      outsideHeads( i )( k )( NoValence ) +=
        outsideM( i )( j )( PlainM ) * insideHeads( k )( j )( NoValence )

      outsideHeads( k )( j )( NoValence ) +=
        outsideM( i )( j )( PlainM ) * insideHeads( i )( k )( NoValence )

      {
        // LeftwardM
        if( j-k == 1 ) {
          val head = intString(j)
          val dep = intString(i)

          val factorAndOutside = 
            outsideM( i )( j )( LeftwardM ) * nearestArcFactor( head, LeftAtt, dep )

          outsideHeads( i )( k )( NoValence ) +=
            insideHeads( k )( j )( NoValence ) * factorAndOutside

          outsideHeads( k )( j )( NoValence ) +=
            insideHeads( i )( k )( NoValence ) * factorAndOutside


          val marginal = 
            insideHeads( i )( k )( NoValence ) *
              insideHeads( k )( j )( NoValence ) *
                factorAndOutside

          Seq(
            ( ChooseEvent( head, LeftAtt, dep ) , marginal ),
            ( StopEvent( head, LeftAtt, NoValence, NotStop ) , marginal ),
            ( StopEvent( dep, RightAtt, NoValence, Stop ) , marginal ),
            ( StopEvent( dep, LeftAtt, NoValence, Stop ) , marginal )
          )
        } else {
          Seq()
        }
      } ++ {
        // RightwardM
        if( k-i == 1 ) {
          val head = intString(i)
          val dep = intString(j)

          val factorAndOutside = 
            outsideM( i )( j )( RightwardM ) * nearestArcFactor( head, RightAtt, dep )

          outsideHeads( i )( k )( NoValence ) +=
            insideHeads( k )( j )( NoValence ) * factorAndOutside

          outsideHeads( k )( j )( NoValence ) +=
            insideHeads( i )( k )( NoValence ) * factorAndOutside


          val marginal = 
            insideHeads( i )( k )( NoValence ) *
              insideHeads( k )( j )( NoValence ) *
                factorAndOutside

          Seq(
            ( ChooseEvent( head, RightAtt, dep ) , marginal ),
            ( StopEvent( head, RightAtt, NoValence, NotStop ) , marginal ),
            ( StopEvent( dep, LeftAtt, NoValence, Stop ) , marginal ),
            ( StopEvent( dep, RightAtt, NoValence, Stop ) , marginal )
          )
        } else {
          Seq()
        }
      }
    } else { // parent is DirectedM
      val context = intString(k) // switch context and dep for top-down second-order

      // Leftward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val lHead = intString(j)
      val lDep = intString(i)

      val lFactorAndOutside =
        outerArcFactor( lHead, context, LeftAtt, lDep ) *
          outsideM( i )( j )( LeftwardM )

      // message to left child, a PlainM node
      outsideM( i )( k )( PlainM ) +=
        insideM( k )( j )( LeftwardM ) * lFactorAndOutside

      // message to right child, a LeftwardM node
      outsideM( k )( j )( LeftwardM ) +=
        insideM( i )( k )( PlainM ) * lFactorAndOutside


      // Rightward M parent
      // Directed M-nodes parent an arc. Get lexical items and local factor:
      val rHead = intString(i)
      val rDep = intString(j)

      val rFactorAndOutside =
        outerArcFactor( rHead, context, RightAtt, rDep ) *
          outsideM( i )( j )( RightwardM )

      // message to right child, a PlainM node
      outsideM( k )( j )( PlainM ) +=
        insideM( i )( k )( RightwardM ) * rFactorAndOutside

      // message to left child, a RightwardM node
      outsideM( i )( k )( RightwardM ) +=
        insideM( k )( j )( PlainM ) * rFactorAndOutside

      val lStopFactorsAndBlanket =
        outsideM(i)(j)(LeftwardM) *
          insideM(i)(k)(PlainM) *
          insideM(k)(j)(LeftwardM) *
            stopFactors( lHead, LeftAtt, lDep )

      val lBackoffMarginal =
        backoffChoose( lHead, context, LeftAtt, lDep ) * lStopFactorsAndBlanket
      val lNotBackoffMarginal =
        notBackoffChoose( lHead, context, LeftAtt, lDep ) * lStopFactorsAndBlanket

      val totalLMarginal = lBackoffMarginal + lNotBackoffMarginal


      val rStopFactorsAndBlanket =
        outsideM(i)(j)(RightwardM) *
          insideM(i)(k)(RightwardM) *
          insideM(k)(j)(PlainM) *
            stopFactors( rHead, RightAtt, rDep )

      val rBackoffMarginal =
        backoffChoose( rHead, context, RightAtt, rDep ) * rStopFactorsAndBlanket
      val rNotBackoffMarginal =
        notBackoffChoose( rHead, context, RightAtt, rDep ) * rStopFactorsAndBlanket

      val totalRMarginal = rBackoffMarginal + rNotBackoffMarginal


      Seq(
        ( ChooseEvent( rHead, context, RightAtt, rDep ) , rNotBackoffMarginal ),
        ( ChooseEvent( rHead, RightAtt, rDep ) , rBackoffMarginal ),
        ( StopEvent( rHead, RightAtt, NoValence, NotStop ) , totalRMarginal ),
        ( StopEvent( rDep, LeftAtt, NoValence, Stop ) , totalRMarginal ),
        ( StopEvent( rDep, RightAtt, NoValence, Stop ) , totalRMarginal ),

        ( ChooseEvent( lHead, context, LeftAtt, lDep ) , lNotBackoffMarginal ),
        ( ChooseEvent( lHead, LeftAtt, lDep ) , lBackoffMarginal ),
        ( StopEvent( lHead, LeftAtt, NoValence, NotStop ) , totalLMarginal ),
        ( StopEvent( lDep, RightAtt, NoValence, Stop ) , totalLMarginal ),
        ( StopEvent( lDep, LeftAtt, NoValence, Stop ) , totalLMarginal )
      )
    }
  }


  def lexMarginals( index:Int ) = Seq()


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
    }// .toMap
  }

  def viterbiRightRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( i )
    ( (i+2) to (j-1) by 2 ).map{ k =>
      val dep = intString( k )

      SplitSpec( k, NoValence, NoValence ) -> {
        insideHeads( k )( j )( NoValence ) *
          insideM( i )( k )( RightwardM ) // *
            // theta( ChooseEvent( head, RightAtt, dep ) ) *
            // theta( StopEvent( head, RightAtt, NoValence, NotStop ) ) *
            //   theta( StopEvent( dep, LeftAtt, NoValence, Stop ) ) *
            //   theta( StopEvent( dep, RightAtt, NoValence, Stop ) )
      }
    }//.toMap
  }

  def viterbiLeftRank( i:Int, j:Int, parentV:Decoration ) = {
    val head = intString( j )
    ( (i+1) to (j-2) by 2 ).map{ k =>
      val dep = intString( k )

      SplitSpec( k, NoValence, NoValence ) -> {
        insideHeads( i )( k )( NoValence ) *
          insideM( k )( j )( LeftwardM ) // *
            // theta( ChooseEvent( head, LeftAtt, dep ) ) *
            // theta( StopEvent( head, LeftAtt, NoValence, NotStop ) ) *
            //   theta( StopEvent( dep, RightAtt, NoValence, Stop ) ) *
            //   theta( StopEvent( dep, LeftAtt, NoValence, Stop ) )
      }
    }// .toMap
  }



  def viterbiMRank( i:Int, j:Int, decoration:MDecoration ) = {
    mSplits( i, j ).flatMap{ k =>
      if( k%2 == 0 ) {
        decoration match {
          case LeftwardM =>
            if( j-k == 1 )
              Seq(
                SplitSpec( k, NoValence, NoValence ) -> {
                  insideHeads( i )( k )( NoValence ) *
                    insideHeads( k )( j )( NoValence ) *
                      nearestArcFactor( intString(j), LeftAtt, intString(i) )
                }
              )
            else Seq()
          case RightwardM =>
            if( k-i == 1 )
              Seq(
                SplitSpec( k, NoValence, NoValence ) -> {
                  insideHeads( i )( k )( NoValence ) *
                    insideHeads( k )( j )( NoValence ) *
                      nearestArcFactor( intString(i), RightAtt, intString(j) )
                }
              )
            else Seq()
          case PlainM =>
              Seq(
                SplitSpec( k, NoValence, NoValence ) -> {
                  insideHeads( i )( k )( NoValence ) *
                    insideHeads( k )( j )( NoValence )
                }
              )
        }
      } else {
        val context = intString(k) // switch context and dep for top-down second-order

        decoration match {
          case LeftwardM => {
            // LeftwardM first
            val lHead = intString(j)
            val lDep = intString(i)
            // left child of LeftwardM is a PlainM, right child is a LeftwardM
            Seq(
              SplitSpec(k,PlainM,LeftwardM) -> {
                insideM( i )( k )( PlainM ) *
                  insideM( k )( j )( LeftwardM ) *
                    outerArcFactor( lHead, context, LeftAtt, lDep )
              }
            )
          }
          case RightwardM => {
            // Now RightwardM
            val rHead = intString(i)
            val rDep = intString(j)
            // left child of RightwardM is a RightwardM, right child is a PlainM
            Seq(
              SplitSpec(k,RightwardM,PlainM) -> {
                  insideM( i )( k )( RightwardM ) *
                    insideM( k )( j )( PlainM ) *
                      outerArcFactor( rHead, context, RightAtt, rDep )
              }
            )
          }
          case PlainM => Seq()
        }

      }
    }
  }// .toMap

  def viterbiLexFill( index:Int ) {
    insideHeads(index)(index+1)( NoValence ) = 1D
    headTrace(index)(index+1) += NoValence -> LexEntry( index )
  }

}

