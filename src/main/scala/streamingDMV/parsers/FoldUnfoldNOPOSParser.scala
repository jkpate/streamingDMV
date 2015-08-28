package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.tables.CPT
import streamingDMV.parameters.NOPOSArcFactoredParameters


import scala.collection.mutable.{Map=>MMap}
import math.log


abstract class FoldUnfoldNOPOSParser[P<:NOPOSArcFactoredParameters](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends StreamingVBParser[DMVCounts,P]( maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed ) {


  val insideChart:Array[Array[MMap[Decoration,Double]]]
  val outsideChart:Array[Array[MMap[Decoration,Double]]]

  def lexSpecs( index:Int ):Seq[Decoration]
  def lexCellFactor( index:Int, pDec:Decoration ):Double
  def lexFill( index:Int ) {
    lexSpecs( index ).foreach{ pDec =>
      insideChart( index )( index+1 )(pDec) += lexCellFactor( index, pDec )
    }
  }
  def insidePass( s:Array[Int] ) = {
    intString = s
    (1 to ( intString.length )).foreach{ j =>
      lexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          synFill( i , j )
        }
    }
  }

  def mSplitSpecs( i:Int, j:Int ):Seq[Tuple2[MDecoration,Seq[Int]]]
  def rootSplitSpecs():Seq[Tuple2[Int,DecorationPair]]
  def rightwardSplitSpecs(i:Int,j:Int):Seq[Tuple2[Decoration,Seq[Tuple3[Int,MDecoration,Decoration]]]]
  def leftwardSplitSpecs(i:Int,j:Int):Seq[Tuple2[Decoration,Seq[Tuple3[Int,MDecoration,Decoration]]]]

  def rightwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    insideChart( i )( k )( mDec ) *
      insideChart( k )( j )( cDec ) *
        rightwardCellFactor( i, k, j, pDec, mDec, cDec )
  }
  def leftwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    insideChart( i )( k )( cDec ) *
      insideChart( k )( j )( mDec ) *
        leftwardCellFactor( i, k, j, pDec, mDec, cDec )
  }
  def mCellScore( i:Int, k:Int, j:Int, mDecoration:MDecoration ) = {
    if( k%2 == 0 ) 
      insideChart( i )( k )( mDecoration.evenLeft ) *
        insideChart( k )( j )( mDecoration.evenRight ) *
          mCellFactor( i, k, j, mDecoration )
    else
      insideChart( i )( k )( mDecoration.oddLeft ) *
        insideChart( k )( j )( mDecoration.oddRight ) *
          mCellFactor( i, k, j, mDecoration )
  }
  def rootCellScore( k:Int, leftDec:Decoration, rightDec:Decoration ) = {
    insideChart( 0 )( k )( leftDec ) *
      insideChart( k )( intString.length )( rightDec ) *
        rootCellFactor( k )
  }


  def computeInsideMScore( i:Int, j:Int ) {
    mSplitSpecs(i,j).foreach{ case (mDecoration, splits) =>
      splits.foreach{ k =>
        insideChart( i )( j )( mDecoration ) += mCellScore( i, k, j, mDecoration )
      }
    }
  }
  def computeInsideRootScore() {
    rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
      val r = intString( k )

      stringProb +=
        rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight )
        // insideChart( 0 )( k )( decorationPair.evenLeft ) *
        //   insideChart( k )( intString.length )( decorationPair.evenRight ) *
        //     rootCellFactor( k )
    }
  }
  def computeInsideRightwardScore( i:Int, j:Int ) {
    rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        insideChart( i )( j )( pDec ) +=
          rightwardCellScore( i, k, j, pDec, mDec, cDec )
      }
    }
  }
  def computeInsideLeftwardScore( i:Int, j:Int ) {
    leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        insideChart( i )( j )( pDec ) +=
          leftwardCellScore( i, k, j, pDec, mDec, cDec )
      }
    }
  }

  def rootCellFactor( k:Int ):Double
  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ):Double
  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ):Double
  def mCellFactor( i:Int, k:Int, j:Int, mDecoration:MDecoration ):Double

  def rightwardCellScores( i:Int, j:Int ) = {
    rightwardSplitSpecs( i, j ).map{ case ( pDec, splits ) =>
      (
        pDec,
        splits.view.map{ case ( k, mDec, cDec ) =>
          (
            (k,mDec, cDec),
            rightwardCellScore( i, k, j, pDec, mDec, cDec )
          )
        }
      )
    }
  }
  def leftwardCellScores( i:Int, j:Int ) = {
    leftwardSplitSpecs( i, j ).map{ case ( pDec, splits ) =>
      (
        pDec,
        splits.view.map{ case ( k, mDec, cDec ) =>
          (
            (k,mDec, cDec),
            leftwardCellScore( i, k, j, pDec, mDec, cDec )
          )
        }
      )
    }
  }
  def mCellScores( i:Int, j:Int ):Seq[Tuple2[MDecoration,Seq[Tuple2[Int,Double]]]] = {
    mSplitSpecs( i, j ).map{ case (mDecoration, splits) =>
      (
        mDecoration,
        splits.view.map{ k => 
          (k, mCellScore( i, k, j, mDecoration ) )
        }
      )
    }
  }

  def rootCellScores() = {
    rootSplitSpecs().map{ case ( k, decorationPair ) =>
      (
        ( k, decorationPair ),
        rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight )
      )
    }
  }

  def synFill( i:Int, j:Int ) {
    if( i%2 == 1 && j%2 == 0 ) {
      computeInsideRightwardScore( i, j )
    } else if( i%2 == 0 && j%2 == 1 ) {
      computeInsideLeftwardScore( i, j )
    } else if( i%2 == 1 && j%2 == 1 ) {
      computeInsideMScore( i, j )
    } else {
      // Root
      if( i == 0 && j == intString.length )
        computeInsideRootScore()
    }
  }

  def outsidePass {
    ( 1 to intString.length ).reverse.foreach{ length =>
      ( 0 to ( intString.length - length ) ).foreach{ i =>
        val j = i + length
        if( i%2 == 0 && j%2 == 0 ) { // Root
          if( i == 0 && j == intString.length ) {
            rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
              val r = intString( k )
              val factorAndOutside = rootCellFactor( k )

              outsideChart( 0 )( k )( decorationPair.evenLeft ) +=
                insideChart( k )( intString.length )( decorationPair.evenRight ) * factorAndOutside

              outsideChart( k )( intString.length )( decorationPair.evenRight ) +=
                insideChart( 0 )( k )( decorationPair.evenLeft ) * factorAndOutside
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( j-i >= 3 ) {
            leftwardSplitSpecs( i, j ).foreach{ case ( decoration, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside =
                  outsideChart( i )( j )( decoration ) *
                    leftwardCellFactor( i, k, j, decoration, mDec, cDec )

                // to left child
                outsideChart( i )( k )( cDec ) +=
                  insideChart( k )( j )( mDec ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( mDec ) +=
                  insideChart( i )( k )( cDec ) *
                    factorAndOutside
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( j-i >= 3 ) {
            rightwardSplitSpecs( i, j ).foreach{ case ( decoration, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside =
                  outsideChart( i )( j )( decoration ) *
                    rightwardCellFactor( i, k, j, decoration, mDec, cDec )

                // to left child
                outsideChart( i )( k )( mDec ) +=
                  insideChart( k )( j )( cDec ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( cDec ) +=
                  insideChart( i )( k )( mDec ) *
                    factorAndOutside
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          mSplitSpecs( i, j ).foreach{ case (mDecoration, splits) =>
            splits.foreach{ k =>
              val factorAndOutside =
                outsideChart( i )( j )( mDecoration ) * mCellFactor( i, k, j, mDecoration )

              if( k%2 == 0 ) {
                // to left child
                outsideChart( i )( k )( mDecoration.evenLeft ) +=
                  insideChart( k )( j )( mDecoration.evenRight ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( mDecoration.evenRight ) +=
                  insideChart( i )( k )( mDecoration.evenLeft ) *
                    factorAndOutside
              } else {
                // to left child
                outsideChart( i )( k )( mDecoration.oddLeft ) +=
                  insideChart( k )( j )( mDecoration.oddRight ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( mDecoration.oddRight ) +=
                  insideChart( i )( k )( mDecoration.oddLeft ) *
                    factorAndOutside
              }
            }
          }
        }
      }
    }
  }



  // Viterbi definitions

  val headTrace = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    MMap[Decoration,Entry]()
  )
  val mTrace = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    MMap[MDecoration,MEntry]()
  )

  abstract class Entry( i:Int, j:Int ) {
    def toDepParse:Set[DirectedArc]
    def toConParse:String
  }

  object ParseFailed extends Entry( -1, -1 ) {
    def toDepParse = Set()
    def toConParse = "PARSE FAILED"
  }

  case class LexEntry( index:Int ) extends Entry( index, index+1 ) {
    def toDepParse = Set()
    def toConParse = s"(${intString(index)} ${intString(index)})"
  }

  abstract class BinaryEntry( i:Int, k:Int, j:Int ) extends Entry( i, j ) {
    val leftChild:Entry
    val rightChild:Entry
  }

  def findLeftRootChild( k:Int ):Entry
  def findRightRootChild( k:Int ):Entry

  def findLeftLeftwardChild( i:Int, k:Int ):Entry
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ):Entry

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ):Entry
  def findRightRightwardChild( k:Int, j:Int ):Entry

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ):Entry
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ):Entry


  def insertRootEntry( k:Int ) = treeRoot = RootEntry( k )
  def insertMEntry( i:Int, k:Int, j:Int, decoration:MDecoration ) =
    mTrace( i )( j )( decoration ) = MEntry( i , k, j, decoration )
  def insertLeftwardEntry( i:Int, k:Int, j:Int, pDec:Decoration, hV:Decoration, mDV:Decoration ) =
    headTrace( i )( j )( pDec ) = LeftwardEntry( i, k, j, hV, mDV )
  def insertRightwardEntry( i:Int, k:Int, j:Int, pDec:Decoration, hV:Decoration, mDV:Decoration ) =
    headTrace( i )( j )( pDec ) = RightwardEntry( i, k, j, hV, mDV )

  def insertLexEntry( index:Int, pDec:Decoration ) =
    headTrace( index )( index+1 )( pDec ) = LexEntry( index )

  case class RootEntry( k:Int ) extends BinaryEntry( 0, k, intString.length ) {
    val leftChild = findLeftRootChild( k )
    val rightChild = findRightRootChild( k )

    def toDepParse =
      Set( DirectedArc( intString.length/2, (k-1)/2 ) ) ++
        leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(*.${intString( k )} ${leftChild.toConParse} ${rightChild.toConParse} )"
  }

  case class MEntry(
    i:Int, k:Int, j:Int, decoration:MDecoration
  ) extends BinaryEntry( i, k, j ) {
    val leftChild = findLeftMChild( i, k, decoration )
    val rightChild = findRightMChild( k, j, decoration )

    def toDepParse = {
      decoration match {
        case LeftwardM => Set( DirectedArc( (j-1)/2, (i-1)/2 ) )
        case RightwardM => Set( DirectedArc( (i-1)/2, (j-1)/2 ) )
        case _ => Set()
      }
    } ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(M ${leftChild.toConParse} ${rightChild.toConParse} )"

  }

  case class LeftwardEntry( i:Int, k:Int, j:Int, hV:Decoration, mDV:Decoration ) extends BinaryEntry( i, k, j ) {
    val leftChild = findLeftLeftwardChild( i, k )
    val rightChild = findRightLeftwardChild( k, j, hV, mDV )

    def toDepParse =
      Set( DirectedArc( (j-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(L.${intString(j)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }

  case class RightwardEntry( i:Int, k:Int, j:Int, hV:Decoration, mDV:Decoration ) extends BinaryEntry( i, k, j ) {
    val leftChild = findLeftRightwardChild( i , k, hV, mDV )
    val rightChild = findRightRightwardChild( k, j )

    def toDepParse =
      Set( DirectedArc( (i-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(R.${intString(i)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }


  var treeRoot:RootEntry = null
  // no need for argMax or argSample for viterbiLexFill because the
  // child is *given* by the string (for now... )
  // def viterbiLexFill( index:Int ):Unit
  // def viterbiLexFill( index:Int ) {
  //   lexCellScores( index ).foreach{ case ( pDec, score ) =>
  //     insideChart( index )( index+1 )(pDec) = score
  //     headTrace( index )( index+1 )( pDec ) = LexEntry( index )
  //   }
  // }
  def viterbiLexFill( index:Int ) {
    lexSpecs( index ).foreach{ pDec =>
      insideChart( index )( index+1 )(pDec) = lexCellFactor( index, pDec )
      headTrace( index )( index+1 )( pDec ) = LexEntry( index )
    }
  }


  def viterbiSynFill( i:Int, j:Int ) {
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      rightwardCellScores( i, j ).foreach{ case( pDec, splitsAndScores ) =>
        val ( (k,mDec,cDec), bestScore ) = argMax( splitsAndScores )
        insideChart( i )( j )( pDec ) = bestScore
        headTrace( i )( j )( pDec ) =
          RightwardEntry( i, k, j, mDec.evenLeft, mDec.evenRight )
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward

      leftwardCellScores( i, j ).foreach{ case( pDec, splitsAndScores ) =>
        val ( (k,mDec,cDec), bestScore ) = argMax( splitsAndScores )
        insideChart( i )( j )( pDec ) = bestScore
        headTrace( i )( j )( pDec ) =
          LeftwardEntry( i, k, j, mDec.evenRight, mDec.evenLeft )
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M

      mCellScores(i,j).foreach{ case (mDecoration, splitsAndScores) =>
        val ( bestK, bestScore ) = argMax( splitsAndScores )
        insideChart( i )( j )( mDecoration ) = bestScore
        mTrace( i )( j )( mDecoration ) =
          MEntry( i, bestK, j, mDecoration )
      }
    } else {
      // Root

      if( i == 0 && j == intString.length ) {
        val ( (bestK,_), bestScore ) = argMax( rootCellScores() )
        stringProb = bestScore
        treeRoot = RootEntry( bestK )
      }
    }
  }

  def viterbiParse( utt:Utt ) = {
    clearCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse(
      utt.id,
      treeRoot.toConParse,
      treeRoot.toDepParse
    )
  }

  def viterbiDepParse( utt:Utt ) = {
    clearCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse( utt.id, "", treeRoot.toDepParse )
  }


  def clearCharts {
    (0 until 2*maxLength ).foreach{ i =>
      (0 until (2*maxLength)+1 ).foreach{ j =>
        insideChart(i)(j).keys.foreach{ vs =>
          insideChart(i)(j)(vs) = 0D
          outsideChart(i)(j)(vs) = 0D
        }
        if( i%2 != j%2 ) {
          headTrace(i)(j).clear
        } else if( i%2 == 1 && j%2 == 1 ) {
          mTrace(i)(j).clear
        }
      }
    }
    treeRoot = null
    stringProb = 0D
  }

  def sampleTreeCounts( i:Int, j:Int, pDec:Decoration ):Seq[Tuple2[Event,Double]] = {
    if( i%2 == 1 && j%2 == 1 ) { // M

      // val splitsAndScores =
      mCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
        assert( parent == pDec )
        val ( k, score ) = argSample( splitsAndScores )

        sampleScore += math.log( score )

        if( k%2 == 0 )
          mEventCounts( i, k, j, parent, 1D ) ++
            sampleTreeCounts( i, k, parent.evenLeft ) ++
              sampleTreeCounts( k, j, parent.evenRight )
        else
          mEventCounts( i, k, j, parent, 1D ) ++
            sampleTreeCounts( i, k, parent.oddLeft ) ++
              sampleTreeCounts( k, j, parent.oddRight )
      }

    } else if( i%2 == 1 ) { // Rightward

      // val splitsAndScores =
      if( j-i > 1 ) {
        rightwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , score ) = argSample( splitsAndScores )

          sampleScore += math.log( score )

          rightwardEventCounts( i, k, j, pDec, mDec, cDec, 1D ) ++
            sampleTreeCounts( i, k, mDec ) ++
              sampleTreeCounts( k, j, cDec )
        }
      } else {
        lexMarginals( j )
      }

    } else if( j%2 == 1 ) { // Leftward

      // val splitsAndScores =
      if( j-i > 1 ) {
        leftwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case ( parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , score ) = argSample( splitsAndScores )

          sampleScore += math.log( score )

          leftwardEventCounts( i, k, j, pDec, mDec, cDec, 1D ) ++
            sampleTreeCounts( i, k, cDec ) ++
              sampleTreeCounts( k, j, mDec )
        }
      } else {
        lexMarginals( i )
      }

    } else if( i == 0 && j == intString.length ) { // Root
      assert( pDec == RootDecoration )

      val ((k,cDecs), score) = argSample( rootCellScores() )

      sampleScore += math.log( score )

      rootEventCounts( k, 1D ) ++
        sampleTreeCounts( i , k, cDecs.evenLeft ) ++
        sampleTreeCounts( k, intString.length, cDecs.evenRight )

    } else {
      Seq()
    }
  }

  def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha )


  var sampleScore = 0D
  def sampleTreeCounts( utt:Utt ):Tuple2[DMVCounts,Double] = {
    val s = doubleString( utt.string )
    clearCharts
    val normalized = theta.fullyNormalized
    theta.fullyNormalized = true
    insidePass( s )
    val c = emptyCounts
    sampleScore = 0D
    sampleTreeCounts( 0, intString.length, RootDecoration ).foreach{ case (event, count) =>
      event match {
        case e:StopEvent => c.stopCounts.increment( e, count )
        case e:ChooseEvent => c.chooseCounts.increment( e, count )
        case e:RootEvent => c.rootCounts.increment( e, count )
      }
    }
    theta.fullyNormalized = normalized
    ( c, sampleScore )
  }


  def initialCounts( utts:List[Utt] ) = emptyCounts

  def logProb( string:Array[Int] ) = {
    // val s = string.flatMap{ w => Seq(w,w) }
    val s = doubleString( string )
    clearCharts
    theta.fullyNormalized = true
    insidePass( s )
    theta.fullyNormalized = false
    math.log(stringProb)
  }

  // training stuff

  def lexMarginals( index:Int ):Seq[Tuple2[Event,Double]]


  def rootEventCounts( k:Int, marginal:Double ):Seq[Tuple2[Event,Double]]
  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ):Seq[Tuple2[Event,Double]]
  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ):Seq[Tuple2[Event,Double]]
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:Double ):Seq[Tuple2[Event,Double]]
  def outsidePassWithCounts( s:Array[Int] ):DMVCounts = {
    val c = DMVCounts(
      new CPT[RootEvent]( rootAlpha ),
      new CPT[StopEvent]( stopAlpha ),
      new CPT[ChooseEvent]( chooseAlpha )
    )

    ( 1 to s.length ).reverse.foreach{ length =>
      ( 0 to ( s.length - length ) ).foreach{ i =>
        val j = i + length
        if( i%2 == 0 && j%2 == 0 ) { // Root
          if( i == 0 && j == s.length ) {
            rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
              val r = intString( k )
              val factorAndOutside = rootCellFactor( k )

              outsideChart( 0 )( k )( decorationPair.evenLeft ) +=
                insideChart( k )( intString.length )( decorationPair.evenRight ) * factorAndOutside

              outsideChart( k )( intString.length )( decorationPair.evenRight ) +=
                insideChart( 0 )( k )( decorationPair.evenLeft ) * factorAndOutside

              val marginal =
                insideChart( 0 )( k )( decorationPair.evenLeft ) *
                  insideChart( k )( intString.length )( decorationPair.evenRight ) *
                    factorAndOutside

              rootEventCounts( k, marginal ).foreach{ case (event, count) =>
                event match {
                  case e:StopEvent => c.stopCounts.increment( e, count )
                  case e:RootEvent => c.rootCounts.increment( e, count )
                }
              }
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( length > 1 ) {
            leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                // println( (i,k,j,pDec, mDec, cDec ) )
                val factorAndOutside =
                  outsideChart( i )( j )( pDec ) *
                    leftwardCellFactor( i, k, j, pDec, mDec, cDec )

                // to left child
                outsideChart( i )( k )( cDec ) +=
                  insideChart( k )( j )( mDec ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( mDec ) +=
                  insideChart( i )( k )( cDec ) *
                    factorAndOutside

                val marginal = 
                  insideChart( i )( k )( cDec ) *
                    insideChart( k )( j )( mDec ) *
                      factorAndOutside

                leftwardEventCounts( i, k, j, pDec, mDec, cDec, marginal ).foreach{ case (event, count) =>
                  event match {
                    case e:StopEvent => c.stopCounts.increment( e, count )
                    case e:ChooseEvent => c.chooseCounts.increment( e, count )
                  }
                }
              }
            }
          } else {
            lexMarginals( i ).foreach{ case (event, count) =>
              event match {
                case e:StopEvent => c.stopCounts.increment( e, count )
                case e:ChooseEvent => c.chooseCounts.increment( e, count )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( length > 1 ) {
            rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside =
                  outsideChart( i )( j )( pDec ) *
                    rightwardCellFactor( i, k, j, pDec, mDec, cDec )

                // to left child
                outsideChart( i )( k )( mDec ) +=
                  insideChart( k )( j )( cDec ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( cDec ) +=
                  insideChart( i )( k )( mDec ) *
                    factorAndOutside

                val marginal = 
                  insideChart( i )( k )( mDec ) *
                    insideChart( k )( j )( cDec ) *
                      factorAndOutside

                rightwardEventCounts( i, k, j, pDec, mDec, cDec, marginal ).foreach{ case (event, count) =>
                  event match {
                    case e:StopEvent => c.stopCounts.increment( e, count )
                    case e:ChooseEvent => c.chooseCounts.increment( e, count )
                  }
                }
              }
            }
          } else {
            lexMarginals( j ).foreach{ case (event, count) =>
              event match {
                case e:StopEvent => c.stopCounts.increment( e, count )
                case e:ChooseEvent => c.chooseCounts.increment( e, count )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          mSplitSpecs( i, j ).foreach{ case (mDecoration, splits) =>
            splits.foreach{ k =>
              val factorAndOutside =
                outsideChart( i )( j )( mDecoration ) * mCellFactor( i, k, j, mDecoration )

              if( k%2 == 0 ) {
                // to left child
                outsideChart( i )( k )( mDecoration.evenLeft ) +=
                  insideChart( k )( j )( mDecoration.evenRight ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( mDecoration.evenRight ) +=
                  insideChart( i )( k )( mDecoration.evenLeft ) *
                    factorAndOutside
              } else {
                // to left child
                outsideChart( i )( k )( mDecoration.oddLeft ) +=
                  insideChart( k )( j )( mDecoration.oddRight ) *
                    factorAndOutside

                // to right child
                outsideChart( k )( j )( mDecoration.oddRight ) +=
                  insideChart( i )( k )( mDecoration.oddLeft ) *
                    factorAndOutside
              }


              val marginal = 
                if( k%2 == 0 ) {
                  insideChart( i )( k )( mDecoration.evenLeft ) *
                    insideChart( k )( j )( mDecoration.evenRight ) *
                      factorAndOutside
                } else {
                  insideChart( i )( k )( mDecoration.oddLeft ) *
                    insideChart( k )( j )( mDecoration.oddRight ) *
                      factorAndOutside
                }

              mEventCounts( i, k, j, mDecoration, marginal ).foreach{ case (event, count) =>
                event match {
                  case e:StopEvent => c.stopCounts.increment( e, count )
                  case e:ChooseEvent => c.chooseCounts.increment( e, count )
                }
              }

            }
          }
        }
      }
    }

    c.rootCounts.divideBy( stringProb )
    c.stopCounts.divideBy( stringProb )
    c.chooseCounts.divideBy( stringProb )

    c
  }
  def populateChart( string:Array[Int] ) {
    // val s = string.flatMap{ w=> Seq(w,w) }
    val s = doubleString( string )
    clearCharts
    insidePass( s )
    outsidePass
  }

  def doubleString( string:Array[Int] ) = {
    string.toSeq.flatMap{ w => List(w,w) }.toArray
  }
  def extractPartialCounts( string:Array[Int] ) = {
    val s = doubleString( string )
    clearCharts
    insidePass( s )
    outsidePassWithCounts( s )
  }



    // // debugging stuff
    // def chartToString(
    //   label:String,
    //   chartToPrint:Array[Array[MMap[Decoration,Double]]],
    //   logSpace:Boolean = true
    // ) = {
    //   s"${label} Chart:\n\n" +
    //   (0 to (intString.length)).flatMap{ i =>
    //     ( (i+1) to intString.length ).map{ j =>
    //       if( chartToPrint(i)(j).size > 0 ) {
    //           (i,j) + chartToPrint(i)(j).map{ case (k,v) =>
    //             s"${k}: ${v}"
    //           }.mkString("\n\t", "\n\t", "\n")
    //       } else {
    //         ""
    //       }
    //     }.mkString("\n","","\n")
    //   }.mkString( "", "", "" )
    // }

    // def seeInsideHeads( logSpace:Boolean = true ) {
    //   println(
    //     chartToString( "Inside Heads", insideChart, logSpace )
    //   )
    // }



}

