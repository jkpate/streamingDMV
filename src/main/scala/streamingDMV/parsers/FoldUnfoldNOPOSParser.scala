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
) extends FoldUnfoldParser[DMVCounts,P]( maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed ) {

  // Inside-Outside definitions
  val insideHeads:Array[Array[MMap[Decoration,Double]]]
  val insideM:Array[Array[MMap[MDecoration,Double]]]
  val outsideHeads:Array[Array[MMap[Decoration,Double]]]
  val outsideM:Array[Array[MMap[MDecoration,Double]]]

  def lexFill( index:Int ):Unit
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

  def populateRightwardCell( i:Int, k:Int, j:Int ):Unit
  def populateLeftwardCell( i:Int, k:Int, j:Int ):Unit
  def populateMCell( i:Int, k:Int, j:Int ):Unit
  def populateRootCell( k:Int ):Unit

  def synFill( i:Int, j:Int ) {
    // println( s"${(i,j)}" )
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      ( (i+2) to (j-1) by 2 ).foreach{ k =>
        populateRightwardCell( i, k, j )
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward
      ( (i+1) to (j-2) by 2 ).foreach{ k =>
        populateLeftwardCell( i, k, j )
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M
      // println( s"${(i,j)}" )
      // ( (i+1) to (j-1) by 2 ).foreach{ k =>
      mSplits( i, j ).foreach{ k =>
        populateMCell( i, k, j )
      }
    } else {
      // Root
      if( i == 0 && j == intString.length )
        (1 to (j-1) by 2).foreach{ k =>
          populateRootCell( k )
        }
    }
  }

  def outsideRoot( k:Int ):Unit
  def outsideLeft( i:Int, k:Int, j:Int ):Unit
  def outsideRight( i:Int, k:Int, j:Int ):Unit
  def outsideM( i:Int, k:Int, j:Int ):Unit

  def outsidePass {
    ( 1 to intString.length ).reverse.foreach{ length =>
      ( 0 to ( intString.length - length ) ).foreach{ i =>
        val j = i + length
        if( i%2 == 0 && j%2 == 0 ) { // Root
          if( i == 0 && j == intString.length ) {
            ( 1 to (intString.length-1) by 2 ).foreach{ k =>
              outsideRoot( k )
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( j-i >= 3 ) {
            ( (i+1) to (j-2) by 2 ).foreach{ k =>
              outsideLeft( i, k, j )
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( j-i >= 3 ) {
            ( (i+2) to (j-1) by 2 ).foreach{ k =>
              outsideRight( i, k, j )
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          ( (i+1) to (j-1) by 2 ).foreach{ k =>
            outsideM( i, k, j )
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


  case class RootEntry( k:Int ) extends BinaryEntry( 0, k, intString.length ) {
    // val leftChild = headTrace( 0 )( k )( Outermost )
    // val rightChild = headTrace( k )( intString.length )( Outermost )
    val leftChild = findLeftRootChild( k )
    val rightChild = findRightRootChild( k )

    def toDepParse =
      Set( DirectedArc( intString.length/2, (k-1)/2 ) ) ++
        leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(*.${intString( k )} ${leftChild.toConParse} ${rightChild.toConParse} )"
  }

  case class MEntry(
    i:Int, k:Int, j:Int, decoration:MDecoration
    // leftV:Decoration, rightV:Decoration
  ) extends BinaryEntry( i, k, j ) {
    // val leftChild = headTrace( i )( k )( leftV )
    // val rightChild = headTrace( k )( j )( rightV )
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
    // val leftChild = headTrace( i )( k )( Outermost )
    // val rightChild = mTrace( k )( j )( ( Outermost, Inner ) )
    val leftChild = findLeftLeftwardChild( i, k )
    val rightChild = findRightLeftwardChild( k, j, hV, mDV )

    def toDepParse =
      Set( DirectedArc( (j-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(L.${intString(j)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }

  case class RightwardEntry( i:Int, k:Int, j:Int, hV:Decoration, mDV:Decoration ) extends BinaryEntry( i, k, j ) {
    // val leftChild = mTrace( i )( k )( ( Inner, Outermost ) )
    // val rightChild = headTrace( k )( j )( ( Outermost ) )
    val leftChild = findLeftRightwardChild( i , k, hV, mDV )
    val rightChild = findRightRightwardChild( k, j )

    def toDepParse =
      Set( DirectedArc( (i-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(R.${intString(i)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }


  var viterbiRoot:RootEntry = null
  def viterbiLexFill( index:Int ):Unit

  // def argMax[K]( seq:Iterable[Tuple2[K,Double]] ):Tuple2[K,Double] = {
  def argMax[K]( seq:Iterable[Tuple2[K,Double]] ):Tuple2[K,Double] = {
    var bestIdx = List[K]()
    // var bestScore = 0D::Nil
    var bestScore = Double.NegativeInfinity
    seq.foreach{ case ( idx, score ) =>
      if( score > bestScore ) {
        bestScore = score
        bestIdx = idx :: Nil
      } else if( score == bestScore ) {
        // bestScore = score
        bestIdx = idx :: bestIdx
      }
    }
    if( bestIdx.length == 1 ) {
      (bestIdx.head, bestScore)
    } else {
      if( bestIdx.length <= 0 ) {
        println( seq.mkString("\n" ) )
      }
      val which = rand.nextInt( bestIdx.length )
      ( bestIdx(which), bestScore )
    }
  }


  case class SplitSpec( k:Int, leftV:Decoration, rightV:Decoration )
  def viterbiRightRank( i:Int, j:Int, parentV:Decoration ):Iterable[Tuple2[SplitSpec,Double]]
  def viterbiLeftRank( i:Int, j:Int, parentV:Decoration ):Iterable[Tuple2[SplitSpec,Double]]
  def viterbiMRank( i:Int, j:Int, mDecoration:MDecoration ):Iterable[Tuple2[SplitSpec,Double]]
  def viterbiRootRank:Iterable[Tuple2[SplitSpec,Double]]

  def leftArcParentVs( i:Int ):Set[Decoration]
  def rightArcParentVs( j:Int ):Set[Decoration]
  def mNodeParentVs( i:Int, j:Int ):Set[MDecoration]

  def viterbiSynFill( i:Int, j:Int ) {
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      // val parentVs = if( j == intString.length ) Set( Outermost ) else Set( Outermost, Inner )

      rightArcParentVs(j).foreach{ parentV =>
        val expansions = viterbiRightRank( i, j, parentV )

        val ( bestSplit, bestScore ) = argMax( expansions )
        insideHeads( i )( j )( parentV ) = bestScore
        headTrace( i )( j )( parentV ) =
          RightwardEntry( i, bestSplit.k, j, bestSplit.leftV, bestSplit.rightV )
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward

      // val hVs = if( i == 0 ) Set( Outermost ) else Set( Outermost, Inner )

      leftArcParentVs(i).foreach{ parentV =>
        val expansions = viterbiLeftRank( i, j, parentV )

        val ( bestSplit, bestScore ) = argMax( expansions )
        insideHeads( i )( j )( parentV ) = bestScore
        headTrace( i )( j )( parentV ) =
          LeftwardEntry( i, bestSplit.k, j, bestSplit.rightV, bestSplit.leftV )
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M

      mNodeParentVs( i, j ).foreach{ mDecoration  =>
        // val (leftV, rightV) = vs
        val expansions = viterbiMRank( i, j, mDecoration )

        val (bestSplit, bestScore) = argMax( expansions )
        insideM( i )( j )( mDecoration ) = bestScore
        mTrace( i )( j )( mDecoration ) =
          MEntry( i, bestSplit.k, j, mDecoration )
      }
    } else {
      // Root
      if( i == 0 && j == intString.length ) {
        val expansions = viterbiRootRank

        val (bestSplit, bestScore) = argMax( expansions )

        stringProb = bestScore
        viterbiRoot = RootEntry( bestSplit.k )
      }
    }
  }

  def viterbiParse( utt:Utt ) = {
    clearCharts
    intString = utt.string.flatMap{ w => Seq(w,w) }
    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse(
      utt.id,
      viterbiRoot.toConParse,
      viterbiRoot.toDepParse
    )
  }

  def viterbiDepParse( utt:Utt ) = {
    clearCharts
    intString = utt.string.flatMap{ w => Seq(w,w) }
    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse( utt.id, "", viterbiRoot.toDepParse )
  }



  // Reset after each sentence
  def clearHeadCells( i:Int, j:Int ) {
    insideHeads(i)(j).keys.foreach{ v =>
      insideHeads(i)(j)(v) = 0D
      outsideHeads(i)(j)(v) = 0D
    }
  }
  def clearMCells( i:Int, j:Int ) {
    insideM(i)(j).keys.foreach{ vs =>
      insideM(i)(j)(vs) = 0D
      outsideM(i)(j)(vs) = 0D
    }
  }

  def clearCharts {
    (0 until 2*maxLength ).foreach{ i =>
      (0 until (2*maxLength)+1 ).foreach{ j =>
        if( i%2 != j%2 ) {
          clearHeadCells( i, j )
          headTrace(i)(j).clear
        } else if( i%2 == 1 ) {
          clearMCells( i, j )
          mTrace(i)(j).clear
        }
      }
    }
    stringProb = 0D
  }

  def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha )
  def initialCounts( utts:List[Utt] ) = emptyCounts

  def logProb( string:Array[Int] ) = {
    val s = string.flatMap{ w => Seq(w,w) }
    clearCharts
    theta.fullyNormalized = true
    insidePass( s )
    theta.fullyNormalized = false
    math.log(stringProb)
  }

  // training stuff

  def outsideRootWithMarginals( k:Int ):Seq[Tuple2[Event,Double]]
  def outsideLeftWithMarginals( i:Int, k:Int, j:Int ):Seq[Tuple2[Event,Double]]
  def outsideRightWithMarginals( i:Int, k:Int, j:Int ):Seq[Tuple2[Event,Double]]
  def outsideMWithMarginals( i:Int, k:Int, j:Int ):Seq[Tuple2[Event,Double]]
  def lexMarginals( index:Int ):Seq[Tuple2[Event,Double]]

  def mSplits( i:Int, j:Int ):Iterable[Int]


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
            ( 1 to (s.length-1) by 2 ).foreach{ k =>
              outsideRootWithMarginals( k ).foreach{ case ( event, count ) =>
                event match {
                  case e:StopEvent => c.stopCounts.increment( e, count )
                  case e:RootEvent => c.rootCounts.increment( e, count )
                }
              }
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( length > 1 ) {
            ( (i+1) to (j-2) by 2 ).foreach{ k =>
              outsideLeftWithMarginals( i, k, j ).foreach{ case (event, count) =>
                event match {
                  case e:StopEvent => c.stopCounts.increment( e, count )
                  case e:ChooseEvent => c.chooseCounts.increment( e, count )
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
            ( (i+2) to (j-1) by 2 ).foreach{ k =>
              outsideRightWithMarginals( i, k, j ).foreach{ case (event, count) =>
                event match {
                  case e:StopEvent => c.stopCounts.increment( e, count )
                  case e:ChooseEvent => c.chooseCounts.increment( e, count )
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
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          // ( (i+1) to (j-1) by 2 ).foreach{ k =>
          mSplits(i,j).foreach{ k =>
            outsideMWithMarginals( i, k, j ).foreach{ case (event, count) =>
              event match {
                case e:StopEvent => c.stopCounts.increment( e, count )
                case e:ChooseEvent => c.chooseCounts.increment( e, count )
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
    val s = string.flatMap{ w=> Seq(w,w) }
    clearCharts
    insidePass( s )
    outsidePass
  }

  def extractPartialCounts( string:Array[Int] ) = {
    val s = string.flatMap{ w=> Seq(w,w) }
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
    //     chartToString( "Inside Heads", insideHeads, logSpace )
    //   )
    // }



}

