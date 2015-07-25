package streamingDMV.parsers

import streamingDMV.tables.CPT
import streamingDMV.labels._
import streamingDMV.parameters.DMVParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class TopDownDMVParser(
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) {
  
  val rand = new util.Random( randomSeed )

  val theta = new DMVParameters( rootAlpha, stopAlpha, chooseAlpha )

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
  var stringProb = 0D

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
    def toConParse = s"(${vitString(index)} ${vitString(index)})"
  }

  abstract class BinaryEntry( i:Int, k:Int, j:Int ) extends Entry( i, j ) {
    val leftChild:Entry
    val rightChild:Entry
  }

  case class RootEntry( k:Int ) extends BinaryEntry( 0, k, vitString.length ) {
    val leftChild = headTrace( 0 )( k )( Outermost )
    val rightChild = headTrace( k )( vitString.length )( Outermost )

    def toDepParse =
      Set( DirectedArc( vitString.length/2, (k-1)/2 ) ) ++
        leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(*.${vitString( k )} ${leftChild.toConParse} ${rightChild.toConParse} )"
  }

  case class MEntry(
    i:Int, k:Int, j:Int,
    leftV:Valence, rightV:Valence
  ) extends BinaryEntry( i, k, j ) {
    val leftChild = headTrace( i )( k )( leftV )
    val rightChild = headTrace( k )( j )( rightV )

    def toDepParse = leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(M ${leftChild.toConParse} ${rightChild.toConParse} )"

  }

  // abstract class DirectedEntry( i:Int, k:Int, j:Int ) extends BinaryEntry( i, k, j )
  case class LeftwardEntry( i:Int, k:Int, j:Int ) extends BinaryEntry( i, k, j ) {
    val leftChild = headTrace( i )( k )( Outermost )
    val rightChild = mTrace( k )( j )( ( Outermost, Inner ) )

    def toDepParse =
      Set( DirectedArc( (j-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(L.${vitString(j)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }
  case class RightwardEntry( i:Int, k:Int, j:Int ) extends BinaryEntry( i, k, j ) {
    val leftChild = mTrace( i )( k )( ( Inner, Outermost ) )
    val rightChild = headTrace( k )( j )( ( Outermost ) )

    def toDepParse =
      Set( DirectedArc( (i-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(R.${vitString(i)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }



  val headTrace = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    MMap[Valence,Entry]()
  )
  val mTrace = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
      MMap[Tuple2[Valence,Valence],MEntry]()
  )

  def clearCharts {
    (0 until 2*maxLength ).foreach{ i =>
      (0 until (2*maxLength)+1 ).foreach{ j =>
        if( i%2 != j%2 ) {
          insideHeads(i)(j) += Outermost -> 0D
          insideHeads(i)(j) += Inner -> 0D

          outsideHeads(i)(j) += Outermost -> 0D
          outsideHeads(i)(j) += Inner -> 0D

          headTrace(i)(j).clear
        } else if( i%2 == 1 ) {
          insideM(i)(j)((Outermost,Inner)) = 0D
          insideM(i)(j)((Inner,Outermost)) = 0D

          outsideM(i)(j)((Outermost,Inner)) = 0D
          outsideM(i)(j)((Inner,Outermost)) = 0D

          mTrace(i)(j).clear
        } else {
          stringProb = 0D
        }
      }
    }
    doubledLength=0
  }

  def lexFill( index:Int, s:Array[Int] ) {
    // insideHeads(index)(index+1) = 1D
    val head = s( index )
    if( index %2 == 0 ) {
      insideHeads(index)(index+1)( Outermost ) =
        theta( StopEvent( head, LeftAtt, Outermost, Stop ) )

      if( index > 0 )
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, LeftAtt, Inner, Stop ) )
    } else {

      insideHeads(index)(index+1)( Outermost ) =
        theta( StopEvent( head, RightAtt, Outermost, Stop ) )

      if( index < s.length-1 )
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, RightAtt, Inner, Stop ) )
    }
  }

  var viterbiRoot:RootEntry = null
  var vitString = Array[Int]()
  def viterbiParse(
    // string:Array[Int]
    utt:Utt
  ) = {
    val s = utt.string.flatMap{ w=> Seq(w,w) }
    clearCharts
    vitString = s
    (1 to ( s.length )).foreach{ j =>
      viterbiLexFill( j-1, s )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j, s )
        }
    }
    Parse(
      utt.id,
      viterbiRoot.toConParse,
      viterbiRoot.toDepParse
    )
  }

  def viterbiDepParse(
    // string:Array[Int]
    utt:Utt
  ) = {
    val s = utt.string.flatMap{ w=> Seq(w,w) }
    clearCharts
    vitString = s
    (1 to ( s.length )).foreach{ j =>
      viterbiLexFill( j-1, s )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j, s )
        }
    }
    Parse(
      utt.id,
      "",
      viterbiRoot.toDepParse
    )
  }

  def synFill( i:Int, j:Int, s:Array[Int] ) {
    // println( s"${(i,j)}" )
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      val head = s( i )

      // Can only take Outermost dependents as children -- that's what Outermost means
      val cDV = Outermost
      val mDV = Outermost
      // hV must be Outermost if right edge index is string length
      val hVs = if( j == s.length ) Set( Outermost ) else Set( Outermost, Inner )

      ( (i+2) to (j-1) by 2 ).foreach{ k =>
        val dep = s( k )

        hVs.foreach{ hV =>
          insideHeads( i )( j )( hV ) +=
            insideHeads( k )( j )( cDV ) *
              insideM( i )( k )( (Inner, mDV) ) * // head child is always Inner
                theta( ChooseEvent( head, RightAtt, dep ) ) *
                theta( StopEvent( head, RightAtt, hV, NotStop ) )
        }
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward
      val head = s( j )
      ( (i+1) to (j-2) by 2 ).foreach{ k =>
        // println( s"\t${(i,k,j)}" )
        val dep = s( k )
        // Can only take Outermost dependents as children -- that's what Outermost means
        val cDV = Outermost
        val mDV = Outermost
        // hV must be Outermost if left edge index is 0
        val hVs = if( i == 0 ) Set( Outermost ) else Set( Outermost, Inner )

        hVs.foreach{ hV =>
          insideHeads( i )( j )( hV ) +=
            insideHeads( i )( k )( cDV ) *
              insideM( k )( j )( (mDV, Inner) ) * // head child is always Inner
                theta( ChooseEvent( head, LeftAtt, dep ) ) *
                theta( StopEvent( head, LeftAtt, hV, NotStop ) )
        }


      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M
      // println( s"${(i,j)}" )
      ( (i+1) to (j-1) by 2 ).foreach{ k =>
        // We can't have both children be Outermost or Inner -- one must be the dependent, one must
        // be the head child.
        insideM( i )( j )( Outermost, Inner ) +=
          insideHeads( i )( k )( Outermost ) *
            insideHeads( k )( j )( Inner )

        insideM( i )( j )( Inner, Outermost ) +=
          insideHeads( i )( k )( Inner ) *
            insideHeads( k )( j )( Outermost )
      }
    } else {
      // Root
      if( i == 0 && j == s.length )
        (1 to (j-1) by 2).foreach{ k =>
          val r = s( k )

          stringProb +=
            insideHeads(i)(k)(Outermost) *
              insideHeads(k)(j)(Outermost) *
                theta( RootEvent( r ) )
        }
    }
  }

  def viterbiLexFill( index:Int, s:Array[Int] ) {
    val head = s( index )
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

      if( index < s.length-1 ) {
        insideHeads(index)(index+1)( Inner ) =
          theta( StopEvent( head, RightAtt, Inner, Stop ) )
        headTrace(index)(index+1) += Inner -> LexEntry( index )
      }
    }
  }

  def argMax( seq:Iterable[Tuple2[Int,Double]] ):Tuple2[Int,Double] = {
    var bestIdx = 0::Nil
    var bestScore = 0D::Nil
    seq.foreach{ case ( idx, score ) =>
      if( score > bestScore.head ) {
        bestScore = score :: Nil
        bestIdx = idx :: Nil
      } else if( score == bestScore.head ) {
        bestScore = score :: bestScore
        bestIdx = idx :: bestIdx
      }
    }
    if( bestIdx.length == 1 ) {
      (bestIdx.head, bestScore.head)
    } else {
      val which = rand.nextInt( bestIdx.length )
      ( bestIdx(which), bestScore(which) )
    }
  }

  def viterbiSynFill( i:Int, j:Int, s:Array[Int] ) {
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      val head = s( i )

      val cDV = Outermost
      val mDV = Outermost
      val hVs = if( j == s.length ) Set( Outermost ) else Set( Outermost, Inner )

      hVs.foreach{ hV =>
        val expansions = ( (i+2) to (j-1) by 2 ).map{ k =>
          val dep = s( k )
          k -> {
            insideHeads( k )( j )( cDV ) *
              insideM( i )( k )( (Inner, mDV) ) *
                theta( ChooseEvent( head, RightAtt, dep ) ) *
                theta( StopEvent( head, RightAtt, hV, NotStop ) )
          }
        }

        val ( bestSplit, bestScore ) = argMax( expansions )
        insideHeads( i )( j )( hV ) = bestScore
        headTrace( i )( j )( hV ) = RightwardEntry( i, bestSplit, j )
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward
      val head = s( j )

      val cDV = Outermost
      val mDV = Outermost
      val hVs = if( i == 0 ) Set( Outermost ) else Set( Outermost, Inner )

      hVs.foreach{ hV =>
        val expansions = ( (i+1) to (j-2) by 2 ).map{ k =>
          val dep = s( k )
          k -> {
            insideHeads( i )( k )( cDV ) *
              insideM( k )( j )( (mDV, Inner) ) *
                theta( ChooseEvent( head, LeftAtt, dep ) ) *
                theta( StopEvent( head, LeftAtt, hV, NotStop ) )
          }
        }

        val ( bestSplit, bestScore ) = argMax( expansions )
        insideHeads( i )( j )( hV ) = bestScore
        headTrace( i )( j )( hV ) = LeftwardEntry( i, bestSplit, j )
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M
      Set( (Outermost,Inner), (Inner,Outermost) ).foreach{ case (leftV, rightV) =>
        val expansions = 
          ( (i+1) to (j-1) by 2 ).map{ k =>
            k -> {
              insideHeads( i )( k )( leftV ) *
                insideHeads( k )( j )( rightV )
            }
          }

        val (bestSplit, bestScore) = argMax( expansions )
        insideM( i )( j )( (leftV, rightV) ) = bestScore
        mTrace( i )( j )( (leftV, rightV ) ) = MEntry( i, bestSplit, j, leftV, rightV )
      }
    } else {
      // Root
      if( i == 0 && j == s.length ) {
        val expansions = 
          (1 to (j-1) by 2).map{ k =>
            val r = s( k )

            k -> {
              insideHeads(i)(k)(Outermost) *
                insideHeads(k)(j)(Outermost) *
                  theta( RootEvent( r ) )
            }
          }

        val (bestSplit, bestScore) = argMax( expansions )

        stringProb = bestScore
        viterbiRoot = RootEntry( bestSplit )
      }
    }
  }


  def chartToString(
    label:String,
    chartToPrint:Array[Array[MMap[Valence,Double]]],
    logSpace:Boolean = true
  ) = {
    s"${label} Chart:\n\n" +
    (0 to (doubledLength)).flatMap{ i =>
      ( (i+1) to doubledLength ).map{ j =>
        if( chartToPrint(i)(j).size > 0 ) {
            (i,j) + "\n\tOutermost: " + chartToPrint(i)(j)(Outermost) +
            "\n\tInner: " + chartToPrint(i)(j)(Inner) + "\n"
        } else {
          ""
        }
      }.mkString("\n","","\n")
    }.mkString( "", "", "" )
  }

  def seeInsideHeads( logSpace:Boolean = true ) {
    println(
      chartToString( "Inside Heads", insideHeads, logSpace )
    )
  }


  def outsidePass( s:Array[Int] ) = {
    ( 1 to s.length ).reverse.foreach{ length =>
      ( 0 to ( s.length - length ) ).foreach{ i =>
        val j = i + length
        if( i%2 == 0 && j%2 == 0 ) { // Root
          if( i == 0 && j == s.length ) {
            // (0 to ((s.length/2)-1)).foreach{ t =>
              // val k = 2*t + 1
            ( 1 to (s.length-1) by 2 ).foreach{ k =>
              val obs = s( k )

              val factor = theta( RootEvent( obs ) )

              outsideHeads( i )( k )( Outermost ) += 
                insideHeads( k )( j )( Outermost ) * factor

              outsideHeads( k )( j )( Outermost ) += 
                insideHeads( i )( k )( Outermost ) * factor

            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( j-i >= 3 ) {
            val head = s( j )
            ( (i+1) to (j-2) by 2 ).foreach{ k =>
              val dep = s( k )
              val cDV = Outermost
              val mDV = Outermost
              val hVs = if( j <= 3 ) Set( Outermost ) else Set( Outermost, Inner )
              // val hVs = Set( Outermost, Inner )

              hVs.foreach{ hV =>
                val factorAndOutside =
                  theta( ChooseEvent( head, LeftAtt, dep ) ) *
                    theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
                      outsideHeads( i )( j )( hV )

                // First, send messages to left child -- that is, the leftward looking dependent
                // label.
                outsideHeads( i )( k )( cDV ) +=
                    insideM( k )( j )( (mDV, Inner ) ) * factorAndOutside
                // Now, send messages to right child -- that is, the M-label

                outsideM( k )( j )( (mDV, Inner) ) +=
                    insideHeads( i )( k )( cDV ) * factorAndOutside
              }

            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( j-i >= 3 ) {
            val head = s( i )
            ( (i+2) to (j-1) by 2 ).foreach{ k =>
              val dep = s( k )
              val cDV = Outermost
              val mDV = Outermost
              val hVs = if( i >= ( s.length-3) ) Set( Outermost ) else Set( Outermost, Inner )
              // val hVs =  Set( Outermost, Inner )

              hVs.foreach{ hV =>
                val factorAndOutside =
                  theta( ChooseEvent( head, RightAtt, dep ) ) *
                    theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
                      outsideHeads( i )( j )( hV )

                // First, send messages to left child -- that is, the leftward looking dependent
                // label.
                outsideHeads( k )( j )( cDV ) +=
                    insideM( i )( k )( (Inner, mDV) ) * factorAndOutside
                // Now, send messages to right child -- that is, the M-label

                outsideM( i )( k )( (Inner, mDV) ) +=
                    insideHeads( k )( j )( cDV ) * factorAndOutside
              }

            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          ( (i+1) to (j-1) by 2 ).foreach{ k =>
            // Messages to left child:

            outsideHeads( i )( k )( Outermost ) +=
              outsideM( i )( j )( Outermost, Inner ) *
                insideHeads( k )( j )( Inner )

            outsideHeads( i )( k )( Inner ) +=
              outsideM( i )( j )( Inner, Outermost ) *
                insideHeads( k )( j )( Outermost )

            // Messages to right child:

            outsideHeads( k )( j )( Inner ) +=
              outsideM( i )( j )( Outermost, Inner ) *
                insideHeads( i )( k )( Outermost )

            outsideHeads( k )( j )( Outermost ) +=
              outsideM( i )( j )( Inner, Outermost ) *
                insideHeads( i )( k )( Inner )

          }
        }
      }
    }
  }

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
            // (0 to ((s.length/2)-1)).foreach{ t =>
              // val k = 2*t + 1
            ( 1 to (s.length-1) by 2 ).foreach{ k =>
              val obs = s( k )

              val factor = theta( RootEvent( obs ) )

              outsideHeads( i )( k )( Outermost ) += 
                insideHeads( k )( j )( Outermost ) * factor

              outsideHeads( k )( j )( Outermost ) += 
                insideHeads( i )( k )( Outermost ) * factor

              c.rootCounts.increment( RootEvent( obs ),
                insideHeads( i )( k )( Outermost ) *
                  insideHeads( k )( j )( Outermost ) *
                    factor
              )
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          val head = s( j )
          if( j-i >= 3 ) {
            ( (i+1) to (j-2) by 2 ).foreach{ k =>
              val dep = s( k )
              val cDV = Outermost
              val mDV = Outermost
              val hVs = if( j <= 3 ) Set( Outermost ) else Set( Outermost, Inner )
              // val hVs = Set( Outermost, Inner )

              hVs.foreach{ hV =>
                val factorAndOutside =
                  theta( ChooseEvent( head, LeftAtt, dep ) ) *
                    theta( StopEvent( head, LeftAtt, hV, NotStop ) ) *
                      outsideHeads( i )( j )( hV )

                // First, send messages to left child -- that is, the leftward looking dependent
                // label.
                outsideHeads( i )( k )( cDV ) +=
                    insideM( k )( j )( (mDV, Inner ) ) * factorAndOutside
                // Now, send messages to right child -- that is, the M-label

                outsideM( k )( j )( (mDV, Inner) ) +=
                    insideHeads( i )( k )( cDV ) * factorAndOutside


                val count = 
                  insideHeads( i )( k )( cDV ) *
                    insideM( k )( j )( (mDV, Inner ) ) *
                      factorAndOutside

                c.stopCounts.increment(
                  StopEvent( head, LeftAtt, hV, NotStop ),
                  count
                )

                c.chooseCounts.increment(
                  ChooseEvent( head, LeftAtt, dep ),
                  count
                )
              }
            }
          } else {
            Set( Outermost, Inner ).foreach{ hV =>
              c.stopCounts.increment(
                StopEvent( head, LeftAtt, hV, Stop ),
                insideHeads( i )( j )( hV ) * outsideHeads( i )( j )( hV )
              )
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          val head = s( i )
          if( j-i >= 3 ) {
            ( (i+2) to (j-1) by 2 ).foreach{ k =>
              val dep = s( k )
              val cDV = Outermost
              val mDV = Outermost
              val hVs = if( i >= ( s.length-3) ) Set( Outermost ) else Set( Outermost, Inner )
              // val hVs =  Set( Outermost, Inner )

              hVs.foreach{ hV =>
                val factorAndOutside =
                  theta( ChooseEvent( head, RightAtt, dep ) ) *
                    theta( StopEvent( head, RightAtt, hV, NotStop ) ) *
                      outsideHeads( i )( j )( hV )

                // First, send messages to left child -- that is, the leftward looking dependent
                // label.
                outsideHeads( k )( j )( cDV ) +=
                    insideM( i )( k )( (Inner, mDV) ) * factorAndOutside
                // Now, send messages to right child -- that is, the M-label

                outsideM( i )( k )( (Inner, mDV) ) +=
                    insideHeads( k )( j )( cDV ) * factorAndOutside

                val count = 
                  insideHeads( k )( j )( cDV ) *
                    insideM( i )( k )( (Inner, mDV) ) *
                      factorAndOutside

                c.stopCounts.increment(
                  StopEvent( head, RightAtt, hV, NotStop ),
                  count
                )

                c.chooseCounts.increment(
                  ChooseEvent( head, LeftAtt, dep ),
                  count
                )


              }

            }
          } else {
            Set( Outermost, Inner ).foreach{ hV =>
              c.stopCounts.increment(
                StopEvent( head, RightAtt, hV, Stop ),
                insideHeads( i )( j )( hV ) * outsideHeads( i )( j )( hV )
              )
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          ( (i+1) to (j-1) by 2 ).foreach{ k =>
            // Messages to left child:

            outsideHeads( i )( k )( Outermost ) +=
              outsideM( i )( j )( Outermost, Inner ) *
                insideHeads( k )( j )( Inner )

            outsideHeads( i )( k )( Inner ) +=
              outsideM( i )( j )( Inner, Outermost ) *
                insideHeads( k )( j )( Outermost )

            // Messages to right child:

            outsideHeads( k )( j )( Inner ) +=
              outsideM( i )( j )( Outermost, Inner ) *
                insideHeads( i )( k )( Outermost )

            outsideHeads( k )( j )( Outermost ) +=
              outsideM( i )( j )( Inner, Outermost ) *
                insideHeads( i )( k )( Inner )

          }
        }
      }
    }

    c.rootCounts.divideBy( stringProb )
    c.stopCounts.divideBy( stringProb )
    c.chooseCounts.divideBy( stringProb )

    c
  }


  def insidePass( s:Array[Int] ) = {
    (1 to ( s.length )).foreach{ j =>
      lexFill( j-1, s )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          synFill( i , j, s )
        }
    }
  }

  var doubledLength = 0
  def populateChart( string:Array[Int] ) {
    val s = string.flatMap{ w=> Seq(w,w) }
    clearCharts
    insidePass( s )
    outsidePass( s )
  }

  def extractPartialCounts( string:Array[Int] ) = {
    val s = string.flatMap{ w=> Seq(w,w) }
    clearCharts
    insidePass( s )
    outsidePassWithCounts( s )
  }

  def zerosInit( corpus:List[Utt] ) {
    theta.zerosInit( corpus )
  }

  def randomInit( corpus:List[Utt], seed:Int, scale:Int ) {
    theta.zerosInit( corpus )
    theta.randomizeCounts( seed, scale )
  }

  def miniBatchVB(
    miniBatch:List[Utt],
    maxIter:Int = 10,
    convergence:Double = 0.001,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ) = {

    var lastFHat = DMVCounts(
      new CPT[RootEvent]( rootAlpha ),
      new CPT[StopEvent]( stopAlpha ),
      new CPT[ChooseEvent]( chooseAlpha )
    )

    var lastMiniBatchScores = 1D
    var insideScores = 0D
    var deltaScores = 1D
    var iter = 0
    while(
      ( iter < maxIter || maxIter == 0 ) &&
      ( math.abs( deltaScores ) > convergence || convergence == 0 )
    ) {
      var thisMiniBatchScores = 0D


      val fHat = miniBatch.map{ s =>
        val counts = extractPartialCounts(s.string)
        thisMiniBatchScores += log( stringProb )
        counts
      }.reduce{ (a,b) => a.destructivePlus(b); a }

      if( iter > 0 ) theta.decrementCounts( lastFHat )
      theta.incrementCounts( fHat )

      deltaScores = ( lastMiniBatchScores - thisMiniBatchScores ) / lastMiniBatchScores
      if( printIterScores )
        println( s"$iter\t$thisMiniBatchScores\t$deltaScores" )


      lastFHat = fHat
      lastMiniBatchScores = thisMiniBatchScores

      iter += 1
    }
    if( printItersReached )
      println( s"iters\t$iter" )
  }


}


// vim: set ts=2 sw=2 et:
