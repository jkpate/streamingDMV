package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.math.LogSum
import streamingDMV.tables.LogCPT
// import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.parameters.ArcFactoredParameters


import scala.collection.mutable.{Map=>MMap}
import math.log


// abstract class FoldUnfoldNOPOSParser[C<:DependencyCounts,P<:NOPOSArcFactoredParameters](
abstract class FoldUnfoldNOPOSParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // randomSeed:Int = 15,
  // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends StreamingVBParser[C,P](
  // maxLength,
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha,
  // randomSeed,
  // reservoirSize
  parserSpec
) {

  // val approximate:Boolean

  var insideChart:Array[Array[MMap[Decoration,Double]]] = Array()
  var outsideChart:Array[Array[MMap[Decoration,Double]]] = Array()

  def cellMap( i:Int, j:Int ):MMap[Decoration,Double]

  def buildCharts( length:Int ) {
    insideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
    outsideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
  }

  def lexSpecs( index:Int ):Seq[Decoration]
  def lexCellFactor( index:Int, pDec:Decoration ):Double
  def lexFill( index:Int ) {
    // println( s"visiting index $index" )
    lexSpecs( index ).foreach{ pDec =>
        // println( s"incrementing ${(index,index+1,pDec)} by ${lexCellFactor( index, pDec )}" )
        // if( LogSum(
        //     insideChart( index )( index+1 )(pDec),
        //     lexCellFactor( index, pDec )
        //   ) > 0
        // ) {
        //   println( (index,pDec) )
        //   println( s"adding ${lexCellFactor( index, pDec )} to ${insideChart( index )( index+1)(pDec)}")
        // }

      insideChart( index )( index+1 )(pDec) = myPlus(
        insideChart( index )( index+1 )(pDec),
        lexCellFactor( index, pDec )
      )
      if( !( insideChart( index )( index+1 )(pDec) <= myOne +0.000001 ) ) {
        println( (index, lexCellFactor( index, pDec ), insideChart( index )( index+1 )(pDec) ) )
      }
      assert( insideChart( index )( index+1 )(pDec) <= myOne +0.000001 )
    }
  }
  def insidePass( s:Array[Int] ) = {
    intString = s
    if( intString.length > insideChart.length )
      buildCharts( intString.length )

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
    myTimes(
      insideChart( i )( k )( mDec ) ,
        insideChart( k )( j )( cDec ) ,
          rightwardCellFactor( i, k, j, pDec, mDec, cDec )
    )
  }
  def leftwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    // println( s"} ${(i,k,j,pDec,mDec,cDec)}" )
    // println( s" ]${(i,k,cDec)}} " + insideChart( i )( k )( cDec ) )
    // println( s" ]${(k,j,mDec)}} " + insideChart( k )( j )( mDec ) )
    // println( " ]} " + leftwardCellFactor( i, k, j, pDec, mDec, cDec ) )
    myTimes(
      insideChart( i )( k )( cDec ) ,
        insideChart( k )( j )( mDec ) ,
          leftwardCellFactor( i, k, j, pDec, mDec, cDec )
    )
  }
  def mCellScore( i:Int, k:Int, j:Int, mDecoration:MDecoration ) = {
    val score = 
      if( k%2 == 0 ) {
        myTimes( 
          insideChart( i )( k )( mDecoration.evenLeft ),
            insideChart( k )( j )( mDecoration.evenRight ),
              mCellFactor( i, k, j, mDecoration )
        )
      } else {
        myTimes(
          insideChart( i )( k )( mDecoration.oddLeft ),
            insideChart( k )( j )( mDecoration.oddRight ),
              mCellFactor( i, k, j, mDecoration )
        )
      }

    if( !( score > myZero && score <= myOne ) ) {
      println( s"mCellScore: ${(i,k,j,mDecoration,score)}" )
      if( k%2 == 0 ) {
        println( "  left:" + insideChart( i )( k )( mDecoration.evenLeft ) )
        println( "  right:" + insideChart( k )( j )( mDecoration.evenRight ) )
      } else {
        println( "  left:" + insideChart( i )( k )( mDecoration.oddLeft ) )
        println( "  right:" + insideChart( k )( j )( mDecoration.oddRight ) )
      }
      println( "  factor:" + mCellFactor( i, k, j, mDecoration ) )
    }

    score
  }
  def rootCellScore( k:Int, leftDec:Decoration, rightDec:Decoration ) = {
    myTimes(
      insideChart( 0 )( k )( leftDec ),
        insideChart( k )( intString.length )( rightDec ),
          rootCellFactor( k )
    )
  }


  def computeInsideMScore( i:Int, j:Int ) {
    // println( s"?> ${(i,j)}" )
    mSplitSpecs(i,j).foreach{ case (mDecoration, splits) =>
      // println( s"=>- ${(i,j,mDecoration,splits.mkString(" "))}" )
      splits.foreach{ k =>

            // if( !( myPlus(
            //     insideChart( i )( j )( mDecoration ),
            //     mCellScore( i, k, j, mDecoration )
            //   ) <= myOne ) ) {
            //   println( s"BEFORE FAIL" )
            //   println( (i,j,mDecoration ) )
            //   println( s"${insideChart( i )( j )( mDecoration ) } <= $myOne = " +
            //     { insideChart( i )( j )( mDecoration ) <= myOne } )

            //   println( s"  ${(i,k,j)}" )
            //   println(  "    mCellScore: " + mCellScore( i, k, j, mDecoration ) )
            //   println(  "    mCellFactor: " + mCellFactor( i, k, j, mDecoration ) )
            //   if( k%2 == 0 ) {
            //     println(  "    left: " + insideChart( i )( k )( mDecoration.evenLeft ) )
            //     println(  "    right: " + insideChart( k )( j )( mDecoration.evenRight ) )
            //   } else {
            //     println(  "    left: " + insideChart( i )( k )( mDecoration.oddLeft ) )
            //     println(  "    right: " + insideChart( k )( j )( mDecoration.oddRight ) )
            //   }
            // }

            // println( s"<${(i,k,j,mDecoration)})>" )
        insideChart( i )( j )( mDecoration ) = myPlus(
            insideChart( i )( j )( mDecoration ),
            mCellScore( i, k, j, mDecoration )
          )

          // if( !( insideChart( i )( j )( mDecoration ) > myZero )) {
          //   println( (i,j,mDecoration,insideChart( i )( j )( mDecoration ) ) )
          // }

        assert( insideChart( i )( j )( mDecoration ) > myZero )

            // if( !( insideChart( i )( j )( mDecoration ) <= myOne ) ) {
            //   println( (i,j,mDecoration ) )
            //   println( s"${insideChart( i )( j )( mDecoration ) } <= $myOne = " +
            //     { insideChart( i )( j )( mDecoration ) <= myOne } )

            //   println( s"  ${(i,k,j)}" )
            //   println(  "    mCellScore: " + mCellScore( i, k, j, mDecoration ) )
            //   println(  "    mCellFactor: " + mCellFactor( i, k, j, mDecoration ) )
            //   if( k%2 == 0 ) {
            //     println(  "    left: " + insideChart( i )( k )( mDecoration.evenLeft ) )
            //     println(  "    right: " + insideChart( k )( j )( mDecoration.evenRight ) )
            //   } else {
            //     println(  "    left: " + insideChart( i )( k )( mDecoration.oddLeft ) )
            //     println(  "    right: " + insideChart( k )( j )( mDecoration.oddRight ) )
            //   }
            // }

        assert( insideChart( i )( j )( mDecoration ) <= myOne )

      }
    }
  }
  def computeInsideRootScore() {
    rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
      val r = intString( k )

      stringProb = myPlus(
        stringProb,
        rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight )
      )

      if( !( stringProb > myZero )) {
        println( stringProb )
      }
      assert( stringProb > myZero )
      assert( stringProb <= myOne )
    }
  }
  def computeInsideRightwardScore( i:Int, j:Int ) {
    rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>

        insideChart( i )( j )( pDec ) = myPlus(
            insideChart( i )( j )( pDec ),
            rightwardCellScore( i, k, j, pDec, mDec, cDec )
          )

        if( !( insideChart( i )( j )( pDec ) > myZero ) ) {
          println( (i,j,pDec, insideChart( i )( j )( pDec ) ) )
        }
        assert( insideChart( i )( j )( pDec ) > myZero )
        assert( insideChart( i )( j )( pDec ) <= myOne )

      }
    }
  }
  def computeInsideLeftwardScore( i:Int, j:Int ) {
    leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        // println( (i,k,j,mDec,cDec) )
        insideChart( i )( j )( pDec ) = myPlus(
            insideChart( i )( j )( pDec ),
            leftwardCellScore( i, k, j, pDec, mDec, cDec )
          )

        if( !( insideChart( i )( j )( pDec ) > myZero &&
                insideChart( i )( j )( pDec ) <= myOne ) ) {
          println( "! " + (i,j,pDec, insideChart( i )( j )( pDec ) ) )
          println( "! " + leftwardCellScore( i, k, j, pDec, mDec, cDec ) )
        }
        assert( insideChart( i )( j )( pDec ) > myZero )
        assert( insideChart( i )( j )( pDec ) <= myOne )
      }
    }
  }

  def rootCellFactor( k:Int ):Double
  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ):Double
  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ):Double
  def mCellFactor( i:Int, k:Int, j:Int, mDecoration:MDecoration ):Double

  def lexCellScores( index:Int ) = {
    lexSpecs( index ).map{ pDec =>
        // println( "lexCellScores " + index + " " + pDec )
      (
        pDec, Seq( lexCellFactor( index, pDec ) )
      )
    }
  }
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
      // println( (0,k,intString.length), decorationPair, )
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

              outsideChart( 0 )( k )( decorationPair.evenLeft ) = myPlus(
                  outsideChart( 0 )( k )( decorationPair.evenLeft ) ,
                  myTimes(
                    insideChart( k )( intString.length )( decorationPair.evenRight ),
                    factorAndOutside
                  )
                )

              outsideChart( k )( intString.length )( decorationPair.evenRight ) = myPlus(
                outsideChart( k )( intString.length )( decorationPair.evenRight ),
                myTimes(
                  insideChart( 0 )( k )( decorationPair.evenLeft ), factorAndOutside
                )
              )
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( j-i >= 3 ) {
            leftwardSplitSpecs( i, j ).foreach{ case ( decoration, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside = myTimes(
                  outsideChart( i )( j )( decoration ),
                    leftwardCellFactor( i, k, j, decoration, mDec, cDec )
                )

                // to left child
                outsideChart( i )( k )( cDec ) = myPlus(
                    outsideChart( i )( k )( cDec ),
                    myTimes(
                      insideChart( k )( j )( mDec ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( mDec ) = myPlus(
                    outsideChart( k )( j )( mDec ),
                    myTimes(
                      insideChart( i )( k )( cDec ),
                        factorAndOutside
                    )
                  )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( j-i >= 3 ) {
            rightwardSplitSpecs( i, j ).foreach{ case ( decoration, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside = myTimes(
                  outsideChart( i )( j )( decoration ),
                    rightwardCellFactor( i, k, j, decoration, mDec, cDec )
                )

                // to left child
                outsideChart( i )( k )( mDec ) = myPlus(
                    outsideChart( i )( k )( mDec ),
                    myTimes(
                      insideChart( k )( j )( cDec ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( cDec ) = myPlus(
                    outsideChart( k )( j )( cDec ),
                    myTimes(
                      insideChart( i )( k )( mDec ),
                        factorAndOutside
                    )
                  )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          mSplitSpecs( i, j ).foreach{ case (mDecoration, splits) =>
            splits.foreach{ k =>
              val factorAndOutside = myTimes(
                outsideChart( i )( j )( mDecoration ), mCellFactor( i, k, j, mDecoration )
              )

              if( k%2 == 0 ) {
                // to left child
                outsideChart( i )( k )( mDecoration.evenLeft ) = myPlus(
                    outsideChart( i )( k )( mDecoration.evenLeft ),
                    myTimes(
                      insideChart( k )( j )( mDecoration.evenRight ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( mDecoration.evenRight ) = myPlus(
                    outsideChart( k )( j )( mDecoration.evenRight ),
                    myTimes(
                      insideChart( i )( k )( mDecoration.evenLeft ),
                        factorAndOutside
                    )
                  )
              } else {
                // to left child
                outsideChart( i )( k )( mDecoration.oddLeft ) = myPlus(
                    outsideChart( i )( k )( mDecoration.oddLeft ),
                    myTimes(
                      insideChart( k )( j )( mDecoration.oddRight ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( mDecoration.oddRight ) = myPlus(
                    outsideChart( k )( j )( mDecoration.oddRight ),
                    myTimes(
                      insideChart( i )( k )( mDecoration.oddLeft ),
                        factorAndOutside
                    )
                  )
              }
            }
          }
        }
      }
    }
  }



  // Viterbi definitions

  var headTrace = Array[Array[MMap[Decoration,Entry]]]()
    // = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    //   MMap[Decoration,Entry]()
    // )
  var mTrace = Array[Array[MMap[MDecoration,MEntry]]]()
    // = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    //   MMap[MDecoration,MEntry]()
    // )

  // def vitCellMap( i:Int, j:Int ):MMap[Decoration,Double]

  def buildVitCharts( length:Int ) {
    insideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
    outsideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
    headTrace = Array.tabulate( length, length+1 )( (i,j) => MMap[Decoration,Entry]() )
    mTrace = Array.tabulate( length, length+1 )( (i,j) => MMap[MDecoration,MEntry]() )
  }

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
    // println( (i,k,j,hV,mDV) )
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
    clearVitCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    if( intString.length > headTrace.length ) {
      buildVitCharts( intString.length )
    }
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
    clearVitCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    if( intString.length > headTrace.length ) {
      buildVitCharts( intString.length )
    }
    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse( utt.id, "", treeRoot.toDepParse )
  }


  def clearVitCharts {
    (0 until headTrace.length ).foreach{ i =>
      (0 until headTrace.length+1 ).foreach{ j =>
        if( i%2 != j%2 ) {
          headTrace(i)(j).clear
        } else if( i%2 == 1 && j%2 == 1 ) {
          mTrace(i)(j).clear
        }
      }
    }
    treeRoot = null
    stringProb = myZero
  }
  def clearCharts {
    (0 until insideChart.length ).foreach{ i =>
      ((i+1) until insideChart.length+1 ).foreach{ j =>
        // insideChart(i)(j) = cellMap( i, j )
        // outsideChart(i)(j) = cellMap( i, j )
        // insideChart(i)(j).mapValues( _ => myZero )
        // outsideChart(i)(j).mapValues( _ => myZero )
        insideChart(i)(j).keys.foreach{ vs =>
          insideChart(i)(j)(vs) = myZero
          outsideChart(i)(j)(vs) = myZero
        }
      }
    }
    // buildCharts( insideChart.length-1 )
    treeRoot = null
    stringProb = myZero
  }

  def sampleTreeCounts( i:Int, j:Int, pDec:Decoration ):Seq[Tuple2[Event,Double]] = {
    if( i%2 == 1 && j%2 == 1 ) { // M

      // val splitsAndScores =
      mCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
        assert( parent == pDec )
        val ( k, score ) = argSample( splitsAndScores, logSpace = logSpace )

        sampleScore = myTimes( sampleScore, score )

        if( k%2 == 0 )
          mEventCounts( i, k, j, parent, myOne ) ++
            sampleTreeCounts( i, k, parent.evenLeft ) ++
              sampleTreeCounts( k, j, parent.evenRight )
        else
          mEventCounts( i, k, j, parent, myOne ) ++
            sampleTreeCounts( i, k, parent.oddLeft ) ++
              sampleTreeCounts( k, j, parent.oddRight )
      }

    } else if( i%2 == 1 ) { // Rightward

      // val splitsAndScores =
      if( j-i > 1 ) {
        rightwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , score ) = argSample( splitsAndScores, logSpace = logSpace )

          sampleScore = myTimes( sampleScore, score )

          rightwardEventCounts( i, k, j, pDec, mDec, cDec, myOne ) ++
            sampleTreeCounts( i, k, mDec ) ++
              sampleTreeCounts( k, j, cDec )
        }
      } else {
        // lexMarginals( j )
        lexCellScores( i ).filter( _._1 == pDec ).flatMap{ case ( parent, scores ) =>
          assert( parent == pDec )
          assert( scores.length == 1 )

          sampleScore = myTimes( sampleScore, scores.head )
          // Seq()
          lexEventCounts( i, pDec, myOne )
        }
      }

    } else if( j%2 == 1 ) { // Leftward

      // val splitsAndScores =
      if( j-i > 1 ) {
        leftwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case ( parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , score ) = argSample( splitsAndScores, logSpace = logSpace )

          sampleScore = myTimes( sampleScore, score )

          leftwardEventCounts( i, k, j, pDec, mDec, cDec, myOne ) ++
            sampleTreeCounts( i, k, cDec ) ++
              sampleTreeCounts( k, j, mDec )
        }
      } else {
        // lexMarginals( i )
        lexCellScores( i ).filter( _._1 == pDec ).flatMap{ case ( parent, scores ) =>
          assert( parent == pDec )
          assert( scores.length == 1 )

          sampleScore = myTimes( sampleScore, scores.head )
          lexEventCounts( i, pDec, myOne )
        }
      }

    } else if( i == 0 && j == intString.length ) { // Root
      assert( pDec == RootDecoration )

      val ((k,cDecs), score) = argSample( rootCellScores(), logSpace = logSpace )

      sampleScore = myTimes( sampleScore, score )

      rootEventCounts( k, myOne ) ++
        sampleTreeCounts( i , k, cDecs.evenLeft ) ++
        sampleTreeCounts( k, intString.length, cDecs.evenRight )

    } else {
      Seq()
    }
  }

  // def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha, true )


  var sampleScore = myOne

  def sampleTreeCounts( originalString:Array[Int] ):Tuple2[C,Double] = {
    val s = doubleString( originalString )
    clearCharts
    val normalized = theta.fullyNormalized
    theta.fullyNormalized = true
    insidePass( s )
    val c = emptyCounts
    sampleScore = myOne
    // println( "incrementing counts for sample" )
    sampleTreeCounts( 0, intString.length, RootDecoration ).foreach{ case (event, count) =>
      // println( s"increment $event by $count (${math.exp(count)})" )
      // event match {
      //   case e:StopEvent => c.stopCounts.increment( e, count )
      //   case e:ChooseEvent => c.chooseCounts.increment( e, count )
      //   case e:RootEvent => c.rootCounts.increment( e, count )
      // }
      c.increment( event, count )
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
    // math.log(stringProb)
    if( logSpace )
      stringProb
    else
      log( stringProb )
  }

  // training stuff

  // def lexMarginals( index:Int ):Seq[Tuple2[Event,Double]]


  def rootEventCounts( k:Int, marginal:Double ):Seq[Tuple2[Event,Double]]
  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ):Seq[Tuple2[Event,Double]]
  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ):Seq[Tuple2[Event,Double]]
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:Double ):Seq[Tuple2[Event,Double]]
  def lexEventCounts( index:Int, pDec:Decoration, marginal:Double ):Seq[Tuple2[Event,Double]]

  def outsidePassWithCounts( s:Array[Int] ):C = {
    // println( s"creating counts in outsidePassWithCounts" )
    // val c = DMVCounts(
    //   new LogCPT[RootEvent]( rootAlpha ),
    //   new LogCPT[StopEvent]( stopAlpha ),
    //   new LogCPT[ChooseEvent]( chooseAlpha )
    // )

    val c = emptyCounts

    ( 1 to s.length ).reverse.foreach{ length =>
      ( 0 to ( s.length - length ) ).foreach{ i =>
        val j = i + length
        if( i%2 == 0 && j%2 == 0 ) { // Root
          if( i == 0 && j == s.length ) {
            rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
              val r = intString( k )
              val factorAndOutside = rootCellFactor( k )

              outsideChart( 0 )( k )( decorationPair.evenLeft ) = myPlus(
                outsideChart( 0 )( k )( decorationPair.evenLeft ),
                myTimes(
                  insideChart( k )( intString.length )( decorationPair.evenRight ),
                  factorAndOutside
                )
              )

              outsideChart( k )( intString.length )( decorationPair.evenRight ) = myPlus(
                outsideChart( k )( intString.length )( decorationPair.evenRight ) ,
                myTimes(
                  insideChart( 0 )( k )( decorationPair.evenLeft ),
                  factorAndOutside
                )
              )

              val marginal = myTimes(
                insideChart( 0 )( k )( decorationPair.evenLeft ),
                  insideChart( k )( intString.length )( decorationPair.evenRight ),
                    factorAndOutside
              )

              rootEventCounts( k, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( length > 1 ) {
            leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside = myTimes(
                  outsideChart( i )( j )( pDec ),
                    leftwardCellFactor( i, k, j, pDec, mDec, cDec )
                )

                // to left child
                outsideChart( i )( k )( cDec ) = myPlus(
                    outsideChart( i )( k )( cDec ),
                    myTimes(
                      insideChart( k )( j )( mDec ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( mDec ) = myPlus(
                    outsideChart( k )( j )( mDec ),
                    myTimes(
                      insideChart( i )( k )( cDec ),
                        factorAndOutside
                    )
                  )

                val marginal = myTimes(
                  insideChart( i )( k )( cDec ),
                    insideChart( k )( j )( mDec ),
                      factorAndOutside
                )

                leftwardEventCounts( i, k, j, pDec, mDec, cDec, marginal ).foreach{ case (event, count) =>
                  c.increment( event, count )
                  // event match {
                  //   case e:StopEvent => c.stopCounts.increment( e, count )
                  //   case e:ChooseEvent => c.chooseCounts.increment( e, count )
                  // }
                }
              }
            }
          } else {
            lexSpecs( i ).foreach{ pDec =>
              // Don't include stop factor -- it's actually already included in insideChart (the
              // *real* inside score for each terminal is 1)
              val marginal = myTimes(
                insideChart(i)(i+1)(pDec),
                outsideChart(i)(i+1)(pDec)
              )

              lexEventCounts( i, pDec, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( length > 1 ) {
            rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                val factorAndOutside = myTimes(
                  outsideChart( i )( j )( pDec ),
                    rightwardCellFactor( i, k, j, pDec, mDec, cDec )
                )

                // to left child
                outsideChart( i )( k )( mDec ) = myPlus(
                    outsideChart( i )( k )( mDec ),
                    myTimes(
                      insideChart( k )( j )( cDec ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( cDec ) = myPlus(
                    outsideChart( k )( j )( cDec ),
                    myTimes(
                      insideChart( i )( k )( mDec ),
                        factorAndOutside
                    )
                  )

                val marginal = myTimes(
                  insideChart( i )( k )( mDec ),
                    insideChart( k )( j )( cDec ),
                      factorAndOutside
                )

                rightwardEventCounts( i, k, j, pDec, mDec, cDec, marginal ).foreach{ case (event, count) =>
                  c.increment( event, count )
                }
              }
            }
          } else {
            lexSpecs( i ).foreach{ pDec =>
              // Don't include stop factor -- it's actually already included in insideChart (the
              // *real* inside score for each terminal is 1)
              val marginal = myTimes(
                insideChart(i)(i+1)(pDec),
                outsideChart(i)(i+1)(pDec)
              )

              lexEventCounts( i, pDec, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          mSplitSpecs( i, j ).foreach{ case (mDecoration, splits) =>
            splits.foreach{ k =>
              val factorAndOutside = myTimes(
                outsideChart( i )( j )( mDecoration ),
                mCellFactor( i, k, j, mDecoration )
              )

              if( k%2 == 0 ) {
                // to left child
                outsideChart( i )( k )( mDecoration.evenLeft ) = myPlus(
                    outsideChart( i )( k )( mDecoration.evenLeft ),
                    myTimes(
                      insideChart( k )( j )( mDecoration.evenRight ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( mDecoration.evenRight ) = myPlus(
                    outsideChart( k )( j )( mDecoration.evenRight ),
                    myTimes(
                      insideChart( i )( k )( mDecoration.evenLeft ),
                        factorAndOutside
                    )
                  )
              } else {
                // to left child
                outsideChart( i )( k )( mDecoration.oddLeft ) = myPlus(
                    outsideChart( i )( k )( mDecoration.oddLeft ),
                    myTimes(
                      insideChart( k )( j )( mDecoration.oddRight ),
                        factorAndOutside
                    )
                  )

                // to right child
                outsideChart( k )( j )( mDecoration.oddRight ) = myPlus(
                    outsideChart( k )( j )( mDecoration.oddRight ),
                    myTimes(
                      insideChart( i )( k )( mDecoration.oddLeft ),
                        factorAndOutside
                    )
                  )
              }


              val marginal = 
                if( k%2 == 0 ) {
                  myTimes(
                    insideChart( i )( k )( mDecoration.evenLeft ),
                      insideChart( k )( j )( mDecoration.evenRight ),
                        factorAndOutside
                  )
                } else {
                  myTimes(
                    insideChart( i )( k )( mDecoration.oddLeft ),
                      insideChart( k )( j )( mDecoration.oddRight ),
                        factorAndOutside
                  )
                }

              mEventCounts( i, k, j, mDecoration, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }

            }
          }
        }
      }
    }

    c.divideBy( stringProb )

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




}

