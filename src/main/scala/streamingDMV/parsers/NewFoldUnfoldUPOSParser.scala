package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.math.LogSum
import streamingDMV.tables.MatrixCPT
import streamingDMV.parameters.UPOSArcFactoredParameters
// import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.parameters.ArcFactoredParameters

import breeze.linalg._
import breeze.numerics._

import scala.collection.mutable.{Map=>MMap}
import math.log


// abstract class FoldUnfoldNOPOSParser[C<:DependencyCounts,P<:NOPOSArcFactoredParameters](
abstract class NewFoldUnfoldUPOSParser[P<:ArcFactoredParameters[MatrixDMVCounts]](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // randomSeed:Int = 15,
  // reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends StreamingVBParser[MatrixDMVCounts,P](
  // maxLength,
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha,
  // randomSeed,
  // reservoirSize
  parserSpec
) {

  val uposCount = parserSpec.uposCount

  // Efficient matrix operations can't be in log-space
  assert( ! logSpace )

  // val approximate:Boolean

  var insideChart:Array[Array[MMap[Decoration,DenseMatrix[Double]]]] = Array()
  var outsideChart:Array[Array[MMap[Decoration,DenseMatrix[Double]]]] = Array()

  def cellMap( i:Int, j:Int ):MMap[Decoration,DenseMatrix[Double]]

  def buildCharts( length:Int ) {
    insideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
    outsideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
  }

  def lexSpecs( index:Int ):Seq[Decoration]
  def lexCellFactor( index:Int, pDec:Decoration ):DenseMatrix[Double]
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

      insideChart( index )( index+1 )(pDec) :+= lexCellFactor( index, pDec )

      if( !( insideChart( index )( index+1 )(pDec).forall{ _ <= 1.000001 } ) ) {
        println( (index, lexCellFactor( index, pDec ), insideChart( index )( index+1 )(pDec) ) )
      }
      assert( insideChart( index )( index+1 )(pDec).forall{ _ <= 1.000001 } )
    }
  }
  // def insidePass( s:Array[Int] ) = {
  def insidePass( utt:Utt ) = {
    intString = doubleString( utt.string )
    lexString = doubleString( utt.lexes )

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
    (
      insideChart( k )( j )( cDec ).t * (
        insideChart( i )( k )( mDec ).t :* rightwardCellFactor( i, k, j, pDec, mDec, cDec )
      )
    ).t
  }
  def leftwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    (
      insideChart( i )( k )( cDec ).t * (
        insideChart( k )( j )( mDec ) :* leftwardCellFactor( i, k, j, pDec, mDec, cDec )
      )
    ).t
  }

  // TODO double-check this is right if using second-order grammar
  def mCellScore( i:Int, k:Int, j:Int, mDecoration:MDecoration ) = {
    val score = 
      if( k%2 == 0 ) {
        // myTimes( 
        //   insideChart( i )( k )( mDecoration.evenLeft ),
        //     insideChart( k )( j )( mDecoration.evenRight ),
        //       mCellFactor( i, k, j, mDecoration )
        // )
        insideChart( i )( k )( mDecoration.evenLeft ) *
          insideChart( k )( j )( mDecoration.evenRight).t
      } else {
        // myTimes(
        //   insideChart( i )( k )( mDecoration.oddLeft ),
        //     insideChart( k )( j )( mDecoration.oddRight ),
        //       mCellFactor( i, k, j, mDecoration )
        // )
        (
          insideChart( i )( k )( mDecoration.evenLeft ) *
            insideChart( k )( j )( mDecoration.evenRight).t
        ) :* (
          mCellFactor( i, k, j, mDecoration )
        )
      }

    if( !( score.forall{ _ > 0D } && score.forall{ _ <= 1.00001 } ) ) {
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
    // myTimes(
    //   insideChart( 0 )( k )( leftDec ),
    //     insideChart( k )( intString.length )( rightDec ),
    //       rootCellFactor( k )
    // )

    insideChart( 0 )( k )( leftDec ) :* 
      insideChart( k )( intString.length )( rightDec ) :* 
        rootCellFactor( k )

  }


  def computeInsideMScore( i:Int, j:Int ) {
    // println( s"?> ${(i,j)}" )
    mSplitSpecs(i,j).foreach{ case (mDecoration, splits) =>
      // println( s"=>- ${(i,j,mDecoration,splits.mkString(" "))}" )
      splits.foreach{ k =>

        insideChart( i )( j )( mDecoration ) :+= mCellScore( i, k, j, mDecoration )


        assert( insideChart( i )( j )( mDecoration ).forall{ _ > myZero } )


        if( !( insideChart( i )( j )( mDecoration ).forall{ _ <= myOne + 1E-20 } ) ) {
          println( insideChart( i )( j )( mDecoration ) )
        }
        assert( insideChart( i )( j )( mDecoration ).forall{ _ <= myOne + 1E-20 } )

      }
    }
  }
  def computeInsideRootScore() {
    rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
      val r = intString( k )

      stringProb :+= sum( rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight ) )

      if( !( stringProb > myZero )) {
        println( stringProb )
      }
      assert( stringProb > myZero )
      assert( stringProb <= myOne + 1E-10 )
    }
  }
  def computeInsideRightwardScore( i:Int, j:Int ) {
    rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>

        insideChart( i )( j )( pDec ) :+= rightwardCellScore( i, k, j, pDec, mDec, cDec )

        if( !( insideChart( i )( j )( pDec ).forall{ _ > myZero } ) ) {
          println( "inside rightward cell: " + (i,j,pDec, insideChart( i )( j )( pDec ) ) )
        }
        assert( insideChart( i )( j )( pDec ).forall{ _ > myZero } )
        assert( insideChart( i )( j )( pDec ).forall{ _ <= myOne + 1E-10 } )

      }
    }
  }
  def computeInsideLeftwardScore( i:Int, j:Int ) {
    leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        insideChart( i )( j )( pDec ) :+= leftwardCellScore( i, k, j, pDec, mDec, cDec )

        if( !( insideChart( i )( j )( pDec ).forall{ _ > myZero } &&
                insideChart( i )( j )( pDec ).forall{ _ <= myOne + 1E-10 } ) ) {
          println( "! " + (i,k,j,pDec, insideChart( i )( j )( pDec ) ) )
          println( "! " + leftwardCellScore( i, k, j, pDec, mDec, cDec ) )
          println( "factor:" )
          println( leftwardCellFactor( i, k, j, pDec, mDec, cDec ) )
          println( "summed across rows: " )
          println( sum( leftwardCellFactor( i, k, j, pDec, mDec, cDec )(::,*) ) )
          println( "left child:" )
          println( insideChart( i )( k )( cDec ) )
          println( "right child:" )
          println( insideChart( k )( j )( mDec ) )
        }
        assert( insideChart( i )( j )( pDec ).forall{ _ > myZero } )
        assert( insideChart( i )( j )( pDec ).forall{ _ <= myOne + 1E-10 } )
      }
    }
  }

  def rootCellFactor( k:Int ):DenseMatrix[Double]
  def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration):DenseMatrix[Double]
  def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration):DenseMatrix[Double]
  def mCellFactor( i:Int, k:Int, j:Int, mDecoration:MDecoration ):DenseMatrix[Double]

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
            (k,mDec, cDec), {
              val scores =
                insideChart( i )( k )( mDec ).t :* rightwardCellFactor( i, k, j, pDec, mDec, cDec )
              scores( *, :: ) :*= insideChart( k )( j )( cDec ).toDenseVector
              scores
            }
            // rightwardCellScore( i, k, j, pDec, mDec, cDec )
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
            (k,mDec, cDec), {
              val scores =
                insideChart( k )( j )( mDec ) :* leftwardCellFactor( i, k, j, pDec, mDec, cDec )
              scores( *, :: ) :*= insideChart( i )( k )( cDec ).toDenseVector
              scores
            }
            // leftwardCellScore( i, k, j, pDec, mDec, cDec )
          )
        }
      )
    }
  }
  def mCellScores( i:Int, j:Int ):Seq[Tuple2[MDecoration,Seq[Tuple2[Int,DenseMatrix[Double]]]]] = {
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

                  // outsideChart( 0 )( k )( decorationPair.evenLeft ) = myPlus(
                  //     outsideChart( 0 )( k )( decorationPair.evenLeft ) ,
                  //     myTimes(
                  //       insideChart( k )( intString.length )( decorationPair.evenRight ),
                  //       factorAndOutside
                  //     )
                  //   )
              outsideChart( 0 )( k )( decorationPair.evenLeft ) :+=
                insideChart( k )( intString.length )( decorationPair.evenRight ) :* factorAndOutside

                  // outsideChart( k )( intString.length )( decorationPair.evenRight ) = myPlus(
                  //   outsideChart( k )( intString.length )( decorationPair.evenRight ),
                  //   myTimes(
                  //     insideChart( 0 )( k )( decorationPair.evenLeft ), factorAndOutside
                  //   )
                  // )
              outsideChart( k )( intString.length )( decorationPair.evenRight ) :+=
                insideChart( 0 )( k )( decorationPair.evenLeft ) :* factorAndOutside
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( j-i >= 3 ) {
            leftwardSplitSpecs( i, j ).foreach{ case ( decoration, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                    // val factorAndOutside = myTimes(
                    //   outsideChart( i )( j )( decoration ),
                    //     leftwardCellFactor( i, k, j, decoration, mDec, cDec )
                    // )
                val factorAndOutside =
                  leftwardCellFactor( i, k, j, decoration, mDec, cDec )(*,::) :*
                    outsideChart( i )( j )( decoration ).toDenseVector

                // to left child
                    // outsideChart( i )( k )( cDec ) = myPlus(
                    //     outsideChart( i )( k )( cDec ),
                    //     myTimes(
                    //       insideChart( k )( j )( mDec ),
                    //         factorAndOutside
                    //     )
                    //   )
                val leftwardMessage = factorAndOutside :* insideChart( k )( j )( mDec )
                outsideChart( i )( k )( cDec ) :+= sum( leftwardMessage(*,::) ).toDenseMatrix
                    // myTimes(
                    //   insideChart( k )( j )( mDec ),
                    //     factorAndOutside
                    // )

                // to right child
                    // outsideChart( k )( j )( mDec ) = myPlus(
                    //     outsideChart( k )( j )( mDec ),
                    //     myTimes(
                    //       insideChart( i )( k )( cDec ),
                    //         factorAndOutside
                    //     )
                    //   )
                val rightwardMessage = factorAndOutside
                rightwardMessage( ::, * ) :*= insideChart( i )( k )( cDec ).toDenseVector
                outsideChart( k )( j )( mDec ) :+= rightwardMessage.toDenseMatrix
                    // myTimes(
                    //   insideChart( i )( k )( cDec ),
                    //     factorAndOutside
                    // )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( j-i >= 3 ) {
            rightwardSplitSpecs( i, j ).foreach{ case ( decoration, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>
                  // val factorAndOutside = myTimes(
                  //   outsideChart( i )( j )( decoration ),
                  //     rightwardCellFactor( i, k, j, decoration, mDec, cDec )
                  // )
                val factorAndOutside =
                  rightwardCellFactor( i, k, j, decoration, mDec, cDec )(*,::) :*
                    outsideChart( i )( j )( decoration ).toDenseVector

                // to left child
                val leftwardMessage = factorAndOutside :* insideChart( k )( j )( mDec ).t
                leftwardMessage( ::, * ) :*= insideChart( k )( j )( cDec ).toDenseVector
                    // outsideChart( i )( k )( mDec ) = myPlus(
                    //     outsideChart( i )( k )( mDec ),
                    //     myTimes(
                    //       insideChart( k )( j )( cDec ),
                    //         factorAndOutside
                    //     )
                    //   )
                outsideChart( i )( k )( mDec ) :+= insideChart( k )( j )( cDec )


                // to right child
                val rightwardMessage = factorAndOutside :* insideChart( i )( k )( mDec ).t
                    // outsideChart( k )( j )( cDec ) = myPlus(
                    //     outsideChart( k )( j )( cDec ),
                    //     myTimes(
                    //       insideChart( i )( k )( mDec ),
                    //         factorAndOutside
                    //     )
                    //   )
                outsideChart( k )( j )( cDec ) :+= sum( rightwardMessage( *,:: ) ).toDenseMatrix
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          // TODO: double-check this with second-order grammar
          mSplitSpecs( i, j ).foreach{ case (mDecoration, splits) =>
            splits.foreach{ k =>
                  // val factorAndOutside = myTimes(
                  //   outsideChart( i )( j )( mDecoration ), mCellFactor( i, k, j, mDecoration )
                  // )
              val factorAndOutside = outsideChart( i )( j )( mDecoration ) :*
                mCellFactor( i, k, j, mDecoration )

              if( k%2 == 0 ) {
                // to left child
                    // outsideChart( i )( k )( mDecoration.evenLeft ) = myPlus(
                    //     outsideChart( i )( k )( mDecoration.evenLeft ),
                    //     myTimes(
                    //       insideChart( k )( j )( mDecoration.evenRight ),
                    //         factorAndOutside
                    //     )
                    //   )

                outsideChart( i )( k )( mDecoration.evenLeft ) :+= (
                    insideChart( k )( j )( mDecoration.evenRight ) *
                      factorAndOutside
                  )

                // to right child
                    // outsideChart( k )( j )( mDecoration.evenRight ) = myPlus(
                    //     outsideChart( k )( j )( mDecoration.evenRight ),
                    //     myTimes(
                    //       insideChart( i )( k )( mDecoration.evenLeft ),
                    //         factorAndOutside
                    //     )
                    //   )
                outsideChart( k )( j )( mDecoration.evenRight ) :+= (
                    insideChart( i )( k )( mDecoration.evenLeft ) *
                      factorAndOutside.t
                  )
              } else {
                // to left child
                    // outsideChart( i )( k )( mDecoration.oddLeft ) = myPlus(
                    //     outsideChart( i )( k )( mDecoration.oddLeft ),
                    //     myTimes(
                    //       insideChart( k )( j )( mDecoration.oddRight ),
                    //         factorAndOutside
                    //     )
                    //   )
                outsideChart( i )( k )( mDecoration.oddLeft ) :+= (
                    insideChart( k )( j )( mDecoration.oddRight ) *
                      factorAndOutside
                  )

                // to right child
                    // outsideChart( k )( j )( mDecoration.oddRight ) = myPlus(
                    //     outsideChart( k )( j )( mDecoration.oddRight ),
                    //     myTimes(
                    //       insideChart( i )( k )( mDecoration.oddLeft ),
                    //         factorAndOutside
                    //     )
                    //   )
                outsideChart( k )( j )( mDecoration.oddRight ) :+= (
                    insideChart( i )( k )( mDecoration.oddLeft ) *
                      factorAndOutside.t
                  )
              }
            }
          }
        }
      }
    }
  }



  // Viterbi definitions

  var headTrace = Array[Array[MMap[Tuple2[Decoration,Int],Entry]]]()
    // = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
    //   MMap[Decoration,Entry]()
    // )
  var mTrace = Array[Array[MMap[Tuple3[MDecoration,Int,Int],MEntry]]]()
    // = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
    //   MMap[MDecoration,MEntry]()
    // )

  // def vitCellMap( i:Int, j:Int ):MMap[Decoration,Double]

  def buildVitCharts( length:Int ) {
    insideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
    outsideChart = Array.tabulate( length, length+1 )( (i,j) => cellMap( i, j ) )
    headTrace = Array.tabulate( length, length+1 )( (i,j) => MMap() )
    mTrace = Array.tabulate( length, length+1 )( (i,j) => MMap() )
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

  def findLeftRootChild( k:Int, rPos:Int ):Entry
  def findRightRootChild( k:Int, rPos:Int ):Entry

  def findLeftLeftwardChild( i:Int, k:Int, dPos:Int ):Entry
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ):Entry

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ):Entry
  def findRightRightwardChild( k:Int, j:Int, dPos:Int ):Entry

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration, lPos:Int ):Entry
  def findRightMChild( k:Int, j:Int, decoration:MDecoration, rPos:Int ):Entry


  def insertRootEntry( k:Int, rPos:Int ) = treeRoot = RootEntry( k, rPos )
  def insertMEntry( i:Int, k:Int, j:Int, decoration:MDecoration, lPos:Int, rPos:Int ) =
    mTrace( i )( j )( ( decoration, lPos, rPos ) ) = MEntry( i , k, j, decoration, lPos, rPos )
  def insertLeftwardEntry( i:Int, k:Int, j:Int, pDec:Decoration, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    headTrace( i )( j )( ( pDec, hPos ) ) = LeftwardEntry( i, k, j, hV, mDV, hPos, dPos )
  def insertRightwardEntry( i:Int, k:Int, j:Int, pDec:Decoration, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
    headTrace( i )( j )( ( pDec, hPos ) ) = RightwardEntry( i, k, j, hV, mDV, hPos, dPos )

  def insertLexEntry( index:Int, pDec:Decoration, hPos:Int ) =
    headTrace( index )( index+1 )( ( pDec, hPos ) ) = LexEntry( index )

  case class RootEntry( k:Int, rPos:Int ) extends BinaryEntry( 0, k, intString.length ) {
    val leftChild = findLeftRootChild( k, rPos )
    val rightChild = findRightRootChild( k, rPos )

    def toDepParse =
      Set( DirectedArc( intString.length/2, (k-1)/2 ) ) ++
        leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(*.${intString( k )}.${rPos} ${leftChild.toConParse} ${rightChild.toConParse} )"
  }

  case class MEntry(
    i:Int, k:Int, j:Int, decoration:MDecoration, lPos:Int, rPos:Int
  ) extends BinaryEntry( i, k, j ) {
    val leftChild = findLeftMChild( i, k, decoration, lPos )
    val rightChild = findRightMChild( k, j, decoration, rPos )

    def toDepParse = {
      decoration match {
        case LeftwardM => Set( DirectedArc( (j-1)/2, (i-1)/2 ) )
        case RightwardM => Set( DirectedArc( (i-1)/2, (j-1)/2 ) )
        case _ => Set()
      }
    } ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(M_${lPos}_${rPos} ${leftChild.toConParse} ${rightChild.toConParse} )"

  }

  case class LeftwardEntry( i:Int, k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) extends BinaryEntry( i, k, j ) {
    // println( (i,k,j,hV,mDV) )
    val leftChild = findLeftLeftwardChild( i, k, dPos )
    val rightChild = findRightLeftwardChild( k, j, hV, mDV, hPos, dPos )

    def toDepParse =
      Set( DirectedArc( (j-1)/2, (k-1)/2 ) ) ++ leftChild.toDepParse ++ rightChild.toDepParse
    def toConParse = s"(L.${intString(j)} ${leftChild.toConParse} ${rightChild.toConParse})"
  }

  case class RightwardEntry( i:Int, k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) extends BinaryEntry( i, k, j ) {
    val leftChild = findLeftRightwardChild( i , k, hV, mDV, hPos, dPos )
    val rightChild = findRightRightwardChild( k, j, dPos )

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
  def argMax[K](
                          // dPos by hPos
    seq:Iterable[Tuple2[K,DenseMatrix[Double]]],
    hPos:Int
  ):Tuple3[K,Int,Double] = {
    // max over splits and dPos
    var bestIdx = List[K]()
    var bestScore = 0D
    var bestDPos = List[Int]()
    seq.foreach{ case ( idx , scores ) =>
      (0 until uposCount).foreach{ dPos =>
        val score = scores( dPos, hPos )
        if( score > bestScore ) {
          bestScore = score
          bestIdx = idx :: Nil
          bestDPos = dPos :: Nil
        } else if( score == bestScore ) {
          bestIdx = idx :: bestIdx
          bestDPos = dPos :: bestDPos
        }
      }
    }
    if( bestIdx.length == 1 ) {
      ( bestIdx.head, bestDPos.head, bestScore )
    } else {
      val which = rand.nextInt( bestIdx.length )
      ( bestIdx(which), bestDPos(which), bestScore )
    }
  }

  def argSample[K](
                          // dPos by hPos
    seq:Iterable[Tuple2[K,DenseMatrix[Double]]],
    hPos:Int
  ):Tuple3[K,Int,Double] = {
    // max over splits and dPos

    var dPos = -1
    var score = Double.NegativeInfinity

    val total = seq.map{ pair => sum( pair._2( ::, hPos ) ) }.sum
    val r = rand.nextDouble() * total

    var runningTotal = 0D

    // first find split randomly
    val ( k, dPoses ) = seq.takeWhile( pair => {
        val keepGoing = runningTotal < r
        if( keepGoing ) runningTotal += sum( pair._2(::,hPos) )
        keepGoing
      }
    ).last

    // then find dPos randomly
    (0 until uposCount).takeWhile{ itDPos =>
      val keepGoing = runningTotal < r
      if( keepGoing ) {
        runningTotal += dPoses( hPos, itDPos )
      } else {
        dPos = itDPos
        score = dPoses( hPos, itDPos )
      }
      keepGoing
    }

    assert( dPos >= 0 && score >= 0 )

    ( k, dPos, score )
  }

  def viterbiLexFill( index:Int ) {
    lexSpecs( index ).foreach{ pDec =>
      insideChart( index )( index+1 )(pDec) = lexCellFactor( index, pDec )
      (0 until uposCount).foreach{ hPos =>
        headTrace( index )( index+1 )( ( pDec, hPos ) ) = LexEntry( index )
      }
    }
  }


  def viterbiSynFill( i:Int, j:Int ) {
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      rightwardCellScores( i, j ).foreach{ case( pDec, splitsAndScores ) =>
        (0 until uposCount).foreach{ hPos =>
          val ( (k,mDec,cDec), dPos, bestScore ) = argMax( splitsAndScores, hPos )
          insideChart( i )( j )( pDec )( hPos, 0 ) = bestScore
          headTrace( i )( j )( ( pDec, hPos ) ) =
            RightwardEntry( i, k, j, mDec.evenLeft, mDec.evenRight, hPos, dPos )
        }
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward

      leftwardCellScores( i, j ).foreach{ case( pDec, splitsAndScores ) =>
        (0 until uposCount).foreach{ hPos =>
          val ( (k,mDec,cDec), dPos, bestScore ) = argMax( splitsAndScores, hPos )
          insideChart( i )( j )( pDec )( hPos, 0 ) = bestScore
          headTrace( i )( j )( ( pDec, hPos ) ) =
            LeftwardEntry( i, k, j, mDec.evenRight, mDec.evenLeft, hPos, dPos )
        }
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M

      mCellScores(i,j).foreach{ case (mDecoration, splitsAndScores) =>
        (0 until uposCount).foreach{ lPos =>
          (0 until uposCount).foreach{ rPos =>
            val ( bestK, bestScore ) = argMax( splitsAndScores.map{ sAndS =>
                (sAndS._1, sAndS._2( lPos, rPos ) )
              }
            )
            insideChart( i )( j )( mDecoration )( lPos, rPos ) = bestScore
            mTrace( i )( j )( ( mDecoration, lPos, rPos ) ) =
              MEntry( i, bestK, j, mDecoration, lPos, rPos )
          }
        }
      }
    } else {
      // Root

      if( i == 0 && j == intString.length ) {
        val ( (bestK,_), rPos, bestScore ) = argMax( rootCellScores(), 0 )
        stringProb = bestScore
        treeRoot = RootEntry( bestK, rPos )
      }
    }
  }

  def viterbiParse( utt:Utt ) = {
    clearVitCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    lexString = doubleString( utt.lexes )
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
    lexString = doubleString( utt.lexes )
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
    buildCharts( insideChart.length )
        // (0 until insideChart.length ).foreach{ i =>
        //   ((i+1) until insideChart.length+1 ).foreach{ j =>
        //     // insideChart(i)(j) = cellMap( i, j )
        //     // outsideChart(i)(j) = cellMap( i, j )
        //     // insideChart(i)(j).mapValues( _ => myZero )
        //     // outsideChart(i)(j).mapValues( _ => myZero )
        //     insideChart(i)(j).keys.foreach{ vs =>
        //       insideChart(i)(j)(vs) = cellMap( i, j )
        //       outsideChart(i)(j)(vs) = cellMap( i, j )
        //     }
        //   }
        // }
    // buildCharts( insideChart.length-1 )
    treeRoot = null
    stringProb = myZero
  }

          // for M-node, otherPos is the POS of the right child -- for all others it's irrelevant,
          // There oughta be a better way to do this...
  def sampleTreeCounts(
    i:Int,
    j:Int,
    hPos:Int,
    otherPos:Int = -1,
    pDec:Decoration
  ):Seq[Tuple2[Event,DenseMatrix[Double]]] = {
    if( i%2 == 1 && j%2 == 1 ) { // M

      // val splitsAndScores =
      mCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
        assert( parent == pDec )

                          // POS of children of M-node is deterministic
        val ( k, score ) = argSample( splitsAndScores.map{ p=> (p._1, p._2(hPos, otherPos) ) } )

        sampleScore = myTimes( sampleScore, score )

        val count = DenseMatrix.zeros[Double]( uposCount, uposCount )
        count( hPos, otherPos ) = 1D

        if( k%2 == 0 )
          mEventCounts( i, k, j, parent, count ) ++
            sampleTreeCounts( i, k, hPos, -1, parent.evenLeft ) ++
              sampleTreeCounts( k, j, otherPos, -1, parent.evenRight )
        else
          mEventCounts( i, k, j, parent, count ) ++
            sampleTreeCounts( i, k, hPos, -1, parent.oddLeft ) ++
              sampleTreeCounts( k, j, otherPos, -1, parent.oddRight )
      }

    } else if( i%2 == 1 ) { // Rightward

      // val splitsAndScores =
      if( j-i > 1 ) {
        rightwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec), dPos , score ) = argSample( splitsAndScores, hPos )

          sampleScore = myTimes( sampleScore, score )

          val count = DenseMatrix.zeros[Double]( uposCount, uposCount )
          count( hPos, dPos ) = 1D

          rightwardEventCounts( i, k, j, pDec, mDec, cDec, count  ) ++
            sampleTreeCounts( i, k, hPos, dPos, mDec ) ++
              sampleTreeCounts( k, j, dPos, -1, cDec )
        }
      } else {
        // lexMarginals( j )
        lexCellScores( i ).filter( _._1 == pDec ).flatMap{ case ( parent, scores ) =>
          assert( parent == pDec )
          assert( scores.length == 1 )

          sampleScore = myTimes( sampleScore, scores.head( 0, hPos ) )

          val count = DenseMatrix.zeros[Double]( uposCount, 1 )
          count( hPos, 0 ) = 1D


          lexEventCounts( i, pDec, count )
        }
      }

    } else if( j%2 == 1 ) { // Leftward

      // val splitsAndScores =
      if( j-i > 1 ) {
        leftwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case ( parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , dPos, score ) = argSample( splitsAndScores, hPos )

          sampleScore = myTimes( sampleScore, score )
          val count = DenseMatrix.zeros[Double]( uposCount, uposCount )
          count( hPos, dPos ) = 1D

          leftwardEventCounts( i, k, j, pDec, mDec, cDec, count ) ++
            sampleTreeCounts( i, k, dPos, -1, cDec ) ++
              sampleTreeCounts( k, j, dPos, hPos, mDec )
        }
      } else {
        // lexMarginals( i )
        lexCellScores( i ).filter( _._1 == pDec ).flatMap{ case ( parent, scores ) =>
          assert( parent == pDec )
          assert( scores.length == 1 )

          val count = DenseMatrix.zeros[Double]( uposCount, 1 )
          count( hPos, 0 ) = 1D

          sampleScore = myTimes( sampleScore, scores.head( 0, hPos) )
          lexEventCounts( i, pDec, count )
        }
      }

    } else if( i == 0 && j == intString.length ) { // Root
      assert( pDec == RootDecoration )

      val ((k,cDecs), rPos, score) = argSample( rootCellScores(), 0 )

      sampleScore = myTimes( sampleScore, score )
      val count = DenseMatrix.zeros[Double]( uposCount, 1 )
      count( rPos, 0 ) = 1D

      rootEventCounts( k, count ) ++
        sampleTreeCounts( i , k, rPos, -1, cDecs.evenLeft ) ++
        sampleTreeCounts( k, intString.length, rPos, -1, cDecs.evenRight )

    } else {
      Seq()
    }
  }

  // def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha, true )

  def emptyCounts = MatrixDMVCounts( uposCount, rootAlpha, stopAlpha, chooseAlpha )

  var sampleScore = myOne

  // def sampleTreeCounts( originalString:Array[Int] ):Tuple2[C,Double] = {
  def sampleTreeCounts( utt:Utt ):Tuple2[MatrixDMVCounts,Double] = {
    // val s = doubleString( originalString )
    clearCharts
    val normalized = theta.fullyNormalized
    theta.fullyNormalized = true
    insidePass( utt )
    val c = emptyCounts
    sampleScore = myOne
    // println( "incrementing counts for sample" )
    sampleTreeCounts( 0, intString.length, 0, -1, RootDecoration ).foreach{ case (event, count) =>
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



  def initialCounts( utts:List[Utt] ) =
    if( parserSpec.harmonicMiniBatchInit )
      theta.harmonicCounts( utts )
    // else if( )
    else
      constantInitialCounts
      // emptyCounts

  def logProb( utt:Utt ) = {
    // val s = string.flatMap{ w => Seq(w,w) }
    clearCharts
    theta.fullyNormalized = true
    insidePass( utt )
    theta.fullyNormalized = false
    // math.log(stringProb)
    if( logSpace )
      stringProb
    else
      log( stringProb )
  }

  // training stuff

  // def lexMarginals( index:Int ):Seq[Tuple2[Event,Double]]


  def rootEventCounts( k:Int, marginal:DenseMatrix[Double] ):Seq[Tuple2[Event,DenseMatrix[Double]]]
  def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:DenseMatrix[Double] ):Seq[Tuple2[Event,DenseMatrix[Double]]]
  def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:DenseMatrix[Double] ):Seq[Tuple2[Event,DenseMatrix[Double]]]
  def mEventCounts( i:Int, k:Int, j:Int, mDecoration:MDecoration, marginal:DenseMatrix[Double] ):Seq[Tuple2[Event,DenseMatrix[Double]]]
  def lexEventCounts( index:Int, pDec:Decoration, marginal:DenseMatrix[Double] ):Seq[Tuple2[Event,DenseMatrix[Double]]]

  // def outsidePassWithCounts( s:Array[Int] ):C = {
  def outsidePassWithCounts():MatrixDMVCounts = {

    val c = emptyCounts

    ( 1 to intString.length ).reverse.foreach{ length =>
      ( 0 to ( intString.length - length ) ).foreach{ i =>
        val j = i + length
        if( i%2 == 0 && j%2 == 0 ) { // Root
          if( i == 0 && j == intString.length ) {
            rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
              val r = intString( k )
              val factorAndOutside = rootCellFactor( k )

              outsideChart( 0 )( k )( decorationPair.evenLeft ) :+=
                insideChart( k )( j )( decorationPair.evenRight ) :* factorAndOutside

              outsideChart( k )( j )( decorationPair.evenRight ) :+=
                insideChart( 0 )( k )( decorationPair.evenLeft ) :* factorAndOutside

                // val marginal = myTimes(
                //   myDiv( insideChart( 0 )( k )( decorationPair.evenLeft ), stringProb ),
                //     insideChart( k )( intString.length )( decorationPair.evenRight ),
                //       factorAndOutside
                // )
              val marginal =
                ( insideChart( 0 )( k )( decorationPair.evenLeft ) :/ stringProb ) :*
                  insideChart( k )( j )( decorationPair.evenRight ) :*
                    factorAndOutside

              rootEventCounts( k, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
            }
          }
        } else if( i%2 == 0 && j%2 == 1 ) { // Leftward label
          if( length > 1 ) {
            leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>

                val factor = leftwardCellFactor( i, k, j, pDec, mDec, cDec )

                val factorAndOutside = factor(*,::) :* outsideChart( i )( j )( pDec ).toDenseVector

                val leftwardMessage = factorAndOutside :* insideChart( k )( j )( mDec )
                outsideChart( i )( k )( cDec ) :+= sum( leftwardMessage(*,::) ).toDenseMatrix.t

                val rightwardMessage =
                  factorAndOutside( ::, * ) :* insideChart( i )( k )( cDec ).toDenseVector
                outsideChart( k )( j )( mDec ) :+= rightwardMessage

                // val marginal =
                //   factorAndOutside :* ( insideChart( k )( j )( mDec ).t :/ stringProb )
                // marginal( ::, * ) :*= insideChart( i )( k )( cDec ).toDenseVector

                val marginal = (
                  insideChart( i )( k )( cDec ) *
                    outsideChart( i )( j )( pDec ).t
                ) :* factor :* ( insideChart( k )( j )( mDec ) :/ stringProb )



                leftwardEventCounts( i, k, j, pDec, mDec, cDec, marginal ).foreach{ case (event, count) =>
                      // if( !( count.forall{ _ <= myOne + 1E-10 } ) ) {
                      //   println( s"probability greater than one for $event ${(i,k,j)}" )
                      //   println( insideChart( i )( k )( cDec ) )
                      //   println( "--" )
                      //   println( stringProb )
                      //   println( "--" )
                      //   println( insideChart( k )( j )( mDec ) )
                      //   println( "--" )
                      //   println( factorAndOutside )
                      //   println( "--" )
                      //   println( count )
                      // }
                  assert( count.forall{ _ > myZero } )
                  assert( count.forall{ _ <= myOne + 1E-10 } )
                  c.increment( event, count )
                }
              }
            }
          } else {
            lexSpecs( i ).foreach{ pDec =>
              // Don't include stop factor -- it's actually already included in insideChart (the
              // *real* inside score for each terminal is 1)
                  // val marginal = myTimes(
                  //   insideChart(i)(i+1)(pDec),
                  //   myDiv( outsideChart(i)(i+1)(pDec), stringProb )
                  // )
              val marginal =
                insideChart( i )( i+1 )( pDec ) :* ( outsideChart( i )( i+1 )( pDec ) :/ stringProb )

              lexEventCounts( i, pDec, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 0 ) { // Rightward label
          if( length > 1 ) {
            rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
              splits.foreach{ case ( k, mDec, cDec ) =>

                val factor = rightwardCellFactor( i, k, j, pDec, mDec, cDec )
                val factorAndOutside = factor(*,::) :* outsideChart( i )( j )( pDec ).toDenseVector

                val rightwardMessage = factorAndOutside :* ( insideChart( i )( k )( mDec ).t  )
                outsideChart( k )( j )( cDec ) :+= sum( rightwardMessage( *,:: ) ).toDenseMatrix.t

                val leftwardMessage = factorAndOutside( ::, * ) :* insideChart( k )( j )( cDec ).toDenseVector
                outsideChart( i )( k )( mDec ) :+= leftwardMessage.t

                    // val marginal =
                    //   factorAndOutside :* ( insideChart( i )( k )( mDec ) :/ stringProb )
                    //     // val cVec = insideChart( k )( j )( cDec )
                    //     // println( (i,k,j) )
                    //     // println( (cVec.rows, cVec.cols) )
                    //     // println( cVec )
                    //     // println( "\n" )
                    // marginal( ::, * ) :*= insideChart( k )( j )( cDec ).toDenseVector

                val marginal = (
                  insideChart( k )( j )( cDec ) *
                    outsideChart( i )( j )( pDec ).t
                ) :* factor :* ( insideChart( i )( k )( mDec ).t :/ stringProb )

                rightwardEventCounts( i, k, j, pDec, mDec, cDec, marginal ).foreach{ case (event, count) =>
                  c.increment( event, count )
                }
              }
            }
          } else {
            lexSpecs( i ).foreach{ pDec =>
              // Don't include stop factor -- it's actually already included in insideChart (the
              // *real* inside score for each terminal is 1)
                  // val marginal = myTimes(
                  //   insideChart(i)(i+1)(pDec),
                  //   myDiv( outsideChart(i)(i+1)(pDec), stringProb )
                  // )
              val marginal =
                insideChart( i )( i+1 )( pDec ) :* ( outsideChart( i )( i+1 )( pDec ) :/ stringProb )

              lexEventCounts( i, pDec, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
            }
          }
        } else if( i%2 == 1 && j%2 == 1 ) { // M label
          mSplitSpecs( i, j ).foreach{ case (mDecoration, splits) =>
            splits.foreach{ k =>


              if( k%2 == 0 ) {
                    // // to left child
                    // outsideChart( i )( k )( mDecoration.evenLeft ) :+= (
                    //     factorAndOutside *
                    //       insideChart( k )( j )( mDecoration.evenRight )
                    //   )

                    // // to right child
                    // outsideChart( k )( j )( mDecoration.evenRight ) :+= (
                    //     factorAndOutside.t *
                    //       insideChart( i )( k )( mDecoration.evenLeft )
                    //   )
                outsideChart( i )( k )( mDecoration.evenLeft ) :+= (
                  insideChart( k )( j )( mDecoration.evenRight ).t *
                    outsideChart( i )( j )( mDecoration ).t
                ).t

                outsideChart( k )( j )( mDecoration.evenRight ) :+= (
                  insideChart( i )( k )( mDecoration.evenLeft ).t *
                    outsideChart( i )( j )( mDecoration )
                ).t

              } else {
                val factorAndOutside = outsideChart( i )( j )( mDecoration ) :*
                  mCellFactor( i, k, j, mDecoration )
                // to left child
                outsideChart( i )( k )( mDecoration.oddLeft ) :+= (
                    insideChart( k )( j )( mDecoration.oddRight ) *
                      factorAndOutside
                  )

                // to right child
                outsideChart( k )( j )( mDecoration.oddRight ) :+= (
                    insideChart( i )( k )( mDecoration.oddLeft ) *
                      factorAndOutside.t
                  )

              }


              // Will need to pay attention when implementing this for the second-order grammar
              assert( k%2 == 0 )
              /*
              val marginal = 
                if( k%2 == 0 ) {
                  myTimes(
                    myDiv(
                      insideChart( i )( k )( mDecoration.evenLeft ),
                      stringProb
                    ),
                      insideChart( k )( j )( mDecoration.evenRight ),
                        factorAndOutside
                  )
                } else {
                  myTimes(
                    myDiv(
                      insideChart( i )( k )( mDecoration.oddLeft ),
                      stringProb
                    ),
                      insideChart( k )( j )( mDecoration.oddRight ),
                        factorAndOutside
                  )
                }

              mEventCounts( i, k, j, mDecoration, marginal ).foreach{ case (event, count) =>
                c.increment( event, count )
              }
              */

            }
          }
        }
      }
    }

    // c.divideBy( stringProb )

        // println( s"inside outsidePassWithCounts" )
    // c.printTotalCountsByType

    c
  }
  def populateChart( utt:Utt ) {
    // val s = string.flatMap{ w=> Seq(w,w) }
    // val s = doubleString( string )
    clearCharts
    insidePass( utt )
    outsidePass
  }

  // def extractPartialCounts( string:Array[Int] ) = {
  def extractPartialCounts( utt:Utt ) = {
    // val s = doubleString( string )
    // println( utt.id + " " + utt.string.mkString(" ") )
    // println( utt.id + " " + utt.lexes.mkString(" ") )
    clearCharts
    insidePass( utt )
    outsidePassWithCounts()
  }




}

