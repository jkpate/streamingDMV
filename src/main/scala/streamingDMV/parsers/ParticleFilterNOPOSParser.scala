package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.NOPOSArcFactoredParameters
// import streamingDMV.parsers.FoldUnfoldNOPOSParser.RootEntry

import scala.collection.mutable.{Map=>MMap}
import scala.reflect.ClassTag

class ParticleFilterNOPOSParser[
  P<:NOPOSArcFactoredParameters,
  R<:FoldUnfoldNOPOSParser[P]:ClassTag
](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  numParticles:Int = 16,
  createParticle:(DMVCounts,Int) => R,
  randomSeed:Int = 15
) extends ParticleFilterParser[DMVCounts,P,R](
  maxLength,
  rootAlpha,
  stopAlpha,
  chooseAlpha,
  numParticles,
  createParticle,
  randomSeed
) {



  def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha )

  def extractPartialCounts(string: Array[Int]) = particles.head.extractPartialCounts( string )
  def initialCounts(utts: List[streamingDMV.labels.Utt]) = particles.head.initialCounts( utts )

  // val insideChart = particles.head.insideChart.map{ _.clone }.clone
  // val outsideChart = particles.head.outsideChart.map{ _.clone }.clone
  val insideChart = particles.head.insideChart
  val outsideChart = particles.head.outsideChart


  // def logProb(string: Array[Int]) = {
  //   (0 until numParticles).map{ l => 
  //     log( particleWeights( l ) ) + particles( l ).logProb( string )
  //   }.reduce( logSum( _, _ ) )
  // }

  def lexSpecs(index:Int) = particles.head.lexSpecs(index)
  def mSplitSpecs(i:Int,j:Int) = particles.head.mSplitSpecs(i,j)
  def rootSplitSpecs() = particles.head.rootSplitSpecs()
  def leftwardSplitSpecs(i:Int,j:Int) = particles.head.leftwardSplitSpecs(i,j)
  def rightwardSplitSpecs(i:Int,j:Int) = particles.head.rightwardSplitSpecs(i,j)
  def lexFill( index:Int ) {
    lexSpecs( index ).foreach{ pDec =>
      particles.head.insideChart( index )( index+1 )(pDec) += lexCellFactor( index, pDec )
    }
  }

  def computeInsideMScore( i:Int, j:Int ) {
    mSplitSpecs(i,j).foreach{ case (mDecoration, splits) =>
      splits.foreach{ k =>
        particles.head.insideChart( i )( j )( mDecoration ) += mCellScore( i, k, j, mDecoration )
      }
    }
  }

  def computeInsideRootScore() {
    rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
      val r = intString( k )

      particles.head.stringProb +=
        rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight )
    }
  }

  def computeInsideRightwardScore( i:Int, j:Int ) {
    rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        particles.head.insideChart( i )( j )( pDec ) +=
          rightwardCellScore( i, k, j, pDec, mDec, cDec )
      }
    }
  }

  def computeInsideLeftwardScore( i:Int, j:Int ) {
    leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        particles.head.insideChart( i )( j )( pDec ) +=
          leftwardCellScore( i, k, j, pDec, mDec, cDec )
      }
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

  def insidePass( s:Array[Int] ) = {
    intString = s
    particles.head.intString = intString
    (1 to ( intString.length )).foreach{ j =>
      lexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          synFill( i , j )
        }
    }
  }

  def logProb( string:Array[Int] ) = {
    // val s = string.flatMap{ w => Seq(w,w) }
    val s = doubleString( string )
    clearCharts
    insidePass( s )
    math.log(particles.head.stringProb)
  }


  def clearCharts {
    (0 until 2*maxLength ).foreach{ i =>
      (0 until (2*maxLength)+1 ).foreach{ j =>
        particles.head.insideChart(i)(j).keys.foreach{ vs =>
          insideChart(i)(j)(vs) = 0D
          outsideChart(i)(j)(vs) = 0D
          particles.head.insideChart(i)(j)(vs) = 0D
          particles.head.outsideChart(i)(j)(vs) = 0D
        }
        if( i%2 != j%2 ) {
          particles.head.headTrace(i)(j).clear
        } else if( i%2 == 1 && j%2 == 1 ) {
          particles.head.mTrace(i)(j).clear
        }
      }
    }
    particles.head.treeRoot = null
    particles.head.stringProb = 0D
  }

  def doubleString( string:Array[Int] ) = {
    string.toSeq.flatMap{ w => List(w,w) }.toArray
  }

  // Viterbi definitions -- largely copied from FoldUnfoldNOPOSParser

  // val headTrace = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
  //   MMap[Decoration,Entry]()
  // )
  // val mTrace = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
  //   MMap[MDecoration,MEntry]()
  // )

  // abstract class Entry( i:Int, j:Int ) {
  //   def toDepParse:Set[DirectedArc]
  //   def toConParse:String
  // }

  // object ParseFailed extends Entry( -1, -1 ) {
  //   def toDepParse = Set()
  //   def toConParse = "PARSE FAILED"
  // }

  // case class LexEntry( index:Int ) extends Entry( index, index+1 ) {
  //   def toDepParse = Set()
  //   def toConParse = s"(${intString(index)} ${intString(index)})"
  // }

  // abstract class BinaryEntry( i:Int, k:Int, j:Int ) extends Entry( i, j ) {
  //   val leftChild:Entry
  //   val rightChild:Entry
  // }

  def findLeftRootChild( k:Int ) = particles.head.findLeftRootChild( k )
  def findRightRootChild( k:Int ) = particles.head.findRightRootChild( k )

  def findLeftLeftwardChild( i:Int, k:Int ) = particles.head.findLeftLeftwardChild( i, k )
  def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) =
    particles.head.findRightLeftwardChild( k, j, hV, mDV )

  def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) =
    particles.head.findLeftRightwardChild( i, k, hV, mDV )
  def findRightRightwardChild( k:Int, j:Int ) =
    particles.head.findRightRightwardChild( k, j )

  def findLeftMChild( i:Int, k:Int, decoration:MDecoration ) =
    particles.head.findLeftMChild( i, k, decoration )
  def findRightMChild( k:Int, j:Int, decoration:MDecoration ) =
    particles.head.findRightMChild( k, j, decoration )


  def rightwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    particles.head.insideChart( i )( k )( mDec ) *
      particles.head.insideChart( k )( j )( cDec ) * {
        (0 until numParticles).map{ l =>
          particleWeights( l ) *
            particles(l).rightwardCellFactor( i, k, j, pDec, mDec, cDec )
        }.sum
      }
  }

  def leftwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    particles.head.insideChart( i )( k )( cDec ) *
      particles.head.insideChart( k )( j )( mDec ) *
        (0 until numParticles).map{ l =>
          particleWeights( l ) *
            particles( l ).leftwardCellFactor( i, k, j, pDec, mDec, cDec )
        }.sum
  }

  def mCellScore( i:Int, k:Int, j:Int, mDecoration:MDecoration ) = {
    if( k%2 == 0 ) {
      particles.head.insideChart( i )( k )( mDecoration.evenLeft ) *
        particles.head.insideChart( k )( j )( mDecoration.evenRight ) *
          (0 until numParticles).map{ l =>
            particleWeights(l) *
              particles(l).mCellFactor( i, k, j, mDecoration )
          }.sum
    } else {
      particles.head.insideChart( i )( k )( mDecoration.oddLeft ) *
        particles.head.insideChart( k )( j )( mDecoration.oddRight ) *
          (0 until numParticles).map{ l =>
            particleWeights(l) *
              particles(l).mCellFactor( i, k, j, mDecoration )
          }.sum
    }
  }

  def rootCellScore( k:Int, leftDec:Decoration, rightDec:Decoration ) = {
    particles.head.insideChart( 0 )( k )( leftDec ) *
      particles.head.insideChart( k )( intString.length )( rightDec ) *
        (0 until numParticles).map{ l =>
          particleWeights(l) *
            particles(l).rootCellFactor( k )
        }.sum
  }




  def rightwardCellScores( i:Int, j:Int ) = {
    particles.head.rightwardSplitSpecs( i, j ).map{ case ( pDec, splits ) =>
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
    particles.head.leftwardSplitSpecs( i, j ).map{ case ( pDec, splits ) =>
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
    particles.head.mSplitSpecs( i, j ).map{ case (mDecoration, splits) =>
      (
        mDecoration,
        splits.view.map{ k => 
          // println( (i,k,j,mDecoration, mCellScore( i, k, j, mDecoration ) ) )
          (k, mCellScore( i, k, j, mDecoration ) )
        }
      )
    }
  }

  def rootCellScores() = {
    particles.head.rootSplitSpecs().map{ case ( k, decorationPair ) =>
      // println( k, decorationPair )
      (
        ( k, decorationPair ),
        rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight )
      )
    }
  }


  def viterbiSynFill( i:Int, j:Int ) {
    // println( i,j )
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
      rightwardCellScores( i, j ).foreach{ case( pDec, splitsAndScores ) =>
        val ( (k,mDec,cDec), bestScore ) = argMax( splitsAndScores )
        particles.head.insideChart( i )( j )( pDec ) = bestScore
        particles.head.insertRightwardEntry( i, k, j, pDec, mDec.evenLeft, mDec.evenRight )
      }
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward

      leftwardCellScores( i, j ).foreach{ case( pDec, splitsAndScores ) =>
        val ( (k,mDec,cDec), bestScore ) = argMax( splitsAndScores )
        particles.head.insideChart( i )( j )( pDec ) = bestScore
        particles.head.insertLeftwardEntry( i, k, j, pDec, mDec.evenRight, mDec.evenLeft )
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M

      mCellScores(i,j).foreach{ case (mDecoration, splitsAndScores) =>
        val ( bestK, bestScore ) = argMax( splitsAndScores )
        particles.head.insideChart( i )( j )( mDecoration ) = bestScore
        particles.head.insertMEntry( i, bestK, j, mDecoration )
        // particles.head.mTrace( i )( j )( mDecoration ) =
        //   FoldUnfoldNOPOSParser.MEntry( i, bestK, j, mDecoration )
      }
    } else {
      // Root

      if( i == 0 && j == intString.length ) {
        val ( (bestK,_), bestScore ) = argMax( rootCellScores() )
        particles.head.stringProb = bestScore
        particles.head.insertRootEntry( bestK )
      }
    }
  }


  // def lexSpecs( index:Int ) = particles.head.lexSpecs( index )
  def lexCellFactor( index:Int, pDec:Decoration ) = {
    (0 until numParticles).map{ l =>
      particleWeights( l ) *
        particles( l ).lexCellFactor( index, pDec )
    }.sum
  }
  def viterbiLexFill( index:Int ) {
    particles.head.lexSpecs( index ).foreach{ pDec =>
      particles.head.insideChart( index )( index+1 )(pDec) = lexCellFactor( index, pDec )
      particles.head.insertLexEntry( index, pDec )
    }
  }

  def viterbiParse( utt:Utt ) = {
    clearCharts
    intString = doubleString( utt.string )
    particles.head.intString = intString
    (0 until numParticles).foreach{ l => particles(l).intString = intString }

    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse(
      utt.id,
      particles.head.treeRoot.toConParse,
      particles.head.treeRoot.toDepParse
    )
  }

  def viterbiDepParse( utt:Utt ) = {
    clearCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    particles.head.intString = intString
    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    Parse( utt.id, "", particles.head.treeRoot.toDepParse )
  }

  // TODO implement me?
  def sampleTreeCounts( utt:Utt ):Tuple2[DMVCounts,Double] = (emptyCounts, Double.NaN)
}

