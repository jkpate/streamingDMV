package streamingDMV.parsers

import streamingDMV.labels._
// import streamingDMV.parameters.NOPOSArcFactoredParameters
import streamingDMV.parameters.ArcFactoredParameters
// import streamingDMV.math.LogSum
// import streamingDMV.parsers.FoldUnfoldNOPOSParser.RootEntry

import scala.collection.mutable.{Map=>MMap}
import scala.reflect.ClassTag

class ParticleFilterNOPOSParser[
  C<:DependencyCounts,
  P<:ArcFactoredParameters[C],
  R<:FoldUnfoldNOPOSParser[C,P]:ClassTag
](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  parserSpec:ParserSpec,
  numParticles:Int = 16,
  createParticle:(C,Array[SampledCounts[C]],Int) => R,
  // randomSeed:Int = 15,
  val emptyCounts:C//,
  // reservoirSize:Int
) extends ParticleFilterParser[C,P,R](
  // maxLength,
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha,
  // numParticles,
  // createParticle,
  // randomSeed,
  // reservoirSize
  parserSpec,
  numParticles,
  createParticle
) {



  // def emptyCounts = DMVCounts( rootAlpha, stopAlpha, chooseAlpha, true )
  // def emptyCounts = particles.head.emptyCounts

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
      particles.head.insideChart( index )( index+1 )(pDec) = myPlus(
        particles.head.insideChart( index )( index+1 )(pDec),
        lexCellFactor( index, pDec )
      )
    }
  }

  def computeInsideMScore( i:Int, j:Int ) {
    mSplitSpecs(i,j).foreach{ case (mDecoration, splits) =>
      splits.foreach{ k =>
        particles.head.insideChart( i )( j )( mDecoration ) = myPlus(
          particles.head.insideChart( i )( j )( mDecoration ),
          mCellScore( i, k, j, mDecoration )
        )
      }
    }
  }

  def computeInsideRootScore() {
    rootSplitSpecs().foreach{ case ( k, decorationPair ) =>
      val r = intString( k )

      particles.head.stringProb = myPlus(
        particles.head.stringProb,
        rootCellScore( k, decorationPair.evenLeft, decorationPair.evenRight )
      )
    }
  }

  def computeInsideRightwardScore( i:Int, j:Int ) {
    rightwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        particles.head.insideChart( i )( j )( pDec ) = myPlus(
          particles.head.insideChart( i )( j )( pDec ),
          rightwardCellScore( i, k, j, pDec, mDec, cDec )
        )
      }
    }
  }

  def computeInsideLeftwardScore( i:Int, j:Int ) {
    leftwardSplitSpecs( i, j ).foreach{ case ( pDec, splits ) =>
      splits.foreach{ case ( k, mDec, cDec ) =>
        particles.head.insideChart( i )( j )( pDec ) = myPlus(
          particles.head.insideChart( i )( j )( pDec ),
          leftwardCellScore( i, k, j, pDec, mDec, cDec )
        )
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
    // println( s.mkString( " " ) )
    particles.foreach( _.intString = intString )
    if( intString.length > particles.head.insideChart.length ) {
      particles.head.buildCharts( intString.length )
    }

    // println( s"intString.length: ${intString.length}" )
    // println( s"particles.head.insideChart.length: ${particles.head.insideChart.length}" )

    (1 to ( intString.length )).foreach{ j =>
      // println( s"> particles.head.insideChart.length: ${particles.head.insideChart.length}" )
      lexFill( j-1 )
      //println( s">> particles.head.insideChart.length: ${particles.head.insideChart.length}" )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          // println( s">>> particles.head.insideChart.length: ${particles.head.insideChart.length}" )
          synFill( i , j )
          // println( s">>>> ${(i,j)} particles.head.insideChart.length: ${particles.head.insideChart.length}" )
        }
    }
  }

  def logProb( string:Array[Int] ) = {
    // val s = string.flatMap{ w => Seq(w,w) }
    val s = doubleString( string )
    clearCharts
    (0 until numParticles).foreach{ l =>
      particles(l).theta.fullyNormalized = true
      // println( particles(l).theta.p_choose.denoms.map{ v=> (v._1, v._2.size ) } )
    }
    assert( math.abs( particleWeights.sum -1 ) < 0.000001 )
    insidePass( s )
    (0 until numParticles).foreach{ l =>
      particles(l).theta.fullyNormalized = false
    }
    math.log(particles.head.stringProb)
  }


      // def clearCharts {
      //   (0 until 2*maxLength ).foreach{ i =>
      //     (0 until (2*maxLength)+1 ).foreach{ j =>
      //       particles.head.insideChart(i)(j).keys.foreach{ vs =>
      //         insideChart(i)(j)(vs) = 0D
      //         outsideChart(i)(j)(vs) = 0D
      //         particles.head.insideChart(i)(j)(vs) = 0D
      //         particles.head.outsideChart(i)(j)(vs) = 0D
      //       }
      //       if( i%2 != j%2 ) {
      //         particles.head.headTrace(i)(j).clear
      //       } else if( i%2 == 1 && j%2 == 1 ) {
      //         particles.head.mTrace(i)(j).clear
      //       }
      //     }
      //   }
      //   particles.head.treeRoot = null
      //   particles.head.stringProb = 0D
      // }

  def doubleString( string:Array[Int] ) = {
    string.toSeq.flatMap{ w => List(w,w) }.toArray
  }

  def clearVitCharts {
    (0 until particles.head.headTrace.length ).foreach{ i =>
      (0 until particles.head.headTrace.length+1 ).foreach{ j =>
        particles.head.insideChart(i)(j).keys.foreach{ vs =>
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

  def clearCharts {
    (0 until particles.head.insideChart.length ).foreach{ i =>
      (0 until particles.head.insideChart.length+1 ).foreach{ j =>
        particles.head.insideChart(i)(j).keys.foreach{ vs =>
          particles.head.insideChart(i)(j)(vs) = 0D
          particles.head.outsideChart(i)(j)(vs) = 0D
        }
      }
    }
    particles.head.treeRoot = null
    particles.head.stringProb = 0D
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
    myTimes(
      particles.head.insideChart( i )( k )( mDec ),
        particles.head.insideChart( k )( j )( cDec ),
        myPlusSeq(
          (0 until numParticles).map{ l =>
            myTimes(
              particleWeights( l ),
                particles(l).rightwardCellFactor( i, k, j, pDec, mDec, cDec )
            )
          }
        )
    )
  }

  def leftwardCellScore( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    myTimes(
      particles.head.insideChart( i )( k )( cDec ),
        particles.head.insideChart( k )( j )( mDec ),
        myPlusSeq(
          (0 until numParticles).map{ l =>
            myTimes(
              particleWeights( l ),
                particles( l ).leftwardCellFactor( i, k, j, pDec, mDec, cDec )
            )
          }
        )
    )
  }

  def mCellScore( i:Int, k:Int, j:Int, mDecoration:MDecoration ) = {
    if( k%2 == 0 ) {
      myTimes(
        particles.head.insideChart( i )( k )( mDecoration.evenLeft ),
          particles.head.insideChart( k )( j )( mDecoration.evenRight ),
          myPlusSeq(
            (0 until numParticles).map{ l =>
              particleWeights(l) *
                particles(l).mCellFactor( i, k, j, mDecoration )
            }
          )
      )
    } else {
      myTimes(
        particles.head.insideChart( i )( k )( mDecoration.oddLeft ),
          particles.head.insideChart( k )( j )( mDecoration.oddRight ),
          myPlusSeq(
            (0 until numParticles).map{ l =>
              particleWeights(l) *
                particles(l).mCellFactor( i, k, j, mDecoration )
            }
          )
      )
    }
  }

  def rootCellScore( k:Int, leftDec:Decoration, rightDec:Decoration ) = {
    myTimes(
      particles.head.insideChart( 0 )( k )( leftDec ),
        particles.head.insideChart( k )( intString.length )( rightDec ),
        myPlusSeq(
          (0 until numParticles).map{ l =>
            // println( s"particleWeights(l): ${particleWeights(l)}" )
            particleWeights(l) *
              particles(l).rootCellFactor( k )
          }
        )
    )
  }



  def lexCellScores( index:Int ) = {
    particles.head.lexSpecs( index ).map{ pDec =>
      (
        pDec, Seq( lexCellFactor( index, pDec ) )
      )
    }
  }

  def rightwardCellScores( i:Int, j:Int ) = {
    particles.head.rightwardSplitSpecs( i, j ).map{ case ( pDec, splits ) =>
      (
        pDec,
        splits.map{ case ( k, mDec, cDec ) =>
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
        splits.map{ case ( k, mDec, cDec ) =>
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
        splits.map{ k => 
          // println( (i,k,j,mDecoration, mCellScore( i, k, j, mDecoration ) ) )
          (k, mCellScore( i, k, j, mDecoration ) )
        }
      )
    }
  }

  def rootCellScores() = {
    particles.head.rootSplitSpecs().map{ case ( k, decorationPair ) =>
          // println( ( k, decorationPair, rootCellScore( k, decorationPair.evenLeft,
          //   decorationPair.evenRight ) ) )
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
      // TODO fix me for when there might be more than one possible production?
      particles.head.insideChart( index )( index+1 )(pDec) = lexCellFactor( index, pDec )
      particles.head.insertLexEntry( index, pDec )
    }
  }

  def viterbiParse( utt:Utt ) = {
    clearVitCharts
    intString = doubleString( utt.string )
    (0 until numParticles).foreach{ l =>
      particles(l).intString = intString
      particles(l).theta.fullyNormalized = true
    }
    if( intString.length > particles.head.headTrace.length ) {
      particles.head.buildVitCharts( intString.length )
    }
    if( intString.length > particles.head.insideChart.length ) {
      particles.head.buildCharts( intString.length )
    }

    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    (0 until numParticles).foreach{ l =>
      particles(l).theta.fullyNormalized = false
    }
    Parse(
      utt.id,
      particles.head.treeRoot.toConParse,
      particles.head.treeRoot.toDepParse
    )
  }

  def viterbiDepParse( utt:Utt ) = {
    clearVitCharts
    // intString = utt.string.flatMap{ w => Seq(w,w) }
    intString = doubleString( utt.string )
    (0 until numParticles).foreach{ l =>
      particles(l).intString = intString
      particles(l).theta.fullyNormalized = true
      // assert( particles(l).approximate )
    }

    if( !(math.abs( particleWeights.sum -1 ) < 0.000001 ) ) {
      println( particleWeights.mkString("; " ) )
    }

    assert( math.abs( particleWeights.sum -1 ) < 0.000001 )

    if( intString.length > particles.head.headTrace.length ) {
      particles.head.buildVitCharts( intString.length )
    }
    if( intString.length > particles.head.insideChart.length ) {
      particles.head.buildCharts( intString.length )
    }


    (1 to ( intString.length )).foreach{ j =>
      viterbiLexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          viterbiSynFill( i , j )
        }
    }
    (0 until numParticles).foreach{ l =>
      particles(l).theta.fullyNormalized = false
    }
    Parse( utt.id, "", particles.head.treeRoot.toDepParse )
  }

  def sampleTreeCounts( i:Int, j:Int, pDec:Decoration ):Seq[Tuple2[Event,Double]] = {
    if( i%2 == 1 && j%2 == 1 ) { // M

      // val splitsAndScores =
      mCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
        assert( parent == pDec )
        val ( k, score ) = argSample( splitsAndScores )

        sampleScore += score

        if( k%2 == 0 )
          particles.head.mEventCounts( i, k, j, parent, 0D ) ++
            sampleTreeCounts( i, k, parent.evenLeft ) ++
              sampleTreeCounts( k, j, parent.evenRight )
        else
          particles.head.mEventCounts( i, k, j, parent, 0D ) ++
            sampleTreeCounts( i, k, parent.oddLeft ) ++
              sampleTreeCounts( k, j, parent.oddRight )
      }

    } else if( i%2 == 1 ) { // Rightward

      // val splitsAndScores =
      if( j-i > 1 ) {
        rightwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case (parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , score ) = argSample( splitsAndScores )

          sampleScore += score

          particles.head.rightwardEventCounts( i, k, j, pDec, mDec, cDec, 0D ) ++
            sampleTreeCounts( i, k, mDec ) ++
              sampleTreeCounts( k, j, cDec )
        }
      } else {
        // lexMarginals( j )
        lexCellScores( i ).filter( _._1 == pDec ).flatMap{ case ( parent, scores ) =>
          assert( parent == pDec )
          assert( scores.length == 1 )

          sampleScore += scores.head
          // Seq()
          particles.head.lexEventCounts( i, pDec, 0D )
        }
      }

    } else if( j%2 == 1 ) { // Leftward

      // val splitsAndScores =
      if( j-i > 1 ) {
        leftwardCellScores(i,j).filter(_._1 == pDec).flatMap{ case ( parent, splitsAndScores ) =>
          assert( parent == pDec )
          val ( (k, mDec, cDec) , score ) = argSample( splitsAndScores )

          sampleScore += score

          particles.head.leftwardEventCounts( i, k, j, pDec, mDec, cDec, 0D ) ++
            sampleTreeCounts( i, k, cDec ) ++
              sampleTreeCounts( k, j, mDec )
        }
      } else {
        // lexMarginals( i )
        lexCellScores( i ).filter( _._1 == pDec ).flatMap{ case ( parent, scores ) =>
          assert( parent == pDec )
          assert( scores.length == 1 )

          sampleScore += scores.head
          particles.head.lexEventCounts( i, pDec, 0D )
        }
      }

    } else if( i == 0 && j == intString.length ) { // Root
      assert( pDec == RootDecoration )

      val ((k,cDecs), score) = argSample( rootCellScores() )

      sampleScore += score

      particles.head.rootEventCounts( k, 0D ) ++
        sampleTreeCounts( i , k, cDecs.evenLeft ) ++
        sampleTreeCounts( k, intString.length, cDecs.evenRight )

    } else {
      Seq()
    }
  }

  var sampleScore = 0D
  // TODO implement me?
  def sampleTreeCounts( originalString:Array[Int] ):Tuple2[C,Double] = {
    if( particles.head.stringProb == 0D ) {
      // Only compute inside scores once per sentence -- we'll be drawing one set of counts for each
      // particle.
      val s = doubleString( originalString )
      insidePass( s )
    }
    sampleScore = 0D

    val c = emptyCounts
    sampleScore = 0D
    sampleTreeCounts( 0, intString.length, RootDecoration ).foreach{ case (event, count) =>
      c.increment( event, count )
      // event match {
      //   case e:StopEvent => c.stopCounts.increment( e, count )
      //   case e:ChooseEvent => c.chooseCounts.increment( e, count )
      //   case e:RootEvent => c.rootCounts.increment( e, count )
      // }
    }

    // (emptyCounts, Double.NaN)
    ( c, sampleScore )
  }
}

