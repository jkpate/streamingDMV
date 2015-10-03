package streamingDMV.test

import streamingDMV.parsers._
import streamingDMV.parameters._
import streamingDMV.math.LogSum
import streamingDMV.labels._

import breeze.linalg._
import breeze.numerics._

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class FastDMVParserTestSuite extends AssertionsForJUnit with Suite {
  val r = new util.Random( 16747 )
  val dmvCorpus = List(
    Array( 0, 5, 3, 1, 2, 4 ),
    Array( 0, 5, 3, 1, 2, 4 ),
    Array( 1, 2, 3, 1, 6, 4 )/*,
    Array.fill( 9 )( r.nextInt( 15 ) ),
    Array.fill( 15 )( r.nextInt( 15 ) )*/
  )

  val idDMVCorpus = dmvCorpus.zipWithIndex.map{ case (v,idx) => 
    Utt( s"utt$idx", v )
  }


  val uposCount = 20
  // val uposCount = 3

  val maxLength = dmvCorpus.map{_.length}.max
  println( s"maxLength: $maxLength" )

  val alpha = 1D
  val backoffAlpha = 10
  val notBackoffAlpha = 1
  val squareNorm = dmvCorpus.map{_.length}.max
  val reservoirSize = 100
  val randomSeed = 15

  val parserSpec = ParserSpec(
    maxLength = maxLength,
    randomSeed = randomSeed,
    rootAlpha = alpha,
    stopAlpha = alpha,
    chooseAlpha = alpha,
    backoffAlpha = backoffAlpha,
    notBackoffAlpha = notBackoffAlpha,
    squarelyNormalized = squareNorm,
    approximate = false,
    reservoirSize = reservoirSize,
    logSpace = false
  )

  // val p = new FiveValenceParser( parserSpec )
  // val p = new FourValenceParser( parserSpec )
  // val p = new ThreeValenceParser( parserSpec )
  // val p = new TopDownDMVParser( parserSpec )
  val p = new OriginalDMVParser( parserSpec )
  // val p = new NoValenceParser( parserSpec )
  // val p = new HeadOutAdjHeadNoValenceParser(
  //   parserSpec
  // )
  // val p = new HeadOutInterpolatedAdjHeadNoValenceParser(
  //   parserSpec
  // )
  // val p = new NoValenceUPOSParser(
  //   parserSpec,
  //   uposCount = uposCount
  // )


  p.zerosInit( idDMVCorpus )
  // p.randomInit( idDMVCorpus, 15, 100 )

  val iters = 1//00//0

  @Test def testInsideOutside {

    var totalTime = 0D
    dmvCorpus.foreach{ s =>
      println( s.mkString(" " ) )
      var i = 0
      // var c = DMVCounts()
      // var c = MatrixDMVCounts( uposCount = uposCount )
      var c = p.emptyCounts
      val startTime = System.currentTimeMillis
      while( i < iters ) {
        // p.populateChart( s )
        c = p.extractPartialCounts( s )
        i += 1
      }
      p.theta.incrementCounts( c )
      p.theta.decrementCounts( c )
      p.theta.incrementCounts( c )
      val endTime = System.currentTimeMillis

      // println(
      //   p.chartToString( "Inside", p.insideChart, logSpace = false )
      // )

      // println( "now we at" )
      // p.theta.p_root.printOut()
      // p.theta.p_stop.printOut()
      // p.theta.p_choose.printOut()

      totalTime += ( endTime - startTime )
      // p.seeInsideHeads()
      // val chart = p.populateChart( s )
      // val pObs = chart.pObs
      val pObs = p.stringProb

      c.printTotalCountsByType
      println( c.totalCounts + " total events seen" )
      println( StopEvent( 4,LeftAtt,Outermost,Stop) )
      println( Outermost + ": " + c.stopCounts( StopEvent( 4,LeftAtt,Outermost,Stop) ) )
      println( Inner + ": " + c.stopCounts( StopEvent( 4,LeftAtt,Inner,Stop) ) )
      println( Outer + ": " + c.stopCounts( StopEvent( 4,LeftAtt,Outer,Stop) ) )
      println( Innermost + ": " + c.stopCounts( StopEvent( 4,LeftAtt,Innermost,Stop) ) )
      println( NoDependentValence + ": " + c.stopCounts( StopEvent( 4,LeftAtt,NoDependentValence,Stop) ) )
      println( OneDependentValence + ": " + c.stopCounts( StopEvent( 4,LeftAtt,OneDependentValence,Stop) ) )
      println( TwoDependentValence + ": " + c.stopCounts( StopEvent( 4,LeftAtt,TwoDependentValence,Stop) ) )
      println( ThreeDependentValence + ": " + c.stopCounts( StopEvent( 4,LeftAtt,ThreeDependentValence,Stop) ) )
      println( FourDependentValence + ": " + c.stopCounts( StopEvent( 4,LeftAtt,FourDependentValence,Stop) ) )


      println(
        p.myPlus(
            p.insideChart(0)(1).keys.map{ k =>
              p.myTimes( p.insideChart(0)(1)(k), p.outsideChart(0)(1)(k) )
          }.toSeq:_*
        ) + " <=> " + pObs
          // p.insideChart(0)(1).keys.map{ k => sum( p.insideChart(0)(1)(k) :* p.outsideChart(0)(1)(k)) }.sum
      )
      println( "all terminals:" )
      (0 to ((2*s.length)-1)).foreach{ i =>
        println(
          p.myPlus(
            p.insideChart(i)(i+1).keys.map{ k =>
              p.myTimes( p.insideChart(i)(i+1)(k) , p.outsideChart(i)(i+1)(k) )
            }.toSeq:_*
          ) + " <=> " + pObs
            // p.insideChart(0)(1).keys.map{ k => sum( p.insideChart(0)(1)(k) :* p.outsideChart(0)(1)(k)) }.sum
        )
      }
      (0 to ((2*s.length)-1)).foreach{ i =>
        assertTrue(
          p.myPlus(
            p.insideChart(i)(i+1).keys.map{ k =>
              p.myTimes( p.insideChart(i)(i+1)(k), p.outsideChart(i)(i+1)(k) )
            }.toSeq:_*
          ) - pObs < 0.000001
            // p.insideChart(i)(i+1).keys.map{ k => sum( p.insideChart(i)(i+1)(k) :* p.outsideChart(i)(i+1)(k)) }.sum
        )
      }
      // p.theta.p_stop.printOut()
      // println( "\n\n===================\n\n" )
      // p.theta.p_stop.printOut()
      println( "\n" )
    }

    println( totalTime / ( iters.toDouble * dmvCorpus.size )  + "ms per sentence" )

  }

  @Test def testViterbi {
    val startTime = System.currentTimeMillis
    idDMVCorpus.foreach{ s =>
      println( s.string.mkString(" " ) )

      println( 
        p.viterbiParse( s )
      )

      val pObs = p.stringProb

    }
    val endTime = System.currentTimeMillis

    println( (endTime-startTime) / (iters * 2D) + "ms per sentence" )
  }

      // @Test def testStreamingBayesUpdate {
      //   val startTime = System.currentTimeMillis


      //   var i = 0
      //   idDMVCorpus.foreach{ s =>
      //     p.streamingBayesUpdate( s::Nil, i )
      //     i += 1
      //   }

      //   val endTime = System.currentTimeMillis

      //   println( (endTime-startTime) / (idDMVCorpus.size) + "ms per sampled sentence" )
      // }

      // @Test def testSampleCounts {
      //   val startTime = System.currentTimeMillis
      //   idDMVCorpus.foreach{ s =>
      //   // Array( idDMVCorpus.head ).foreach{ s =>
      //     println( s.string.mkString(" " ) )

      //     // println(
      //     //   p.viterbiParse( s )
      //     // )

      //     // val pObs = p.stringProb
      //     val ( sampleCounts, treeScore ) = p.sampleTreeCounts( s )
      //     val treeLogProb = { if( p.logSpace ) treeScore else log( treeScore ) }
      //     // sampleCounts.printTotalCountsByType
      //     println( sampleCounts.totalCounts + " total events sampled" )
      //     println( s"tree logProb is: ${treeLogProb}" )


      //   }
      //   val endTime = System.currentTimeMillis

      //   println( (endTime-startTime) / (idDMVCorpus.size) + "ms per sampled sentence" )
      // }

      //   @Test def testParticleFilter {
      //     val numParticles = 10

      //     val startTime = System.currentTimeMillis

      //     val pf =
      //       new ParticleFilterNOPOSParser[TopDownDMVParameters,TopDownDMVParser](
      //         maxLength = maxLength,
      //         numParticles = numParticles,
      //         createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
      //           val p_l = new TopDownDMVParser( maxLength, randomSeed = 15 + 32*l, reservoirSize = 3 )
      //           p_l.zerosInit( idDMVCorpus )
      //           p_l.theta.incrementCounts( counts )
      //           p_l.sampleReservoir = reservoir
      //           p_l
      //         },
      //         reservoirSize = 3
      //       )


      //     (0 until iters).foreach{ _ => pf.resampleParticles }
      //     // println( "---" )
      //     // pf.resampleParticles
      //     // println( "---" )
      //     // pf.resampleParticles
      //     // println( "---" )
      //           // (0 until 4 ).foreach{ i =>
      //           //   println( 
      //           //     "particle ess before update: " + pf.ess
      //           //   )
      //           //   pf.streamingBayesUpdate( idDMVCorpus )
      //           //   idDMVCorpus.foreach{ s =>
      //           //     println( s.string.mkString(" " ) )

      //           //     println( 
      //           //       pf.viterbiParse( s )
      //           //     )
      //           //     println( 
      //           //       "particle ess after update: " + pf.ess
      //           //     )
      //           //     pf.resampleParticles
      //           //     println( "---" )

      //           //     val pObs = pf.stringProb

      //           //   }
      //           // }
      //     val endTime = System.currentTimeMillis

      //     println( (endTime-startTime) / (iters*numParticles) + "ms per particle" )


      //   }


}

