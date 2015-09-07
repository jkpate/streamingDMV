package streamingDMV.test

import streamingDMV.parsers._
import streamingDMV.parameters._
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
    Array( 1, 2, 3, 1, 6 ),
    Array.fill( 9 )( r.nextInt( 15 ) ),
    Array.fill( 15 )( r.nextInt( 15 ) )
  )

  val idDMVCorpus = dmvCorpus.zipWithIndex.map{ case (v,idx) => 
    Utt( s"utt$idx", v )
  }


  val uposCount = 20
  // val uposCount = 3

  val maxLength = dmvCorpus.map{_.length}.max
  println( s"maxLength: $maxLength" )

  val p = new TopDownDMVParser( maxLength, randomSeed = 15 )
  // val p = new OriginalDMVParser( maxLength, randomSeed = 15 )
  // val p = new NoValenceParser( maxLength, randomSeed = 15 )
  // val p = new HeadOutAdjHeadNoValenceParser(
  //   maxLength,
  //   randomSeed = 15
  // )
  // val p = new HeadOutInterpolatedAdjHeadNoValenceParser(
  //   maxLength,
  //   notBackoffAlpha = 10,
  //   backoffAlpha = 0,
  //   randomSeed = 15
  // )
  // val p = new NoValenceUPOSParser(
  //   maxLength,
  //   uposCount = uposCount,
  //   randomSeed = 15
  // )


  p.zerosInit( idDMVCorpus )
  // p.randomInit( idDMVCorpus, 15, 100 )

  val iters = 10

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
      val endTime = System.currentTimeMillis

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


      println(
        {
          p.insideChart(0)(1).keys.map{ k => p.insideChart(0)(1)(k) * p.outsideChart(0)(1)(k) }.sum
          // p.insideChart(0)(1).keys.map{ k => sum( p.insideChart(0)(1)(k) :* p.outsideChart(0)(1)(k)) }.sum
        } + " <=> " + pObs
      )
      // println( "all terminals:" )
      // (0 to ((2*s.length)-1)).foreach{ i =>
      //   println(
      //     {
      //       p.insideChart(i)(i+1).keys.map{ k => p.insideChart(i)(i+1)(k) * p.outsideChart(i)(i+1)(k) }.sum
      //       // p.insideChart(0)(1).keys.map{ k => sum( p.insideChart(0)(1)(k) :* p.outsideChart(0)(1)(k)) }.sum
      //     } + " <=> " + pObs
      //   )
      // }
      (0 to ((2*s.length)-1)).foreach{ i =>
        assertTrue(
          {
            p.insideChart(i)(i+1).keys.map{ k => p.insideChart(i)(i+1)(k) * p.outsideChart(i)(i+1)(k) }.sum
            // p.insideChart(i)(i+1).keys.map{ k => sum( p.insideChart(i)(i+1)(k) :* p.outsideChart(i)(i+1)(k)) }.sum
          } - pObs < 0.0000001
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

  @Test def testSampleCounts {
    val startTime = System.currentTimeMillis
    idDMVCorpus.foreach{ s =>
    // Array( idDMVCorpus.head ).foreach{ s =>
      println( s.string.mkString(" " ) )

      // println(
      //   p.viterbiParse( s )
      // )

      // val pObs = p.stringProb
      val ( sampleCounts, treeLogProb ) = p.sampleTreeCounts( s )
      sampleCounts.printTotalCountsByType
      println( sampleCounts.totalCounts + " total events sampled" )
      println( s"tree logProb is: ${treeLogProb}" )


    }
    val endTime = System.currentTimeMillis

    println( (endTime-startTime) / (idDMVCorpus.size) + "ms per sampled sentence" )
  }

    @Test def testParticleFilter {
      val numParticles = 10

      val startTime = System.currentTimeMillis

      val pf =
        new ParticleFilterNOPOSParser[TopDownDMVParameters,TopDownDMVParser](
          maxLength = maxLength,
          numParticles = numParticles,
          createParticle = (counts:DMVCounts,l:Int) => {
            val p_l = new TopDownDMVParser( maxLength, randomSeed = 15 + 32*l )
            p_l.zerosInit( idDMVCorpus )
            p_l.theta.incrementCounts( counts )
            p_l
          }
        )


      (0 until iters).foreach{ _ => pf.resampleParticles }
      // println( "---" )
      // pf.resampleParticles
      // println( "---" )
      // pf.resampleParticles
      // println( "---" )
            // (0 until 4 ).foreach{ i =>
            //   println( 
            //     "particle ess before update: " + pf.ess
            //   )
            //   pf.streamingBayesUpdate( idDMVCorpus )
            //   idDMVCorpus.foreach{ s =>
            //     println( s.string.mkString(" " ) )

            //     println( 
            //       pf.viterbiParse( s )
            //     )
            //     println( 
            //       "particle ess after update: " + pf.ess
            //     )
            //     pf.resampleParticles
            //     println( "---" )

            //     val pObs = pf.stringProb

            //   }
            // }
      val endTime = System.currentTimeMillis

      println( (endTime-startTime) / (iters*numParticles) + "ms per particle" )


    }


}

