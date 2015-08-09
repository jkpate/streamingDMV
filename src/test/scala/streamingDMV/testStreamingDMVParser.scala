package streamingDMV.test

import streamingDMV.parsers._
import streamingDMV.labels._

import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.Suite
import org.junit.Assert._
import org.junit.Test
import org.junit.Before


class FastDMVParserTestSuite extends AssertionsForJUnit with Suite {
  val r = new util.Random( 16747 )
  val dmvCorpus = List(
    Array( 0, 5, 3, 1, 2, 4 ),
    Array( 1, 2, 3, 1, 6 ),
    Array.fill( 9 )( r.nextInt( 15 ) ),
    Array.fill( 15 )( r.nextInt( 15 ) )
  )

  val idDMVCorpus = dmvCorpus.zipWithIndex.map{ case (v,idx) => 
    Utt( s"utt$idx", v )
  }


  // val p = new TopDownDMVParser( dmvCorpus.map{_.length}.max )
  // val p = new OriginalDMVParser( dmvCorpus.map{_.length}.max )
  // val p = new HeadOutAdjHeadNoValenceParser(
  //   dmvCorpus.map{_.length}.max,
  //   randomSeed = 15
  // )
  // val p = new NoValenceParser(
  //   dmvCorpus.map{_.length}.max,
  //   randomSeed = 15
  // )
  val p = new HeadOutInterpolatedAdjHeadNoValenceParser(
    dmvCorpus.map{_.length}.max,
    notBackoffAlpha = 0,
    randomSeed = 15
  )


  // p.zerosInit( idDMVCorpus )
  p.randomInit( idDMVCorpus, 15, 1 )

  val iters = 1//000

  @Test def testInsideOutside {

    val startTime = System.currentTimeMillis
    dmvCorpus.foreach{ s =>
      println( s.mkString(" " ) )
      var i = 0
      var c = DMVCounts()
      while( i < iters ) {
        // p.populateChart( s )
        c = p.extractPartialCounts( s )
        i += 1
      }
      // p.seeInsideHeads()
      // val chart = p.populateChart( s )
      // val pObs = chart.pObs
      val pObs = p.stringProb


      println(
        {
          p.insideHeads(0)(1).keys.map{ k => p.insideHeads(0)(1)(k) * p.outsideHeads(0)(1)(k) }.sum
          // p.insideHeads(0)(1)( Inner ) * p.outsideHeads(0)(1)( Inner ) +
          // p.insideHeads(0)(1)( Outermost ) * p.outsideHeads(0)(1)( Outermost ) 
          // p.insideHeads(0)(1)( Innermost ) * p.outsideHeads(0)(1)( Innermost ) 
        } + " <=> " + pObs
      )
      // println( "all terminals:" )
      // (0 to ((2*s.length)-1)).foreach{ i =>
      //   println(
      //     {
      //       p.insideHeads(i)(i+1).keys.map{ k => p.insideHeads(i)(i+1)(k) * p.outsideHeads(i)(i+1)(k) }.sum
      //     } + " <=> " + pObs
      //   )
      // }
      (0 to ((2*s.length)-1)).foreach{ i =>
        assertTrue(
          // p.insideHeads(i)(i+1) * p.outsideHeads(i)(i+1)
          {
            p.insideHeads(i)(i+1).keys.map{ k => p.insideHeads(i)(i+1)(k) * p.outsideHeads(i)(i+1)(k) }.sum
            // p.insideHeads(i)(i+1)( Inner ) * p.outsideHeads(i)(i+1)( Inner ) +
            // p.insideHeads(i)(i+1)( Outermost ) * p.outsideHeads(i)(i+1)( Outermost ) 
            // p.insideHeads(i)(i+1)( Innermost ) * p.outsideHeads(i)(i+1)( Innermost ) 
          } - pObs < 0.000000001
        )
      }
      p.theta.incrementCounts( c )
    }
    val endTime = System.currentTimeMillis

    println( (endTime-startTime) / (iters * 2D) + "ms per sentence" )

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



}

