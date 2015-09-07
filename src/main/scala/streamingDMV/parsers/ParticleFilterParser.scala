package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ArcFactoredParameters

import scala.collection.mutable.{Set => MSet}
import scala.math.{log,log1p,exp}
import scala.reflect.ClassTag

abstract class ParticleFilterParser[
  C<:DependencyCounts,
  P<:ArcFactoredParameters[C],
  R<:StreamingVBParser[C,P]:ClassTag
](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  numParticles:Int = 16,
  createParticle:(C,Int) => R,
  // reservoirSize:Int = 0,
  randomSeed:Int = 15
) extends FoldUnfoldParser[C,P]( maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed ) {

  val particles = Array.tabulate( numParticles )( l => createParticle(emptyCounts,l) )
  val particleWeights = Array.fill( numParticles )( 1D/numParticles )

  def ess = {
    // println( particleWeights.mkString( " " ) )
    1D / (
      particleWeights.map{ w => math.pow( w, 2D ) }.sum
    )
  }

  def particlePerplexity = {
    math.pow(
      math.E,
      -1 *
      (0 until numParticles).map{ l =>
        if( particleWeights(l) >= 0D )
          particleWeights(l) * math.log( particleWeights(l) )
        else 0D
      }.sum
    )
  }

  val theta = particles.head.theta

  override def zerosInit( corpus:List[Utt] ) {
    particles.head.theta.zerosInit( corpus )
    val zeroCounts = particles.head.theta.toCounts


    ( 1 until numParticles ).foreach{ l =>
      particles(l).theta.setEventsAndCounts( zeroCounts )
    }
  }

  // particle filters are always fully normalized
  particles.foreach{ _.theta.fullyNormalized == true }

  // val reservoir = Array.ofDim[Tuple2[List[Utt],C]](reservoirSize,numParticles)

  def resampleParticles /*( uttNum:Int )*/ = {
    val uniqueAncestors = MSet[Int]()
    val startTime = System.currentTimeMillis
    if( numParticles > 1 ) {
      // println( s"particle weights ${particleWeights.mkString("{ ",", ", " }")}" )
      val newParticleWeights = Array.fill( numParticles )( 0D )
      val newParticles = Array.tabulate( numParticles )( l => {
          val ( idx, _ ) = argSample( particleWeights.zipWithIndex.map{ p => (p._2,p._1) }.toSeq )
          // println( s" sampled particle $idx" )

          uniqueAncestors += idx

          // newParticleWeights(l) = w

          if( l == idx ) {  // Probably doesn't buy us much...
            particles(l)
          } else {
            // println( s"creating particle $l" )
            createParticle( particles(idx).theta.toCounts, rand.nextInt )
          }
        }
      )

      val totalWeight = newParticleWeights.filter{ _ > Double.NegativeInfinity }.sum
      (0 until numParticles).foreach{ l =>
        particles(l) = newParticles(l)
        particleWeights(l) = 1D/numParticles.toDouble
      }
    }
    // println( s"resampling took ${System.currentTimeMillis - startTime}ms" )
    // println( s"resampled ${uniqueAncestors.size} ancestors" )

    uniqueAncestors.size
  }

  def streamingBayesUpdate(
    miniBatch:List[Utt],
    maxIter:Int = 10,
    convergence:Double = 0.001,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ) {


    (0 until numParticles ).foreach{ l => particleWeights(l) = math.log( particleWeights(l) ) }

    ( 0 until numParticles )/*.par*/.map{ l =>
      particles(l).theta.fullyNormalized = true
      val (counts, proposalScore) = miniBatch.map{ s =>
        val (sentCounts, sentProposalScore) = particles( l ).sampleTreeCounts( s )

        assert( particles(l).stringProb > 0D )

        particleWeights(l) =
          particleWeights(l) +
            ( particles(l).trueLogProb( sentCounts ) - (
                sentProposalScore - math.log( particles(l).stringProb )
              )
            )


        (sentCounts, sentProposalScore)
      }.reduce{ (a,b) => a._1.destructivePlus( b._1 ); ( a._1, a._2 + b._2 ) }

      particles( l ).theta.incrementCounts( counts )
    }


    val totalLogWeight = particleWeights.reduce( logSum( _,_) )
    (0 until numParticles).foreach{ l =>
      particleWeights(l) = math.exp( particleWeights(l) - totalLogWeight )
    }

    // println( s"ess: ${ess}" )


        // ( 0 until numParticles )/*.par*/.foreach{ l =>
        //   particles(l).theta.fullyNormalized = true
        //   miniBatch.foreach{ s =>

        //     val (counts, proposalScore) = particles( l ).sampleTreeCounts( s )

        //     assert( particles(l).stringProb > 0D )
        //     particleWeights(l) =
        //       particleWeights(l) *
        //         math.exp( particles(l).trueLogProb( counts ) - (
        //             proposalScore - math.log( particles(l).stringProb )
        //           )
        //         )


        //     particles( l ).theta.incrementCounts( counts )
        //   }
        //   particleWeights(l) /= particleWeights.sum
        //   println( s"ess: ${ess}" )
        // }


  }

  def logSum( a:Double, b:Double ):Double =
    if( a == Double.NegativeInfinity )
      b
    else if( b == Double.NegativeInfinity )
      a
    else if( b < a )
      a + log1p( exp( b - a ) )
    else
      b + log1p( exp( a - b ) )

}

