package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ArcFactoredParameters

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
        if( particleWeights(l) >= Double.NegativeInfinity )
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

  def resampleParticles /*( uttNum:Int )*/ {
    val newParticleWeights = Array.fill( numParticles )( 0D )
    val newParticles = Array.tabulate( numParticles )( l => {
        val ( idx, w ) = argSample( particleWeights.zipWithIndex.map{ p => (p._2,p._1) }.toSeq )

        newParticleWeights(l) = w

        // println( s"particle $l" )
        createParticle( particles(idx).theta.toCounts, rand.nextInt )
      }
    )

    val totalWeight = newParticleWeights.filter{ _ > Double.NegativeInfinity }.sum
    (0 until numParticles).foreach{ l =>
      particles(l) = newParticles(l)
      particleWeights(l) = newParticleWeights(l) / totalWeight
    }

  }

  def streamingBayesUpdate(
    miniBatch:List[Utt],
    maxIter:Int = 10,
    convergence:Double = 0.001,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ) {

    // if( !( particleWeights.sum - 1D < 0.00000000001 ) ) {
    //   println( particleWeights.mkString("\t","\n\t","\n\n") )
    // }
    // assert( particleWeights.sum - 1D < 0.00000000001 )

    ( 0 until numParticles )/*.par*/.map{ l =>
      particles(l).theta.fullyNormalized = true
      val (counts, proposalScore) = miniBatch.map{ s =>
        val (sentCounts, sentProposalScore) = particles( l ).sampleTreeCounts( s )

        // println( particles( l ).stringProb )
        // println( ( particles(l).trueLogProb( sentCounts ) ) + " <=> " +
        //      ( sentProposalScore + math.log( particles( l ).stringProb ) )
        // )

        particleWeights(l) =
          math.log( particleWeights(l) ) +
            ( particles(l).trueLogProb( sentCounts ) - (
                sentProposalScore //+ math.log( particles( l ).stringProb  )
              )
            )

        val totalLogWeight = particleWeights.reduce( logSum( _,_) )
        (0 until numParticles).foreach{ l =>
          particleWeights(l) = math.exp( particleWeights(l) - totalLogWeight )
        }

        (sentCounts, sentProposalScore)
      }.reduce{ (a,b) => a._1.destructivePlus( b._1 ); ( a._1, a._2 + b._2 ) }


      particles( l ).theta.incrementCounts( counts )
    }


    // handle overflow -- set non-overflowing weights to zero.
    // TODO see if this is still necessary -- I think the overflow came from a bug in the
    // trueLogProb code (I was still multiplying even though in log space).

      // if( particleWeights.exists( _ == Double.PositiveInfinity ) ) {
      //   var totalLogWeight = Double.NegativeInfinity

      //   ( 0 until numParticles ).foreach{ l =>
      //     if( math.exp( particleWeights(l) ) == Double.PositiveInfinity ) {
      //       totalLogWeight = logSum( totalLogWeight, particleWeights(l) )
      //     } else {
      //       particleWeights(l) = Double.NegativeInfinity
      //     }
      //   }

      //   ( 0 until numParticles ).foreach{ l =>
      //     if( particleWeights(l) == Double.PositiveInfinity ) {
      //       particleWeights(l) = 1D
      //     } else if( particleWeights(l) == Double.NegativeInfinity ) {
      //       particleWeights(l) = 0D
      //     } else {
      //       particleWeights(l) = math.exp( particleWeights( l ) - totalLogWeight )
      //     }
      //   }
      // } else {
      //   val totalLogWeight = particleWeights.reduce( logSum(_,_) )
      //   (0 until numParticles).foreach{ l =>
      //     particleWeights(l) = 
      //       math.exp(
      //         particleWeights(l) - totalLogWeight
      //       )
      //   }
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

