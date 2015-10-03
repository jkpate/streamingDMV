package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.math.LogSum
import streamingDMV.parameters.ArcFactoredParameters

import scala.collection.mutable.{Set => MSet}
import scala.math.{log,log1p,exp}
import scala.reflect.ClassTag

abstract class ParticleFilterParser[
  C<:DependencyCounts,
  P<:ArcFactoredParameters[C],
  R<:StreamingVBParser[C,P]:ClassTag
](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  parserSpec:ParserSpec,
  numParticles:Int = 16,
  createParticle:(C,Array[SampledCounts[C]],Int) => R//,
  // randomSeed:Int = 15,
  // reservoirSize:Int
// ) extends FoldUnfoldParser[C,P]( maxLength, rootAlpha, stopAlpha, chooseAlpha, randomSeed ) {
) extends FoldUnfoldParser[C,P]( parserSpec ) {
  
  val reservoirSize = parserSpec.reservoirSize

  val emptyReservoir = Array.fill( reservoirSize )(
    SampledCounts( Array(), emptyCounts, myZero, myZero )
  )

  val particles = Array.tabulate( numParticles )( l => createParticle(emptyCounts,emptyReservoir,l) )
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
    // println( "--- STARTING FIRST ZEROS INIT--" )
    particles.head.theta.zerosInit( corpus )
    // println( "--- FIRST ZEROS INIT DONE--" )
    val zeroCounts = particles.head.theta.toCounts
    // println( "-- CREATED zeroCounts" )

    // zeroCounts.printTotalCountsByType


    ( 1 until numParticles ).foreach{ l =>
      // println( s"setting counts for particle $l" )
      particles(l).theta.setEventsAndCounts( zeroCounts )
    }
  }

  // particle filters are always fully normalized
  particles.foreach{ _.theta.fullyNormalized == true }


  def rejuvenate:Double = {
    // println( "rejuvenating..." )
    var acceptanceCount = 0D
    var rejectionCount = 0D
    (0 until numParticles).foreach{ l =>
      // particles(l).reservoir.foreach{ prev =>
      var sentenceCount = 0
      (0 until particles(l).reservoirSize).foreach{ reservoirIndex =>
        val prev = particles(l).sampleReservoir( reservoirIndex )

        if( prev.string.length > 0 ) {
          sentenceCount += 1
          particles(l).theta.decrementCounts( prev.counts )
          val (newCounts, newProposalScore) = particles( l ).sampleTreeCounts( prev.string )

          val newTrueScore = particles(l).trueLogProb( newCounts )
          val newSamplingScore =
            if( logSpace )
              newProposalScore - particles(l).stringProb
            else
              newProposalScore - math.log( particles(l).stringProb )

          val mhScore = (
            newTrueScore + prev.samplingScore
          ) - (
            prev.trueScore + newSamplingScore
          )

          if( mhScore > 0 || math.log( rand.nextDouble ) < mhScore ) {
            // println( s"  accepted new sample for particle $l" )
            acceptanceCount += 1
            particles(l).theta.incrementCounts( newCounts )
            particles(l).sampleReservoir( reservoirIndex ) = SampledCounts(
              prev.string, newCounts, newSamplingScore, newTrueScore
            )
          } else {
            rejectionCount += 1
            // println( s"  rejected new sample for particle $l" )
            particles(l).theta.incrementCounts( prev.counts )
          }

        }
      }
      // println( s"particle $l has $sentenceCount reservoir sentences" )
    }

    ( acceptanceCount / (acceptanceCount + rejectionCount ) )
  }

  def resampleParticles /*( uttNum:Int )*/ = {
    val uniqueAncestors = MSet[Int]()
    val startTime = System.currentTimeMillis
    if( numParticles > 1 ) {
      val newParticleWeights = Array.fill( numParticles )( 0D )
      val newParticles = Array.tabulate( numParticles )( l => {
          val ( idx, _ ) = argSample( particleWeights.zipWithIndex.map{ p => (p._2,p._1) }.toSeq )
          uniqueAncestors += idx

          if( l == idx ) {  // Probably doesn't buy us much...
            particles(l)
          } else {
            createParticle( particles(idx).theta.toCounts, particles(idx).sampleReservoir, rand.nextInt )
          }
        }
      )

      val totalWeight = newParticleWeights.filter{ _ > Double.NegativeInfinity }.sum
      (0 until numParticles).foreach{ l =>
        particles(l) = newParticles(l)
        particleWeights(l) = 1D/numParticles.toDouble
      }
    }

    val ancestorsSampled = uniqueAncestors.size

    val acceptanceRate = 
      if( reservoirSize > 0 && ancestorsSampled < numParticles / 4 )
        rejuvenate
      else
        Double.NegativeInfinity

    ( ancestorsSampled, acceptanceRate )
  }

  def clearCharts:Unit

  // TODO increment eval during initial minibatch
  def streamingBayesUpdate(
    miniBatch:List[Utt],
    sentenceNum:Int,
    testSet:List[Utt],
    maxIter:Int = 10,
    convergence:Double = 0.001,
    evalMaxLength:Int = 0,
    evalRate:Int = 10,
    logEvalRate:Boolean = true,
    constituencyEval:Boolean = true,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ) = {


    (0 until numParticles ).foreach{ l => particleWeights(l) = math.log( particleWeights(l) ) }

    ( 0 until numParticles ).map{ l =>
      particles(l).theta.fullyNormalized = true
      val (counts, proposalScore) = miniBatch.zipWithIndex.map{ case ( s, i ) =>
        val (sentCounts, sentProposalScore) = particles( l ).sampleTreeCounts( s )

        assert( particles(l).stringProb > Double.NegativeInfinity )

        val trueScore = particles(l).trueLogProb( sentCounts )
        val samplingScore =
          if( logSpace )
            sentProposalScore - particles(l).stringProb
          else
            sentProposalScore - log( particles(l).stringProb )

        particleWeights(l) =
          particleWeights(l) + ( trueScore - samplingScore )

          // sentenceNum is zero-based
        val reservoirIndex = rand.nextInt( sentenceNum + i + 1 )
        if( reservoirIndex < particles(l).reservoirSize ) {
          particles(l).sampleReservoir( reservoirIndex ) =
            SampledCounts( s.string, sentCounts, samplingScore, trueScore )
        }

        (sentCounts, sentProposalScore)
      }.reduce{ (a,b) => a._1.destructivePlus( b._1 ); ( a._1, a._2 + b._2 ) }

      particles( l ).theta.incrementCounts( counts )

    }

    val totalLogWeight = particleWeights.reduce( LogSum( _,_) )
    (0 until numParticles).foreach{ l =>
      particleWeights(l) = math.exp( particleWeights(l) - totalLogWeight )
    }


    // No iterations reached for particle filter
    Double.NaN
  }


}

