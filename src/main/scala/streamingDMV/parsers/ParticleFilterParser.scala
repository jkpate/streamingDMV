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
    SampledCounts( Utt( "", Array() ), emptyCounts, myZero, myZero )
  )

  val particles = Array.tabulate( numParticles )( l => createParticle(emptyCounts,emptyReservoir,l) )
  val particleWeights = Array.fill( numParticles )(
    if( logSpace )
      -1 * log( numParticles )
    else
      1D/numParticles
  )

  def ess = {
    // println( particleWeights.mkString( "\n" ) )
    if( ! particleWeights.forall( _ > myZero ) )
      -1D
    else
      if( logSpace )
        -1 * (
          particleWeights.map{ w => w * 2D }.reduce( LogSum(_,_) )
        )
      else
        1D / (
          particleWeights.map{ w => math.pow( w, 2D ) }.sum
        )
  }

  def particlePerplexity = {
    if( logSpace )
      math.exp(
        -1 *
        (0 until numParticles).map{ l =>
          if( particleWeights(l) >= Double.NegativeInfinity )
            math.exp( particleWeights(l) ) * particleWeights(l)
          else 0D
        }.sum
      )
    else
      math.exp(
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
    println( s"corpus length: ${corpus.length}" )
    println( s"first utterance: ${corpus.head}" )
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
      (0 until particles(l).reservoirSize)/*.par*/.foreach{ reservoirIndex =>
        val prev = particles(l).sampleReservoir( reservoirIndex )

        if( prev.utt.string.length > 0 ) {
          sentenceCount += 1
          particles(l).theta.decrementCounts( prev.counts, integerDec = true )
          val (newCounts, newProposalScore) = particles( l ).sampleTreeCounts( prev.utt )

          val newTrueScore = particles(l).trueLogProb( newCounts )
          // println( newTrueScore + "  <=>  " + particles(l).fastTrueLogProb( newCounts ) )
          val newSamplingScore =
            if( logSpace )
              newProposalScore - particles(l).stringProb
            else
              math.log( newProposalScore ) - math.log( particles(l).stringProb )

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
              prev.utt, newCounts, newSamplingScore, newTrueScore
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
      val newParticles = Array.tabulate( numParticles )( l => {
          val particleProbs:Seq[Tuple2[Int,Double]] =
            if( particleWeights.forall( w => !( w > myZero ) ) ) {
              Seq.tabulate(numParticles){ pIdx:Int =>
                if( logSpace ) {
                  ( pIdx, -1D * log( numParticles ) )
                } else {
                  ( pIdx, 1D/numParticles )
                }
              }
            } else {
              particleWeights.zipWithIndex.map{ p => (p._2,p._1) }.toSeq
            }

          val ( idx, _ ) = argSample( particleProbs, logSpace )
          uniqueAncestors += idx

          if( l == idx ) {  // Probably doesn't buy us much...
            particles(l)
          } else {
            createParticle( particles(idx).theta.toCounts, particles(idx).sampleReservoir, rand.nextInt )
          }
        }
      )

      // val totalWeight = myPlus( newParticleWeights.filter{ _ > Double.NegativeInfinity }.toSeq:_* )
      (0 until numParticles).foreach{ l =>
        particles(l) = newParticles(l)
        particleWeights(l) =
          if( logSpace )
            -1 * log( numParticles.toDouble )
          else
            1D/numParticles.toDouble
        if( ! ( particleWeights(l) > myZero ) ) {
          particleWeights(l) = myZero
        }
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

  def streamingUpdate(
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
  ) = streamingVBUpdate(
    miniBatch = miniBatch,
    sentenceNum = sentenceNum,
    testSet = testSet,
    maxIter = maxIter,
    convergence = convergence,
    evalMaxLength = evalMaxLength,
    evalRate = evalRate,
    logEvalRate = logEvalRate,
    constituencyEval = constituencyEval,
    printIterScores = printIterScores,
    printItersReached = printItersReached
  )


  // TODO increment eval during initial minibatch
  def streamingVBUpdate(
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


    // (0 until numParticles ).foreach{ l => particleWeights(l) = math.log( particleWeights(l) ) }

    val logWeights = particleWeights.map{ log( _ ) }

    ( 0 until numParticles )/*.par*/.map{ l =>
      particles(l).theta.fullyNormalized = true
      val (counts, proposalScore) = miniBatch.zipWithIndex.map{ case ( s, i ) =>
        val (sentCounts, sentProposalScore) = particles( l ).sampleTreeCounts( s )

        assert( particles(l).stringProb > Double.NegativeInfinity )

        val trueScore = particles(l).trueLogProb( sentCounts )
        val samplingScore =
          if( logSpace )
            sentProposalScore - particles(l).stringProb
          else
            log( sentProposalScore / particles(l).stringProb )

        // println( s"  >$l: ${particleWeights(l)}" )
        // println( s"    } $sentProposalScore" )
        // println( s"    } ${particles(l).stringProb}" )
        // println( s"    } $trueScore" )
        // println( s"    } $samplingScore" )

        // particleWeights(l) =
        //   if( logSpace )
        //     particleWeights(l) + trueScore - samplingScore
        //   else
        //     particleWeights(l) * math.exp( trueScore - samplingScore )
        logWeights(l) =
          if( logSpace )
            logWeights(l) + trueScore - samplingScore
          else
            logWeights(l) + math.log( trueScore / samplingScore )

        // println( s"  $l: ${particleWeights(l)}" )

          // sentenceNum is zero-based
        val reservoirIndex = rand.nextInt( sentenceNum + i + 1 )
        if( reservoirIndex < particles(l).reservoirSize ) {
          particles(l).sampleReservoir( reservoirIndex ) =
            SampledCounts( s, sentCounts, samplingScore, trueScore )
        }

        (sentCounts, sentProposalScore)
      }.reduce{ (a,b) => a._1.destructivePlus( b._1 ); ( a._1, a._2 + b._2 ) }

      particles( l ).theta.incrementCounts( counts )

    }


    // val totalLogWeight = particleWeights.reduce( LogSum( _,_) )
    // (0 until numParticles).foreach{ l =>
    //   particleWeights(l) = math.exp( particleWeights(l) - totalLogWeight )
    // }

    // println( logWeights.mkString( "\n" ) )
    // println( miniBatch.head.string.mkString(" ") )
    // val totalWeight = myPlus( particleWeights.filter{_>Double.NegativeInfinity}.toSeq:_* )
    if( logWeights.forall( x => !(  x >  Double.NegativeInfinity ) ) ) {
      (0 until numParticles).foreach{ l =>
        if( logSpace )
          particleWeights(l) = -1D * log( numParticles )
        else
          particleWeights(l) = 1D / numParticles
      }
    } else {
      val totalWeight = logPlus( logWeights.filter{_>Double.NegativeInfinity}.toSeq:_* )
      (0 until numParticles).foreach{ l =>
        // if( ! ( particleWeights(l)  > myZero ) ) {
        //   println( (l, particleWeights(l), totalWeight ) )
        // }
        // assert( particleWeights(l)  > myZero )

        if( logSpace )
          particleWeights(l) = logWeights(l) - totalWeight
        else
          particleWeights(l) = exp( logWeights(l) - totalWeight )

        if( ! ( particleWeights(l) > myZero ) ) {
          particleWeights(l) = myZero
        }

      }
    }

    // println( particleWeights.mkString( "\n[\n\t","\n\t","\n]\n" ) )


    // No iterations reached for particle filter
    Double.NaN
  }


}

