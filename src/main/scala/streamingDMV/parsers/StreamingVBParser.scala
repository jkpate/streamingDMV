package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ArcFactoredParameters

import scala.math.log

abstract class StreamingVBParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) extends FoldUnfoldParser[C,P](
  maxLength,
  rootAlpha,
  stopAlpha,
  chooseAlpha,
  randomSeed
) {

  def streamingBayesUpdate(
    miniBatch:List[Utt],
    maxIter:Int = 10,
    convergence:Double = 0.001,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ) = {

    var lastFHat = initialCounts( miniBatch )
      // emptyCounts
        // DMVCounts(
        //   new CPT[RootEvent]( rootAlpha ),
        //   new CPT[StopEvent]( stopAlpha ),
        //   new CPT[ChooseEvent]( chooseAlpha )
        // )

    theta.incrementCounts( lastFHat )

    var lastMiniBatchScores = 1D
    var insideScores = 0D
    var deltaScores = 1D
    var iter = 0
    while(
      ( iter < maxIter || maxIter == 0 ) &&
      ( math.abs( deltaScores ) > convergence || convergence == 0 )
    ) {
      var thisMiniBatchScores = 0D


      val fHat = miniBatch.map{ s =>
        val counts = extractPartialCounts(s.string)
        thisMiniBatchScores += log( stringProb )
        counts
      }.reduce{ (a,b) => a.destructivePlus(b); a }

      // if( iter > 0 )
      // decrement every time now since lastFHat may not be zero
      theta.decrementCounts( lastFHat )
      // println( miniBatch.map{_.string.length}.sum )
      // println( "about to increment by fHat:\n" )
      theta.incrementCounts( fHat )

      deltaScores = ( lastMiniBatchScores - thisMiniBatchScores ) / lastMiniBatchScores
      if( printIterScores )
        println( s"$iter\t$thisMiniBatchScores\t$deltaScores" )


      lastFHat = fHat
      lastMiniBatchScores = thisMiniBatchScores

      iter += 1
    }
    if( printItersReached )
      println( s"iters\t$iter" )
  }

  def particlePerplexity = 1D
  def ess = 1D
  def resampleParticles = 0
  def trueLogProb( counts:C ):Double

}

