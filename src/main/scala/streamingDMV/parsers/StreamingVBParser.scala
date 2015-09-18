package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ArcFactoredParameters

import math.{exp,log}

abstract class StreamingVBParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15,
  val reservoirSize:Int = 0
) extends FoldUnfoldParser[C,P](
  maxLength,
  rootAlpha,
  stopAlpha,
  chooseAlpha,
  randomSeed
) {

  def streamingBayesUpdate(
    miniBatch:List[Utt],
    sentenceNum:Int,
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

    theta.incrementCounts( lastFHat, updateEvents = false )

    var lastMiniBatchScores = 1D
    var insideScores = 0D
    var deltaScores = 1D
    var iter = 0
    while(
      ( iter < maxIter || maxIter == 0 ) &&
      ( math.abs( deltaScores ) > convergence || convergence == 0 )
    ) {
      // println( s"  $iter" )
      var thisMiniBatchScores = 0D


      var i = 0
      val fHat = miniBatch.map{ s =>
        // println( s"    $i" )
        // println( s.id + " " + s.string.mkString(" ") )
        val counts = extractPartialCounts(s.string)
        if( !( stringProb > Double.NegativeInfinity && stringProb <= 0D ) ) {
          println( stringProb )
          println( s.id + " " + s.string.mkString( " " )  )
          assert( stringProb > Double.NegativeInfinity && stringProb <= 0D )
        }
        thisMiniBatchScores += stringProb
        i+=1
        counts
      }.reduce{ (a,b) => a.destructivePlus(b); a }

      // assert( fHat.logSpace )
      // assert( lastFHat.logSpace )
      // assert( theta.logSpace )

      // fHat.printTotalCountsByType
      // println( s"thisFHat: ${fHat.totalCounts} total events seen" )
      // println( s"lastFHat: ${lastFHat.totalCounts} total events seen" )


          // println( s"before incrementing or decrementing" )
          // theta.printTotalCountsByType

          // println( s"description of lastFHat:" )
          // lastFHat.printTotalCountsByType

      theta.decrementCounts( lastFHat )

          // println( s"before incrementing" )
          // theta.printTotalCountsByType

          // println( s"description of fHat:" )
          // fHat.printTotalCountsByType

                                    // Grammar should already be defined -- avoid useless hashing
      theta.incrementCounts( fHat, updateEvents = false )

          // println( s"after incrementing" )
          // theta.printTotalCountsByType

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


  var caching = false

  var sampleReservoir = Array.fill( reservoirSize )(
    SampledCounts( Array(), emptyCounts, Double.NegativeInfinity, Double.NegativeInfinity )
  )

  def particlePerplexity = 1D
  def ess = 1D
  def resampleParticles = (0,0)
  def trueLogProb( counts:C ):Double

}

