package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ArcFactoredParameters

import math.{exp,log}

abstract class StreamingVBParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // randomSeed:Int = 15,
  // val reservoirSize:Int = 0
  parserSpec:ParserSpec
) extends FoldUnfoldParser[C,P](
  // maxLength,
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha,
  // randomSeed
  parserSpec
) {

  val reservoirSize = parserSpec.reservoirSize

  var wordsSeen = 0
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

    // println( s"miniBatchSize: ${miniBatch.size}" )

    var evalEvery = evalRate

    // println( s"\n\n\n-----\nevalEvery: $evalEvery\n\n-----\n\n\n" )

    // println( miniBatch.map{ _.string.mkString(" ") }.mkString("\n") )
    var lastFHat = initialCounts( miniBatch )
      // println( s"initial lastFHat" )
      // lastFHat.printTotalCountsByType
    if( scaleInitMiniBatchCounts ) {
      val currentCounts = theta.toCounts
      if( wordsSeen > 0 )
        // lastFHat.scaleToMatch( currentCounts )
        lastFHat.scaleToMatch( sentenceNum, wordsSeen )
        lastFHat.multiplyBy( 10D )
    }

      // emptyCounts
        // DMVCounts(
        //   new CPT[RootEvent]( rootAlpha ),
        //   new CPT[StopEvent]( stopAlpha ),
        //   new CPT[ChooseEvent]( chooseAlpha )
        // )

    // lastFHat.printTotalCountsByType

        // println( s"incrementing theta by lastFHat" )
    theta.incrementCounts( lastFHat, updateEvents = false )
    // theta.incrementCounts( lastFHat )

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
      // val fHat = miniBatch.map{ s =>
      val fHat = emptyCounts
      // println( s"initial fHat:" )
      // fHat.printTotalCountsByType

      while( i < miniBatch.size ) {
        val s = miniBatch( i )
        println( s.string.mkString("[ ", ", ", " ]") )
        // println( s"    $i" )
        val counts = extractPartialCounts( s )
        // val counts = extractPartialCounts( s.string, fHat )
        if( !( stringProb > myZero && stringProb <= myOne ) ) {
          println( stringProb )
          println( s.id + " " + s.string.mkString( " " )  )
          assert( stringProb > myZero && stringProb <= myOne )
        }
        thisMiniBatchScores += { if( logSpace ) stringProb else log( stringProb ) }
        fHat.destructivePlus( counts )
        i+=1

        val sentencesProcessed = sentenceNum + i
            // Not sure what to do if we're doing more than one max iter?
        if( maxIter == 1 & sentencesProcessed < miniBatch.size & sentencesProcessed%evalEvery == 0 ) {
          // println( s"\n\n  ---  STREAMING BAYES UPDATE EVAL $i $evalEvery---\n\n" )
          theta.incrementCounts( fHat )
          if( constituencyEval )
            printViterbiParses(
              testSet,
              s"it${sentencesProcessed}",
              evalMaxLength
            )
          else
            printViterbiDepParses(
              testSet,
              s"it${sentencesProcessed}",
              evalMaxLength
            )
          theta.decrementCounts( fHat )
        }

        if(
          maxIter == 1 &&
          logEvalRate &&
          scala.math.log10( sentencesProcessed )%1 == 0 &&
          sentencesProcessed > evalEvery
        ) {
          evalEvery *= 10
          println( s"after processing $sentencesProcessed of largeMiniBatch we eval every $evalEvery" )
        }
      }



      // println( s"decrementing by lastFHat" )
      theta.decrementCounts( lastFHat )
      // println( s"incrementing by fHat" )
      // theta.incrementCounts( fHat, updateEvents = false )
      theta.incrementCounts( fHat, updateEvents = true )
      println( "fHat counts" )
      fHat.printTotalCountsByType
          // // fHat.printStopEvents
          // println( "\n\n---\n\nAccumulated counts:" )
          // theta.toCounts.printTotalCountsByType

      deltaScores = ( lastMiniBatchScores - thisMiniBatchScores ) / lastMiniBatchScores
      if( printIterScores )
        println( s"$iter\t$thisMiniBatchScores\t$deltaScores" )


      lastFHat = fHat
      lastMiniBatchScores = thisMiniBatchScores

      iter += 1
    }

    wordsSeen += miniBatch.map{ _.string.size }.sum

    if( printItersReached )
      println( s"iters\t$iter" )

    iter
  }



  var caching = false

  var sampleReservoir = Array.fill( reservoirSize )(
    SampledCounts( Utt( "", Array() ), emptyCounts, Double.NegativeInfinity, Double.NegativeInfinity )
  )

  def particlePerplexity = 1D
  def ess = 1D
  def resampleParticles = (0,0)
  def trueLogProb( counts:C ):Double

}

