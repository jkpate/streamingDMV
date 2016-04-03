package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.{ArcFactoredParameters,WeightsParameters}

import math.{abs,log,exp}


trait AdaDeltaParser[P<:WeightsParameters] extends StreamingVBParser[DMVCounts,P] {

  override def streamingUpdate(
    miniBatch:List[Utt],
    sentenceNum:Int,
    testSet:List[Utt],
    maxIter:Int = 10,
    convergence:Double = 0.001,
    evalMaxLength:Int = 0,
    evalRate:Int = 10,
    logEvalRate:Boolean = true,
    constituencyEval:Boolean = true,
    morphsEval:Boolean = true,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ) = adaDeltaUpdate(
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

  def adaDeltaUpdate(
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
    // regularization parameter from Berg-Kirkpatrick
    val kappa = 0

    // theta.clearMemory
    var lastMiniBatchScores = 1D
    var insideScores = 0D
    var deltaScores = 1D
    var iter = 0
    while(
      ( iter < maxIter || maxIter == 0 ) &&
      ( abs( deltaScores ) > convergence || convergence == 0 )
    ) {
      var thisMiniBatchScores = 0D

      var i = 0
      val fHat = emptyCounts
      while( i < miniBatch.size ) {
        val s = miniBatch( i )
        // println( s.string.mkString("[ ", ", ", " ]") )
        // println( s"    $i" )
        val counts = extractPartialCounts( s )

        if( !( stringProb > myZero && stringProb <= myOne ) ) {
          println( stringProb )
          println( s.id + " " + s.string.mkString( " " )  )
          assert( stringProb > myZero && stringProb <= myOne )
        }
        thisMiniBatchScores += {
          if( logSpace ) {
            stringProb 
          } else {
            log(
              stringProb
            )//  - theta.l2NormDerivative( kappa = kappa )
          }
        }

        fHat.destructivePlus( counts )
        i+=1

      }
      thisMiniBatchScores -= theta.l2NormDerivative( kappa = kappa )

      ( 0 until 10 ).foreach{ _=>
        // theta.updateWeights( fHat, kappa )
        theta.sgd( fHat, kappa )
        // println( "l2Norm: " + theta.l2Norm( 1 ) )
        val logProbs = miniBatch.map{ s => logProb( s ) }.sum
        println( s"${theta.l2Norm(1)}\t$logProbs" )
      }
      theta.clearMemory

      deltaScores = ( lastMiniBatchScores - thisMiniBatchScores ) / lastMiniBatchScores
      if( printIterScores )
        println( s"$iter\t$thisMiniBatchScores\t$deltaScores" )


      lastMiniBatchScores = thisMiniBatchScores

      iter += 1
    }

    iter
  }

}

