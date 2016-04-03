package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.{ArcFactoredParameters,SteppingParameters,NumericalOptimizing}


// abstract class StochasticVBParser[P<:ArcFactoredParameters[DMVCounts] with
// SteppingParameters](
//   parserSpec:ParserSpec
// ) extends FoldUnfoldParser[DMVCounts,P] (
//   parserSpec
// ) {

trait StochasticVBParser[P<:ArcFactoredParameters[DMVCounts] with NumericalOptimizing] extends StreamingVBParser[DMVCounts,P] {
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
  ) = {
    var evalEvery = evalRate

    val fHat = emptyCounts
    var i = 0
    while( i < miniBatch.size ) {
      val s = miniBatch( i )
      val counts = extractPartialCounts( s )
      if( !( stringProb > myZero && stringProb <= myOne ) ) {
        println( stringProb )
        println( s.id + " " + s.string.mkString( " " )  )
        assert( stringProb > myZero && stringProb <= myOne )
      }
      fHat.destructivePlus( counts )
      i+=1
    }

    theta.step(
      fHat,
      miniBatch.size,
      miniBatch.map{_.string.length-1}.sum,
      2*miniBatch.map{_.string.length}.sum
    )

    0

  }

}


