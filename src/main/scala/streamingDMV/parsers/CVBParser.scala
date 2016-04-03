package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.ArcFactoredParameters

import math.{exp,log}


trait CVBParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]] extends FoldUnfoldParser[C,P] {


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
    miniBatch.foreach{ s =>
      theta.decrementCounts( sentenceCounts( s.id ) )
      val newCounts = extractPartialCounts( s )
      theta.incrementCounts( newCounts, updateEvents = true )

      sentenceCounts += s.id -> newCounts
    }
    0
  }


}


