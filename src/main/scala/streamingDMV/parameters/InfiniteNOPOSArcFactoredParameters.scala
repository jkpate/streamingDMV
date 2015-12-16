package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.{DPCPT,LogDPCPT}

import collection.mutable.{Set=>MSet}
import math.{exp,log}

trait InfiniteParameters extends NOPOSArcFactoredParameters {

  val alphabetSize:Int

  val logAlphabetSize = log( alphabetSize.toDouble )

  val letterProb = 1D/alphabetSize
  val logLetterProb = -1* log( alphabetSize )


  val stopProb = 0.1
  val notStopProb = 1 - stopProb

  val logStopProb = log( stopProb )
  val logNotStopProb = log( notStopProb )

  def monkeyModel( w:String ) = {
    val l = w.length
    // println( s"length: $l" )
    math.pow( notStopProb , l-1 ) * math.pow( letterProb, l ) * stopProb
  }

  def logMonkeyModel( w:String ) = {
    val l = w.length
    // println( s"length: $l" )
    // math.pow( notStopProb , l-1 ) * math.pow( letterProb, l ) * stopProb
    val score = ( logStopProb * (l-1)) +
      ( logLetterProb * l ) + logStopProb

    // println( "baseDistributionScore: " + score )
    score
  }

  val baseDistribution =
    if (alphabetSize < 200 )
      if( logSpace )
        logMonkeyModel _
      else
        monkeyModel _
    else
      if( logSpace )
        (w:String) => { -1*logAlphabetSize }
      else
        (w:String) => { 1D / alphabetSize }

  override val p_root =
    if( logSpace )
      new LogDPCPT[RootEvent](
        rootAlpha,
        // logMonkeyModel
        baseDistribution
      )
    else
      new DPCPT[RootEvent](
        rootAlpha,
        // monkeyModel
        baseDistribution
      )

  override val p_choose =
    if( logSpace )
      new LogDPCPT[ChooseEvent](
        chooseAlpha,
        // logMonkeyModel
        baseDistribution
      )
    else
      new DPCPT[ChooseEvent](
        chooseAlpha,
        // monkeyModel
        baseDistribution
      )

}


