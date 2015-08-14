package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.tables.CPT
import streamingDMV.parameters.ArcFactoredParameters
import scala.collection.mutable.{Map=>MMap}

import math.log


abstract class FoldUnfoldParser[C<:DependencyCounts,+P<:ArcFactoredParameters[C]](
  maxLength:Int,
  rootAlpha:Double = 1D,
  stopAlpha:Double = 1D,
  chooseAlpha:Double = 1D,
  randomSeed:Int = 15
) {
  val rand = new util.Random( randomSeed )

  val theta:P

  var stringProb = 0D

  var intString:Array[Int] = Array()

  def viterbiParse( utt:Utt ):Parse

  def viterbiDepParse( utt:Utt ):Parse

  // Initialization stuff
  def zerosInit( corpus:List[Utt] ) {
    theta.zerosInit( corpus )
  }

  def randomInit( corpus:List[Utt], seed:Int, scale:Int ) {
    theta.zerosInit( corpus )
    theta.randomizeCounts( seed, scale )
  }

  def logProb( string:Array[Int] ):Double

  def emptyCounts:C
  def initialCounts( utts:List[Utt] ):C

  def extractPartialCounts( string:Array[Int] ):C

  def miniBatchVB(
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


  // // debugging stuff
  // def chartToString(
  //   label:String,
  //   chartToPrint:Array[Array[MMap[Decoration,Double]]],
  //   logSpace:Boolean = true
  // ) = {
  //   s"${label} Chart:\n\n" +
  //   (0 to (intString.length)).flatMap{ i =>
  //     ( (i+1) to intString.length ).map{ j =>
  //       if( chartToPrint(i)(j).size > 0 ) {
  //           (i,j) + chartToPrint(i)(j).map{ case (k,v) =>
  //             s"${k}: ${v}"
  //           }.mkString("\n\t", "\n\t", "\n")
  //       } else {
  //         ""
  //       }
  //     }.mkString("\n","","\n")
  //   }.mkString( "", "", "" )
  // }

  // def seeInsideHeads( logSpace:Boolean = true ):Unit


}

