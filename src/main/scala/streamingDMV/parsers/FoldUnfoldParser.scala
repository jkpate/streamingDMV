package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.tables.CPT
import streamingDMV.parameters.ArcFactoredParameters
import streamingDMV.math.LogSum
import scala.collection.mutable.{Map=>MMap}

import math.log


abstract class FoldUnfoldParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
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

  def particlePerplexity:Double
  def ess:Double
  def resampleParticles:Tuple2[Int,Double]

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
  def sampleTreeCounts( originalString:Array[Int] ):Tuple2[C,Double]
  def sampleTreeCounts( utt:Utt ):Tuple2[C,Double] = sampleTreeCounts( utt.string )


  def streamingBayesUpdate(
    miniBatch:List[Utt],
    sentenceNum:Int,
    maxIter:Int = 10,
    convergence:Double = 0.001,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ):Unit

  def argSample[K]( seq:Seq[Tuple2[K,Double]], logSpace:Boolean = false ):Tuple2[K,Double] = {

    if( logSpace ) {
      val logTotal = seq.filter{_._2 >= Double.NegativeInfinity}.map{_._2}.reduce(LogSum(_,_))
      val r = math.log( rand.nextDouble() ) + logTotal
      var runningTotal = Double.NegativeInfinity

      // println( seq.mkString( " " ) )

      seq.takeWhile( pair => {
          val result = runningTotal < r
          if( pair._2 > Double.NegativeInfinity ) runningTotal = LogSum( runningTotal, pair._2 )
          result
        }
      ).last
    } else {
      val total = seq.filter{_._2 >= Double.NegativeInfinity}.map{_._2}.sum
      val r = rand.nextDouble()*total
      var runningTotal = 0D

      // println( seq.mkString( " " ) )

      seq.takeWhile( pair => {
          val result = runningTotal < r
          if( pair._2 > Double.NegativeInfinity ) runningTotal += pair._2
          result
        }
      ).last
    }
  }
  def argMax[K]( seq:Iterable[Tuple2[K,Double]] ):Tuple2[K,Double] = {
    var bestIdx = List[K]()
    var bestScore = Double.NegativeInfinity

    seq.foreach{ case ( idx, score ) =>
      if( !( score > Double.NegativeInfinity && score <= 0D ) ) {
        println( idx, score )
      }
      assert( score > Double.NegativeInfinity && score <= 0D )
      if( score > bestScore ) {
        bestScore = score
        bestIdx = idx :: Nil
      } else if( score == bestScore ) {
        bestIdx = idx :: bestIdx
      }
    }

    if( bestIdx.length == 1 ) {
      (bestIdx.head, bestScore)
    } else {
      if( bestIdx.length <= 0 ) {
        println( seq.mkString("\n" ) )
      }
      val which = rand.nextInt( bestIdx.length )
      ( bestIdx(which), bestScore )
    }
  }




      // def streamingBayesUpdate(
      //   miniBatch:List[Utt],
      //   maxIter:Int = 10,
      //   convergence:Double = 0.001,
      //   printIterScores:Boolean = false,
      //   printItersReached:Boolean = false
      // ) = {

      //   var lastFHat = initialCounts( miniBatch )
      //     // emptyCounts
      //       // DMVCounts(
      //       //   new CPT[RootEvent]( rootAlpha ),
      //       //   new CPT[StopEvent]( stopAlpha ),
      //       //   new CPT[ChooseEvent]( chooseAlpha )
      //       // )

      //   theta.incrementCounts( lastFHat )

      //   var lastMiniBatchScores = 1D
      //   var insideScores = 0D
      //   var deltaScores = 1D
      //   var iter = 0
      //   while(
      //     ( iter < maxIter || maxIter == 0 ) &&
      //     ( math.abs( deltaScores ) > convergence || convergence == 0 )
      //   ) {
      //     var thisMiniBatchScores = 0D


      //     val fHat = miniBatch.map{ s =>
      //       val counts = extractPartialCounts(s.string)
      //       thisMiniBatchScores += log( stringProb )
      //       counts
      //     }.reduce{ (a,b) => a.destructivePlus(b); a }

      //     // if( iter > 0 )
      //     // decrement every time now since lastFHat may not be zero
      //     theta.decrementCounts( lastFHat )
      //     // println( miniBatch.map{_.string.length}.sum )
      //     theta.incrementCounts( fHat )

      //     deltaScores = ( lastMiniBatchScores - thisMiniBatchScores ) / lastMiniBatchScores
      //     if( printIterScores )
      //       println( s"$iter\t$thisMiniBatchScores\t$deltaScores" )


      //     lastFHat = fHat
      //     lastMiniBatchScores = thisMiniBatchScores

      //     iter += 1
      //   }
      //   if( printItersReached )
      //     println( s"iters\t$iter" )
      // }

  // def sampleTreeCounts( utt:Utt ):Tuple2[DMVCounts,Double]

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

