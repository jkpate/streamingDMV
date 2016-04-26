package streamingDMV.parsers

import streamingDMV.io.{printDependencyParse,printMorphs}
import streamingDMV.labels._
import streamingDMV.tables.CPT
import streamingDMV.parameters.ArcFactoredParameters
import streamingDMV.math.LogSum
import scala.collection.mutable.{Map=>MMap}


import math.log

import scala.reflect.ClassTag


abstract class FoldUnfoldParser[C<:DependencyCounts,P<:ArcFactoredParameters[C]](
  // maxLength:Int,
  // rootAlpha:Double = 1D,
  // stopAlpha:Double = 1D,
  // chooseAlpha:Double = 1D,
  // randomSeed:Int = 15
  parserSpec:ParserSpec
) {

  val maxLength = parserSpec.maxLength
  val randomSeed = parserSpec.randomSeed
  val rootAlpha = parserSpec.rootAlpha
  val stopAlpha = parserSpec.stopAlpha
  val chooseAlpha = parserSpec.chooseAlpha
  val approximate = parserSpec.approximate
  val squarelyNormalized = parserSpec.squarelyNormalized
  val scaleInitMiniBatchCounts = parserSpec.scaleInitMiniBatchCounts
  val logSpace = parserSpec.logSpace

  val myZero = if( parserSpec.logSpace ) Double.NegativeInfinity else 0D
  val myOne = if( parserSpec.logSpace ) 0D else 1D

  def logPlus( nums:Double* ) = { nums.reduce( LogSum(_,_) ) }
  def realPlus( nums:Double* ) = { nums.sum }
  def logTimes( nums:Double* ) = { nums.sum }
  def realTimes( nums:Double* ) = { nums.reduce(_*_) }

  def logDiv( a:Double, b:Double ) = { a - b }
  def realDiv( a:Double, b:Double ) = { a / b }
  def myDiv = if( logSpace ) logDiv _ else realDiv _

  // in scala 2.10+ partially applied varargs functions take Seq[_]...
  val myPlusSeq = if( logSpace ) logPlus _ else realPlus _
  def myPlus( nums:Double* ) = myPlusSeq( nums )

  val myTimesSeq = if( logSpace ) logTimes _ else realTimes _
  def myTimes( nums:Double* ) = myTimesSeq( nums )



  val rand = new util.Random( randomSeed )

  val theta:P

  var stringProb = 0D

  var intString:Array[Int] = Array()
  var lexString:Array[String] = Array()
  var annotString:Array[Double] = Array()

  def particlePerplexity:Double
  def ess:Double
  def resampleParticles:Tuple2[Int,Double]

  def viterbiParse( utt:Utt ):Parse

  def viterbiDepParse( utt:Utt ):Parse
  def viterbiDepParseWithMorphs( utt:Utt ):Parse

  def doubleString[A:ClassTag]( string:Array[A] ) = {
    string.toSeq.flatMap{ w => List(w,w) }.toArray
  }

  def printViterbiParses(
    testSet:List[Utt],
    prefix:String,
    evalMaxLength:Int//,
    // counts:C = emptyCounts
  ) = {
    var heldOutLogProb = 0D

    prepareForParses()

    // theta.incrementCounts( counts )

    val parseStartTime = System.currentTimeMillis
    testSet.foreach{ utt =>
      if( evalMaxLength == 0 || utt.string.length <= evalMaxLength ) {
        val Parse( id, conParse, depParse, _ ) = viterbiParse( utt )
        println( s"${prefix}:constituency:${id} ${conParse}" )
        println( s"${prefix}:dependency:${id} ${printDependencyParse(depParse)}" )
        // heldOutLogProb += logProb( s.string )
        heldOutLogProb += logProb( utt )
      }
    }

    val thisTestTime = System.currentTimeMillis - parseStartTime

    println( s"${prefix}:logProb:${heldOutLogProb}" )
    println( s"${prefix}:testTimePerSentence:${ thisTestTime.toDouble / testSet.size}ms/sentence" )

    // theta.decrementCounts( counts )
    // ( heldOutLogProb, thisTestTime )
    thisTestTime
  }

  def prepareForParses() = {}

  def printViterbiDepParses(
    testSet:List[Utt],
    prefix:String,
    evalMaxLength:Int// ,
    // counts:C = emptyCounts
  ) = {
    var heldOutLogProb = 0D

    // theta.incrementCounts( counts )
    prepareForParses()

    val parseStartTime = System.currentTimeMillis
    testSet.foreach{ utt =>
      if( evalMaxLength == 0 || utt.string.length <= evalMaxLength ) {
        val Parse( id, conParse, depParse, _ ) = viterbiParse( utt )
        println( s"${prefix}:dependency:${id} ${printDependencyParse(depParse)}" )
        // heldOutLogProb += logProb( s.string )
        heldOutLogProb += logProb( utt )
      }
    }

    val thisTestTime = System.currentTimeMillis - parseStartTime

    println( s"${prefix}:logProb:${heldOutLogProb}" )
    println( s"${prefix}:testTimePerSentence:${ thisTestTime.toDouble / testSet.size}ms/sentence" )

    // theta.decrementCounts( counts )
    // ( heldOutLogProb, thisTestTime )
    thisTestTime
  }

  def printViterbiDepParsesWithMorphs(
    testSet:List[Utt],
    prefix:String,
    evalMaxLength:Int// ,
    // counts:C = emptyCounts
  ) = {
    var heldOutLogProb = 0D

    // theta.incrementCounts( counts )
    prepareForParses()

    val parseStartTime = System.currentTimeMillis
    testSet.foreach{ utt =>
      if( evalMaxLength == 0 || utt.string.length <= evalMaxLength ) {
        val Parse( id, conParse, depParse, morphs ) = viterbiDepParseWithMorphs( utt )
        println( s"${prefix}:dependency:${id} ${printDependencyParse(depParse)}" )
        println( s"${prefix}:morphology:${id} ${printMorphs(morphs)}" )
        // heldOutLogProb += logProb( s.string )
        heldOutLogProb += logProb( utt )
      }
    }

    val thisTestTime = System.currentTimeMillis - parseStartTime

    println( s"${prefix}:logProb:${heldOutLogProb}" )
    println( s"${prefix}:testTimePerSentence:${ thisTestTime.toDouble / testSet.size}ms/sentence" )

    // theta.decrementCounts( counts )
    // ( heldOutLogProb, thisTestTime )
    thisTestTime
  }



  // Initialization stuff
  def zerosInit( corpus:List[Utt] ) {
    theta.zerosInit( corpus )
  }

  def randomInit( corpus:List[Utt], seed:Int, scale:Int ) {
    theta.zerosInit( corpus )
    theta.randomizeCounts( seed, scale )
  }

  // def logProb( string:Array[Int] ):Double
  def logProb( utt:Utt ):Double

  def emptyCounts:C

  var constantInitialCounts = emptyCounts
  def setConstantHarmonicCounts( utts:List[Utt] ) {
    constantInitialCounts = theta.harmonicCounts( utts )
  }

  var sentenceCounts = Map[String,C]().withDefaultValue( emptyCounts )
  def sentenceSpecificHarmonicInit( utts:List[Utt] ) {
    println( "sentenceSpecificHarmonicInit" )
    val windowSize = math.ceil( utts.size.toDouble/20 )
    var i = 1
    utts.foreach{ s =>
      val hCounts = theta.harmonicCounts( s::Nil )
      theta.incrementCounts( hCounts, updateEvents = true )
      sentenceCounts += s.id -> { hCounts }
      if( i %windowSize == 0 ) {
        println( s"$i/${utts.size} processed" )
      }
      i += 1
    }
  }
  def sentenceSpecificRandomInit( utts:List[Utt] ) {
    println( "sentenceSpecificRandomInit" )
    val windowSize = math.ceil( utts.size.toDouble/20 )
    var i = 1
    utts.foreach{ s =>
      val rCounts = theta.randomCounts( s::Nil )
      theta.incrementCounts( rCounts, updateEvents = true )
      sentenceCounts += s.id -> { rCounts }
      if( i %windowSize == 0 ) {
        println( s"$i/${utts.size} processed" )
      }
      i += 1
    }
  }

  def harmonicInit( utts:List[Utt] ) {
    theta.harmonicCounts( utts )
  }

  def initialCounts( utts:List[Utt] ):C

  def initializeParticles = {}

  // def extractPartialCounts( string:Array[Int] ):C
  def extractPartialCounts( utt:Utt ):C
  def sampleTreeCounts( utt:Utt ):Tuple2[C,Double]
  // def sampleTreeCounts( utt:Utt ):Tuple2[C,Double] = sampleTreeCounts( utt.string )


  // def streamingBayesUpdate(
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
    morphsEval:Boolean = true,
    printIterScores:Boolean = false,
    printItersReached:Boolean = false
  ):Double

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

    // println( seq.map(_._2).mkString("{\n\t","\n\t","\n}\n") )

    seq.foreach{ case ( idx, score ) =>
      if( !( score > myZero && score <= myOne + 0.000001 ) ) {
        println( idx, score )
      }
      assert( score > myZero && score <= myOne + 0.000001 )
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

  // debugging stuff
  def chartToString(
    label:String,
    chartToPrint:Array[Array[MMap[Decoration,Double]]],
    logSpace:Boolean = true
  ) = {
    s"${label} Chart:\n\n" +
    (0 to (intString.length)).flatMap{ i =>
      ( (i+1) to intString.length ).map{ j =>
        if( chartToPrint(i)(j).size > 0 ) {
            (i,j) + chartToPrint(i)(j).map{ case (k,v) =>
              s"${k}: ${v}"
            }.mkString("\n\t", "\n\t", "\n")
        } else {
          ""
        }
      }.mkString("\n","","\n")
    }.mkString( "", "", "" )
  }



}

