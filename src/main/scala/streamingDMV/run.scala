package streamingDMV

import joptsimple.OptionParser
import joptsimple.OptionSet


import streamingDMV.io._
import streamingDMV.parsers._
import streamingDMV.labels._
import streamingDMV.parameters._
import streamingDMV.labels.{Parse,Utt,DependencyCounts,DMVCounts,BackoffChooseDMVCounts}

import scala.math.{pow,log10,floor}


object run {
  var miniBatchDur = 0D
  var totalDur = 0D
  var totalTestDur = 0D
  var testEvents = 0D
  var resamplingDur = 0D
  var ancestors = 0D
  var sentencesProcessed = 0
  var resamplingEventCounts = 0
  var totalIters = 0D
  var totalMiniBatches = 0D
  sys.addShutdownHook {
    println( s"${sentencesProcessed} sentences processed" )
    println( s"${totalDur}ms spent training")
    println( s"${totalTestDur}ms spent testing")
    println( s"${resamplingDur}ms spent resampling")
    println( s"${totalDur/sentencesProcessed}ms training per training sentence")
    println( s"${totalTestDur/testEvents}ms testing per testing sentence per test event")
    println( s"resampled ${resamplingEventCounts} times")
    println( s"resampled ${ancestors/resamplingEventCounts} distinct ancestors per resampling event")
    println( s"${resamplingDur/sentencesProcessed}ms resampling per training sentence")
    println( s"${resamplingDur/resamplingEventCounts}ms resampling per resampling event")
    println( s"${totalIters} total iters")
    println( s"${totalMiniBatches} total totalMiniBatches")
    println( s"${totalIters/totalMiniBatches} iters per miniBatch")
  }
  def main( args:Array[String] ) {
    val optsParser = new OptionParser()

    optsParser.accepts( "trainStrings" ).withRequiredArg
    optsParser.accepts( "testStrings" ).withRequiredArg
    optsParser.accepts( "parserType" ).withRequiredArg
    optsParser.accepts( "fullyNormalizing" )
    optsParser.accepts( "incConvergence" ).withRequiredArg
    optsParser.accepts( "incIters" ).withRequiredArg
    optsParser.accepts( "logEvalRate" )
    optsParser.accepts( "evalEvery" ).withRequiredArg
    optsParser.accepts( "evalMaxLength" ).withRequiredArg
    optsParser.accepts( "alpha" ).withRequiredArg
    optsParser.accepts( "rootAlpha" ).withRequiredArg
    optsParser.accepts( "stopAlpha" ).withRequiredArg
    optsParser.accepts( "chooseAlpha" ).withRequiredArg
    optsParser.accepts( "backoffAlpha" ).withRequiredArg
    optsParser.accepts( "notBackoffAlpha" ).withRequiredArg
    optsParser.accepts( "squarelyNormalized" )
    optsParser.accepts( "uposCount" ).withRequiredArg
    optsParser.accepts( "randomSeed" ).withRequiredArg
    optsParser.accepts( "harmonicMiniBatchInit" )
    optsParser.accepts( "harmonicCorpusInit" )
    optsParser.accepts( "scaleInitMiniBatchCounts" )
    optsParser.accepts( "miniBatchSize" ).withRequiredArg
    optsParser.accepts( "initialMiniBatchSize" ).withRequiredArg
    optsParser.accepts( "largeMiniBatchEvery" ).withRequiredArg
    optsParser.accepts( "convergeInitialMiniBatch" )
    optsParser.accepts( "printItersReached" )
    optsParser.accepts( "printIterScores" )
    optsParser.accepts( "batchVB" )
    optsParser.accepts( "infiniteModels" )
    optsParser.accepts( "baseDistribution" ).withRequiredArg
    optsParser.accepts( "logSpace" )
    optsParser.accepts( "particleFilter" )
    optsParser.accepts( "numParticles" ).withRequiredArg
    optsParser.accepts( "reservoirSize" ).withRequiredArg
    optsParser.accepts( "noResampling" )
    optsParser.accepts( "printResamplingEvents" )
    optsParser.accepts( "constituencyEval" )
    optsParser.accepts( "printInitialGrammar" )
    optsParser.accepts( "printFinalGrammar" )

    val opts = optsParser.parse( args:_* )

    val trainStrings = opts.valueOf( "trainStrings" ).toString
    val testStrings = opts.valueOf( "testStrings" ).toString
    val fullyNormalizing = opts.has( "fullyNormalizing" )
    val parserType =
      if( opts.has( "parserType" ) )
        opts.valueOf( "parserType" )
      else
        "TopDownDMVParser"
    val incConvergence =
      if( opts.has( "incConvergence" ) )
        opts.valueOf( "incConvergence" ).toString.toDouble
      else
        1.0E-5
        //0.00001
    val incIters =
      if( opts.has( "incIters" ) )
        opts.valueOf( "incIters" ).toString.toInt
      else
        10
    val alpha =
      if( opts.has( "alpha" ) )
        opts.valueOf( "alpha" ).toString.toDouble
      else
        1D
    val rootAlpha =
      if( opts.has( "rootAlpha" ) )
        opts.valueOf( "rootAlpha" ).toString.toDouble
      else
        alpha
    val stopAlpha =
      if( opts.has( "stopAlpha" ) )
        opts.valueOf( "stopAlpha" ).toString.toDouble
      else
        alpha
    val chooseAlpha =
      if( opts.has( "chooseAlpha" ) )
        opts.valueOf( "chooseAlpha" ).toString.toDouble
      else
        alpha
    val backoffAlpha =
      if( opts.has( "backoffAlpha" ) )
        opts.valueOf( "backoffAlpha" ).toString.toDouble
      else
        alpha
    val notBackoffAlpha =
      if( opts.has( "notBackoffAlpha" ) )
        opts.valueOf( "notBackoffAlpha" ).toString.toDouble
      else
        alpha
    val squarelyNormalized = opts.has( "squarelyNormalized" )
    val uposCount =
      if( opts.has( "uposCount" ) )
        opts.valueOf( "uposCount" ).toString.toInt
      else
        3
    val randomSeed =
      if( opts.has( "randomSeed" ) )
        opts.valueOf( "randomSeed" ).toString.toInt
      else
        15
    val scaleInitMiniBatchCounts = opts.has( "scaleInitMiniBatchCounts" )
    val harmonicMiniBatchInit = opts.has( "harmonicMiniBatchInit" )
    val harmonicCorpusInit = opts.has( "harmonicCorpusInit" )
    val miniBatchSize =
      if( opts.has( "miniBatchSize" ) )
        opts.valueOf( "miniBatchSize" ).toString.toInt
      else
        1
    val initialMiniBatchSize =
      if( opts.has( "initialMiniBatchSize" ) )
        opts.valueOf( "initialMiniBatchSize" ).toString.toInt
      else
        miniBatchSize
    val largeMiniBatchEvery =
      if( opts.has( "largeMiniBatchEvery" ) )
        opts.valueOf( "largeMiniBatchEvery" ).toString.toInt
      else
        0
    val convergeInitialMiniBatch = opts.has( "convergeInitialMiniBatch" )
    val batchVB = opts.has( "batchVB" )
    val infiniteModels = opts.has( "infiniteModels" )
    val baseDistribution =
      if( opts.has( "baseDistribution" ) )
        opts.valueOf( "baseDistribution" ).toString
      else
        "monkeyModel"
    val logSpace = opts.has( "logSpace" )
    val particleFilter = opts.has( "particleFilter" )
    val numParticles =
      if( opts.has( "numParticles" ) )
        opts.valueOf( "numParticles" ).toString.toInt
      else
        if( opts.has( "particleFilter" ) )
          16
        else
          1
    val reservoirSize =
      if( opts.has( "reservoirSize" ) )
        opts.valueOf( "reservoirSize" ).toString.toInt
      else
        if( opts.has( "particleFilter" ) )
          100
        else
          0
    val noResampling = opts.has( "noResampling" )
    val printResamplingEvents = opts.has( "printResamplingEvents" )
    val logEvalRate = opts.has( "logEvalRate" )
    var evalEvery =
      if(opts.has( "evalEvery" ))
        opts.valueOf( "evalEvery" ).toString.toInt
      else
        initialMiniBatchSize
    var evalMaxLength =
      if(opts.has( "evalMaxLength" ))
        opts.valueOf( "evalMaxLength" ).toString.toInt
      else
        0
    val printItersReached =
      opts.has( "printItersReached" )
    val printIterScores =
      opts.has( "printIterScores" )
    val constituencyEval =
      opts.has( "constituencyEval" )
    val printInitialGrammar =
      opts.has( "printInitialGrammar" )
    val printFinalGrammar =
      opts.has( "printFinalGrammar" )
    assert( ! ( opts.has( "incConvergence" ) && opts.has( "fullyNormalizing" ) ) )

    println( s"trainStrings: ${trainStrings}" )
    println( s"testStrings: ${testStrings}" )
    println( s"fullyNormalizing: ${fullyNormalizing}" )
    println( s"parserType: ${parserType}" )
    println( s"incConvergence: ${incConvergence}" )
    println( s"incIters: ${incIters}" )
    println( s"miniBatchSize: ${miniBatchSize}" )
    println( s"initialMiniBatchSize: ${initialMiniBatchSize}" )
    println( s"scaleInitMiniBatchCounts: ${scaleInitMiniBatchCounts}" )
    println( s"harmonicMiniBatchInit: ${harmonicMiniBatchInit}" )
    println( s"harmonicCorpusInit: ${harmonicCorpusInit}" )
    println( s"printItersReached: ${printItersReached}" )
    println( s"batchVB: ${batchVB}" )
    println( s"infiniteModels: ${infiniteModels}" )
    println( s"baseDistribution: ${baseDistribution}" )
    println( s"logSpace: ${logSpace}" )
    println( s"particleFilter: ${particleFilter}" )
    println( s"numParticles: ${numParticles}" )
    println( s"reservoirSize: ${reservoirSize}" )
    println( s"noResampling: ${noResampling}" )
    println( s"printResamplingEvents: ${printResamplingEvents}" )
    println( s"convergeInitialMiniBatch: ${convergeInitialMiniBatch}" )
    println( s"largeMiniBatchEvery: ${largeMiniBatchEvery}" )
    println( s"logEvalRate: ${logEvalRate}" )
    println( s"evalEvery: ${evalEvery}" )
    println( s"evalMaxLength: ${evalMaxLength}" )
    println( s"alpha: ${alpha}" )
    println( s"rootAlpha: ${rootAlpha}" )
    println( s"stopAlpha: ${stopAlpha}" )
    println( s"chooseAlpha: ${chooseAlpha}" )
    println( s"backoffAlpha: ${backoffAlpha}" )
    println( s"notBackoffAlpha: ${notBackoffAlpha}" )
    println( s"squarelyNormalized: ${squarelyNormalized}" )
    println( s"uposCount: ${uposCount}" )
    println( s"randomSeed: ${randomSeed}" )
    println( s"constituencyEval: ${constituencyEval}" )
    println( s"printInitialGrammar: ${printInitialGrammar}" )
    println( s"printFinalGrammar: ${printFinalGrammar}" )

    // particle-reweighting not implemented yet for squarely normalized particles.
    assert( ! (particleFilter && squarelyNormalized) )

    val trainWordStrings = readStrings( trainStrings )
    val testWordStrings = readStrings( testStrings )

    val Seq( trainSet, testSet ) =
      stringsToUtts( infiniteModels, trainWordStrings, testWordStrings )

    // lazy val totalVocabSize = (trainSet ++ testSet).flatMap{_.string}.toSet.size
    lazy val totalVocabSize = stringsToUtts.dictionary.size
    println( s"${trainSet.size} training strings" )
    println( s"${testSet.size} testing strings" )
    println( s"${trainSet.map{_.string.size}.sum} training words" )
    println( s"${testSet.map{_.string.size}.sum} testing words" )
    println( s"${stringsToUtts.dictionary.size} unique words" )

    val maxLength = { trainSet ++ testSet }.map{ _.string.length }.max

    val r = new util.Random( randomSeed )

    val squareNorm = if( squarelyNormalized ) totalVocabSize else 0

    val parserSpec = ParserSpec(
      maxLength = maxLength,
      alphabetSize = if( baseDistribution == "monkeyModel" ) 27 else totalVocabSize,
      randomSeed = randomSeed,
      rootAlpha = rootAlpha,
      stopAlpha = stopAlpha,
      chooseAlpha = chooseAlpha,
      backoffAlpha = backoffAlpha,
      notBackoffAlpha = notBackoffAlpha,
      squarelyNormalized = squareNorm,
      scaleInitMiniBatchCounts = scaleInitMiniBatchCounts,
      harmonicMiniBatchInit = harmonicMiniBatchInit,
      approximate = false,
      reservoirSize = reservoirSize,
      uposCount = uposCount,
      logSpace = logSpace
    )

    // val p:FoldUnfoldParser[_<:ArcFactoredParameters] =
    // TODO: come up with a better way of picking the parser type...
    val p:FoldUnfoldParser[_<:DependencyCounts,_<:ArcFactoredParameters[_]] =
      if( particleFilter ) {
        if( parserType == "TopDownDMVParser" ) {
          println( "Using TopDownDMVParser" )
          new ParticleFilterNOPOSParser[DMVCounts,TopDownDMVParameters,TopDownDMVParser](
            // maxLength = 3,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              val p_l = new TopDownDMVParser(
                  // maxLength = 3,
                  // randomSeed = l,
                  // squarelyNormalized = squareNorm,
                  // // approximate = true
                  // approximate = false,
                  // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )// ,
            // reservoirSize = reservoirSize
          )
        } else if( parserType == "OriginalDMVParser" ) {
          println( "Using OriginalDMVParser" )
          new ParticleFilterNOPOSParser[DMVCounts,OriginalDMVParameters,OriginalDMVParser](
            // maxLength = 3,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              val p_l = new OriginalDMVParser(
                    // maxLength = 3,
                    // randomSeed = l,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )//,
            // reservoirSize = reservoirSize
          )
        } else if( parserType == "NoValenceParser" ) {
          println( "Using NoValenceParser" )
          new ParticleFilterNOPOSParser[DMVCounts,NoValenceParameters,NoValenceParser](
            // maxLength = maxLength,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              // println( s"creating new particle!" )
              // counts.printTotalCountsByType
              val p_l = new NoValenceParser(
                    // maxLength = 3,
                    // randomSeed = l,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // println( s"created parser with random seed $l" )
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )//,
            // reservoirSize = reservoirSize
          )
        } else if( parserType == "FiveValenceParser" ) {
          println( "Using FiveValenceParser" )
          new ParticleFilterNOPOSParser[DMVCounts,FiveValenceParameters,FiveValenceParser](
            // maxLength = maxLength,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              // println( s"creating new particle!" )
              // counts.printTotalCountsByType
              val p_l = new FiveValenceParser(
                    // maxLength = 3,
                    // randomSeed = l,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )//,
            // reservoirSize = reservoirSize
          )
        } else if( parserType == "FourValenceParser" ) {
          println( "Using FourValenceParser" )
          new ParticleFilterNOPOSParser[DMVCounts,FourValenceParameters,FourValenceParser](
            // maxLength = maxLength,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              // println( s"creating new particle!" )
              // counts.printTotalCountsByType
              val p_l = new FourValenceParser(
                    // maxLength = 3,
                    // randomSeed = l,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )//,
            // reservoirSize = reservoirSize
          )
        } else if( parserType == "ThreeValenceParser" ) {
          println( "Using ThreeValenceParser" )
          new ParticleFilterNOPOSParser[DMVCounts,ThreeValenceParameters,ThreeValenceParser](
            // maxLength = maxLength,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              // println( s"creating new particle!" )
              // counts.printTotalCountsByType
              val p_l = new ThreeValenceParser(
                    // maxLength = 3,
                    // randomSeed = l,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )//,
            // reservoirSize = reservoirSize
          )
        } else if( parserType == "HeadOutAdjHeadNoValence" ) {
          println( "Using HeadOutAdjHeadNoValence" )
          // new HeadOutAdjHeadNoValenceParser(
          //   maxLength = maxLength, randomSeed = randomSeed
          // )
          new
          ParticleFilterNOPOSParser[DMVCounts,HeadOutAdjHeadNoValenceParameters,HeadOutAdjHeadNoValenceParser](
            // maxLength = 3,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              val p_l =
                new HeadOutAdjHeadNoValenceParser(
                    // maxLength = 3,
                    // randomSeed = r.nextInt,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                  ParserSpec.withRandomSeed( parserSpec, l )
                )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )/*,
            reservoirSize = reservoirSize*/
          )
        } else if( parserType == "HeadOutInterpolatedAdjHeadNoValence" ) {
          println( "Using HeadOutInterpolatedAdjHeadNoValence" )
          // new HeadOutInterpolatedAdjHeadNoValenceParser(
          //   maxLength = maxLength,
          //   backoffAlpha = backoffAlpha,
          //   notBackoffAlpha = notBackoffAlpha
          // )
          new ParticleFilterNOPOSParser[BackoffChooseDMVCounts,HeadOutInterpolatedAdjHeadNoValenceParameters,HeadOutInterpolatedAdjHeadNoValenceParser](
            // maxLength = 3,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle =
            (counts:BackoffChooseDMVCounts,reservoir:Array[SampledCounts[BackoffChooseDMVCounts]],l:Int) => {
              val p_l =
                new HeadOutInterpolatedAdjHeadNoValenceParser(
                      // maxLength = 3,
                      // backoffAlpha = backoffAlpha,
                      // notBackoffAlpha = notBackoffAlpha,
                      // randomSeed = r.nextInt,
                      // squarelyNormalized = squareNorm,
                      // // approximate = true
                      // approximate = false,
                      // reservoirSize = reservoirSize
                  ParserSpec.withRandomSeed( parserSpec, l )
                )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = BackoffChooseDMVCounts(
              alpha,
              alpha,
              alpha,
              Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha ),
              true
            )/*,
            reservoirSize = reservoirSize*/
          )
        } else {
          println( "parser type not recognized -- defaulting to OriginalDMVParser" )
          new ParticleFilterNOPOSParser[DMVCounts,OriginalDMVParameters,OriginalDMVParser](
            // maxLength = 3,
            parserSpec = parserSpec,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,reservoir:Array[SampledCounts[DMVCounts]],l:Int) => {
              val p_l = new OriginalDMVParser(
                    // maxLength = 3,
                    // randomSeed = l,
                    // squarelyNormalized = squareNorm,
                    // // approximate = true
                    // approximate = false,
                    // reservoirSize = reservoirSize
                ParserSpec.withRandomSeed( parserSpec, l )
              )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              p_l.sampleReservoir = reservoir
              // p_l.theta.incrementCounts( counts )
              p_l
            },
            emptyCounts = DMVCounts( alpha, alpha, alpha, true )/*,
            reservoirSize = reservoirSize*/
          )
        }
      } else {
        if( parserType == "TopDownDMVParser" ) {
          if( infiniteModels ) {
            println( "Using Infinite TopDownDMVParser" )
            new InfiniteTopDownDMVParser(
              parserSpec
            )
          } else {
            println( "Using TopDownDMVParser" )
            new TopDownDMVParser(
              parserSpec
            )
          }
        } else if( parserType == "OriginalDMVParser" ) {
          println( "Using OriginalDMVParser" )
          new OriginalDMVParser(
            // maxLength,
            // randomSeed = randomSeed,
            // squarelyNormalized = squareNorm
            parserSpec
          )
        } else if( parserType == "NewNoValenceUPOSParser" ) {
          println( "Using NewNoValenceUPOSParser" )
          new NewNoValenceUPOSParser(
            // maxLength,
            // uposCount = uposCount,
            parserSpec = parserSpec
            // randomSeed = randomSeed
          )
        } else if( parserType == "NoValenceParser" ) {
          println( "Using NoValenceParser" )
          new NoValenceParser(
              // maxLength,
              // randomSeed = randomSeed,
              // squarelyNormalized = squareNorm
            parserSpec
          )
        } else if( parserType == "FiveValenceParser" ) {
          if( infiniteModels ) {
            println( "Using Infinite FiveValenceParser" )
            new InfiniteFiveValenceParser(
              parserSpec
            )
          } else {
            println( "Using FiveValenceParser" )
            new FiveValenceParser(
              parserSpec
            )
          }
        } else if( parserType == "FourValenceParser" ) {
          println( "Using FourValenceParser" )
          new FourValenceParser(
              // maxLength,
              // randomSeed = randomSeed,
              // squarelyNormalized = squareNorm
            parserSpec
          )
        } else if( parserType == "ThreeValenceParser" ) {
          println( "Using ThreeValenceParser" )
          new ThreeValenceParser(
              // maxLength,
              // randomSeed = randomSeed,
              // squarelyNormalized = squareNorm
            parserSpec
          )
        } else if( parserType == "HeadOutAdjHeadNoValence" ) {
          println( "Using HeadOutAdjHeadNoValence" )
          new HeadOutAdjHeadNoValenceParser(
                // maxLength,
                // randomSeed = randomSeed,
                // squarelyNormalized = squareNorm
            parserSpec
          )
        } else if( parserType == "HeadOutInterpolatedAdjHeadNoValence" ) {
          println( "Using HeadOutInterpolatedAdjHeadNoValence" )
          new HeadOutInterpolatedAdjHeadNoValenceParser(
                // maxLength = maxLength,
                // backoffAlpha = backoffAlpha,
                // notBackoffAlpha = notBackoffAlpha,
                // randomSeed = randomSeed,
                // squarelyNormalized = squareNorm
            parserSpec
          )
        } else {
          println( "parser type not recognized -- defaulting to OriginalDMVParser" )
          new OriginalDMVParser(
                // maxLength = maxLength,
                // randomSeed = randomSeed,
                // squarelyNormalized = squareNorm
            parserSpec
          )
        }
      }

    if( particleFilter ) { p.initializeParticles }

    if( !infiniteModels ) p.zerosInit( trainSet ++ testSet )

    if( harmonicCorpusInit ) p.setConstantHarmonicCounts( trainSet )

    if( printInitialGrammar ) {
      println( "INITIAL GRAMMAR" )
      // p.theta.printOut()
    }

    val shuffledTrainSet = r.shuffle( trainSet.toList )

    // Batch VB is just a special case of minibatch VB where the first minibatch is set to the
    // entire training corpus. Use initialMiniBatchSize to control how much of the training corpus
    // we use (so we can get learning curves against corpus size).
    // val firstMiniBatch:List[Utt] =
    //     shuffledTrainSet.take( initialMiniBatchSize )

    // val subsequentMiniBatches:List[List[Utt]] =
    //   if( batchVB )
    //     Nil
    //   else
    //     shuffledTrainSet.toList.drop( initialMiniBatchSize ).grouped( miniBatchSize ).toList

    // println( s"largeMiniBatchEvery" )
    val miniBatches = buildMiniBatches(
      corpus = shuffledTrainSet,
      largeMiniBatchSize = initialMiniBatchSize,
      largeMiniBatchEvery = largeMiniBatchEvery,
      miniBatchSize = miniBatchSize,
      batchVB = batchVB
    )

    // println( miniBatches.map{ _.size }.mkString("\n") )

    // if( !(miniBatches.map{_.size}.sum == shuffledTrainSet.size) ) {
    //   println( miniBatches.map{_.size}.sum + " != " + shuffledTrainSet.size )
    // }
    assert( batchVB || miniBatches.map{_.size}.sum == shuffledTrainSet.size )

    var i = 0
    var sentencesSinceLastEval = 0
    // ( firstMiniBatch :: subsequentMiniBatches ).foreach{ mb =>
    miniBatches.foreach{ mb =>


      val startTime = System.currentTimeMillis
      val printFirstMBScores =
        (initialMiniBatchSize != miniBatchSize) && ( i == 0 )
      // p.miniBatchVB(
      if( printIterScores || printItersReached || batchVB || printIterScores )
        println( s"===== MINI BATCH $i" )

      val mbIters = p.streamingBayesUpdate(
        miniBatch = mb,
        sentenceNum = sentencesProcessed,
        testSet = testSet,
        maxIter = { if( convergeInitialMiniBatch && i == 0 ) 0 else incIters },
        convergence = incConvergence,
        evalMaxLength = evalMaxLength,
        evalRate = evalEvery,
        logEvalRate = logEvalRate,
        constituencyEval = constituencyEval,
        printIterScores = batchVB || printFirstMBScores || printIterScores,
        printItersReached = printItersReached
      )
      val endTime = System.currentTimeMillis
      totalDur += endTime - startTime
      miniBatchDur += endTime - startTime
      totalIters += mbIters
      totalMiniBatches += 1



      // // DELETE BELOW
      // println( s"iteration $i grammar" )
      // p.theta.printOut()
      // println( s"end $i -------\n\n\n" )


      sentencesSinceLastEval += mb.size
      sentencesProcessed += mb.size

                                            // Particle weights will be uniform if we resample right
                                            // before evaluation...
      if( particleFilter && (!noResampling) && sentencesProcessed % evalEvery != 0 ) {
        val resamplingStartTime = System.currentTimeMillis
        val ess = p.ess
        if( ess < numParticles/2 ) {
          if( printResamplingEvents) {
            println( s"it${sentencesProcessed}:resamplingESS:${ess}")
          }

          val ( ancestorCount, acceptanceRate ) = p.resampleParticles

          ancestors += ancestorCount

          if( printResamplingEvents) {
            println( s"it${sentencesProcessed}:ancestorsResampled:${ancestorCount}")
            println( s"it${sentencesProcessed}:acceptanceRate:${acceptanceRate}")
          }
          resamplingEventCounts += 1
        } else {
          if( printResamplingEvents) println( s"it${sentencesProcessed}:notResamplingESS:${ess}")
        }
        resamplingDur += System.currentTimeMillis - resamplingStartTime
      }

      if(
        sentencesProcessed%evalEvery == 0 && !(
          sentencesProcessed == initialMiniBatchSize && incIters == 1
        )
      ) {

        // val ( thisHeldOutLogProb, thisTestTime ) =
        val thisTestTime =
          if( constituencyEval )
            p.printViterbiParses( testSet, s"it${sentencesProcessed}", evalMaxLength )
          else
            p.printViterbiDepParses( testSet, s"it${sentencesProcessed}", evalMaxLength )

        // p.theta.printTotalCountsByType

            // var heldOutLogProb = 0D
            // val parseStartTime = System.currentTimeMillis
            // testSet.foreach{ s =>
            //   if( evalMaxLength == 0 || s.string.length <= evalMaxLength )
            //     if( constituencyEval ) {
            //       val Parse( id, conParse, depParse ) = p.viterbiParse( s )
            //       println( s"it${sentencesProcessed}:constituency:${id} ${conParse}" )
            //       println( s"it${sentencesProcessed}:dependency:${id} ${printDependencyParse(depParse)}" )
            //     } else {
            //       val Parse( id, _, depParse ) = p.viterbiDepParse( s )
            //       println( s"it${sentencesProcessed}:dependency:${id} ${printDependencyParse(depParse)}" )
            //     }
            //     heldOutLogProb += p.logProb( s.string )
            // }

            // val thisTestTime = System.currentTimeMillis - parseStartTime
        // heldOutLogProb += thisHeldOutLogProb
        totalTestDur += thisTestTime
        testEvents += testSet.size

          // if( i == 0 )
          //   miniBatchDur/(((initialMiniBatchSize + (evalEvery-1)*miniBatchSize)).toDouble)
          // else
          //   miniBatchDur/evalEvery.toDouble

        val timePerSentence = miniBatchDur / sentencesSinceLastEval

        println( s"it${sentencesProcessed}:trainTimePerSentence:${timePerSentence}ms/sentence" )
        if( particleFilter ) {
          println( s"it${sentencesProcessed}:particlePerplexity:${p.particlePerplexity}" )
          println( s"it${sentencesProcessed}:ess:${p.ess}" )
        }
        miniBatchDur = 0
        sentencesSinceLastEval = 0
      }
      i += 1

      if(
        logEvalRate &&
        scala.math.log10( sentencesProcessed )%1 == 0 &&
        sentencesProcessed > evalEvery &&
        ( sentencesProcessed > initialMiniBatchSize /* || incIters != 1 */ )
      ) {
        evalEvery *= 10
        println( s"after processing $sentencesProcessed we eval every $evalEvery" )
      } else if(
        ( sentencesProcessed >= evalEvery*10 )// && incIters == 1
      ) {
        // evalEvery = initialMiniBatchSize
          // evalEvery should be nearest power of ten below sentencesProcessed
        evalEvery = pow( 10, floor( log10( sentencesProcessed ) ) ).toInt
      }
    }
    // println( s"sentencesProcessed: $sentencesProcessed" )
    // println( s"trainSet.size: ${trainSet.size}" )
    // assert( sentencesProcessed == trainSet.size.toDouble )
    println( s"overall training took ${totalDur/(sentencesProcessed)}ms/sentence" )

    val trainingSentCount = miniBatches.map{_.size}.sum
      // ( firstMiniBatch :: subsequentMiniBatches ).map{ _.size }.sum
    if( !( trainingSentCount % evalEvery == 0 ) ) {
      var heldOutLogProb = 0D
      val parseStartTime = System.currentTimeMillis
      testSet.foreach{ utt =>
        if( constituencyEval ) {
          val Parse( id, conParse, depParse ) = p.viterbiParse( utt )
          println( s"it${trainingSentCount}:constituency:${id} ${conParse}" )
          println( s"it${trainingSentCount}:dependency:${id} ${printDependencyParse(depParse)}" )
        } else {
          val Parse( id, _, depParse ) = p.viterbiDepParse( utt )
          println( s"it${trainingSentCount}:dependency:${id} ${printDependencyParse(depParse)}" )
        }
        heldOutLogProb += p.logProb( utt )
      }
      val parseEndTime = System.currentTimeMillis
      println( s"it${trainingSentCount}:logProb:${heldOutLogProb}" )
      println( s"it${trainingSentCount}:testTimePerSentence:${ (parseEndTime - parseStartTime ) / testSet.size}ms/sentence" )
      println( s"it${trainingSentCount}:particlePerplexity:${p.particlePerplexity}" )
      println( s"it${sentencesProcessed}:ess:${p.ess}" )
    }

    // if( particleFilter ) {
    //   println( s"resampled ${resamplingEventCounts} times" )
    //   println( s"${resamplingDur/sentencesProcessed}ms resampling per training sentence")
    // }

    p.theta.printTotalCountsByType

    if( printFinalGrammar ) {
      println( "FINAL GRAMMAR" )
      // p.theta.printOut()
    }


  }
}



