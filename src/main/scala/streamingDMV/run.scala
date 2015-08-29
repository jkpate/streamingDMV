package streamingDMV

import joptsimple.OptionParser
import joptsimple.OptionSet


import streamingDMV.io._
import streamingDMV.parsers._
import streamingDMV.labels._
import streamingDMV.parameters._
import streamingDMV.labels.{Parse,Utt,DependencyCounts}


object run {
  var miniBatchDur = 0D
  var totalDur = 0D
  var sentencesProcessed = 0
  sys.addShutdownHook {
    println( s"${sentencesProcessed} sentences processed" )
    println( s"${totalDur/sentencesProcessed}ms per training sentence")
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
    optsParser.accepts( "alpha" ).withRequiredArg
    optsParser.accepts( "backoffAlpha" ).withRequiredArg
    optsParser.accepts( "notBackoffAlpha" ).withRequiredArg
    optsParser.accepts( "uposCount" ).withRequiredArg
    optsParser.accepts( "randomSeed" ).withRequiredArg
    optsParser.accepts( "miniBatchSize" ).withRequiredArg
    optsParser.accepts( "initialMiniBatchSize" ).withRequiredArg
    optsParser.accepts( "convergeInitialMiniBatch" )
    optsParser.accepts( "printItersReached" )
    optsParser.accepts( "batchVB" )
    optsParser.accepts( "particleFilter" )
    optsParser.accepts( "numParticles" ).withRequiredArg
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
        0.00001
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
    val convergeInitialMiniBatch = opts.has( "convergeInitialMiniBatch" )
    val batchVB = opts.has( "batchVB" )
    val particleFilter = opts.has( "particleFilter" )
    val numParticles =
      if( opts.has( "numParticles" ) )
        opts.valueOf( "numParticles" ).toString.toInt
      else
        16
    val printResamplingEvents = opts.has( "printResamplingEvents" )
    val logEvalRate = opts.has( "logEvalRate" )
    var evalEvery =
      if(opts.has( "evalEvery" ))
        opts.valueOf( "evalEvery" ).toString.toInt
      else
        initialMiniBatchSize
    val printItersReached =
      opts.has( "printItersReached" )
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
    println( s"printItersReached: ${printItersReached}" )
    println( s"batchVB: ${batchVB}" )
    println( s"particleFilter: ${particleFilter}" )
    println( s"numParticles: ${numParticles}" )
    println( s"printResamplingEvents: ${printResamplingEvents}" )
    println( s"convergeInitialMiniBatch: ${convergeInitialMiniBatch}" )
    println( s"logEvalRate: ${logEvalRate}" )
    println( s"evalEvery: ${evalEvery}" )
    println( s"alpha: ${alpha}" )
    println( s"backoffAlpha: ${backoffAlpha}" )
    println( s"notBackoffAlpha: ${notBackoffAlpha}" )
    println( s"uposCount: ${uposCount}" )
    println( s"randomSeed: ${randomSeed}" )
    println( s"constituencyEval: ${constituencyEval}" )
    println( s"printInitialGrammar: ${printInitialGrammar}" )
    println( s"printFinalGrammar: ${printFinalGrammar}" )


    val trainWordStrings = readStrings( trainStrings )
    val testWordStrings = readStrings( testStrings )

    val Seq( trainSet, testSet ) =
      stringsToUtts( trainWordStrings, testWordStrings )

    println( s"${trainSet.size} training strings" )
    println( s"${testSet.size} testing strings" )
    println( s"${trainSet.map{_.string.size}.sum} training words" )
    println( s"${testSet.map{_.string.size}.sum} testing words" )
    println( s"${stringsToUtts.dictionary.size} unique words" )

    val maxLength = { trainSet ++ testSet }.map{ _.string.length }.max

    val r = new util.Random( randomSeed )

    // val p:FoldUnfoldParser[_<:ArcFactoredParameters] =
    val p:FoldUnfoldParser[_<:DependencyCounts,_<:ArcFactoredParameters[_]] =
      if( particleFilter ) {
        if( parserType == "TopDownDMVParser" ) {
          println( "Using TopDownDMVParser" )
          new ParticleFilterNOPOSParser[TopDownDMVParameters,TopDownDMVParser](
            maxLength = maxLength,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,l:Int) => {
              val p_l = new TopDownDMVParser( maxLength = maxLength, randomSeed = l)
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              // p_l.theta.incrementCounts( counts )
              p_l
            }
          )
        } else if( parserType == "OriginalDMVParser" ) {
          println( "Using OriginalDMVParser" )
          new ParticleFilterNOPOSParser[OriginalDMVParameters,OriginalDMVParser](
            maxLength = maxLength,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,l:Int) => {
              val p_l = new OriginalDMVParser( maxLength = maxLength, randomSeed = l)
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              // p_l.theta.incrementCounts( counts )
              p_l
            }
          )
        } else if( parserType == "NoValenceParser" ) {
          println( "Using NoValenceParser" )
          new ParticleFilterNOPOSParser[NoValenceParameters,NoValenceParser](
            maxLength = maxLength,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,l:Int) => {
              val p_l = new NoValenceParser( maxLength = maxLength, randomSeed = l)
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              // p_l.theta.incrementCounts( counts )
              p_l
            }
          )
        } else if( parserType == "HeadOutAdjHeadNoValence" ) {
          println( "Using HeadOutAdjHeadNoValence" )
          // new HeadOutAdjHeadNoValenceParser(
          //   maxLength = maxLength, randomSeed = randomSeed
          // )
          new
          ParticleFilterNOPOSParser[HeadOutAdjHeadNoValenceParameters,HeadOutAdjHeadNoValenceParser](
            maxLength = maxLength,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,l:Int) => {
              val p_l =
                new HeadOutAdjHeadNoValenceParser(
                  maxLength = maxLength,
                  randomSeed = r.nextInt
                )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              // p_l.theta.incrementCounts( counts )
              p_l
            }
          )
        } else if( parserType == "HeadOutInterpolatedAdjHeadNoValence" ) {
          println( "Using HeadOutInterpolatedAdjHeadNoValence" )
          // new HeadOutInterpolatedAdjHeadNoValenceParser(
          //   maxLength = maxLength,
          //   backoffAlpha = backoffAlpha,
          //   notBackoffAlpha = notBackoffAlpha
          // )
          new ParticleFilterNOPOSParser[HeadOutInterpolatedAdjHeadNoValenceParameters,HeadOutInterpolatedAdjHeadNoValenceParser](
            maxLength = maxLength,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,l:Int) => {
              val p_l =
                new HeadOutInterpolatedAdjHeadNoValenceParser(
                  maxLength = maxLength,
                  backoffAlpha = backoffAlpha,
                  notBackoffAlpha = notBackoffAlpha,
                  randomSeed = r.nextInt
                )
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              // p_l.theta.incrementCounts( counts )
              p_l
            }
          )
        } else {
          println( "parser type not recognized -- defaulting to OriginalDMVParser" )
          new ParticleFilterNOPOSParser[OriginalDMVParameters,OriginalDMVParser](
            maxLength = maxLength,
            numParticles = numParticles,
            createParticle = (counts:DMVCounts,l:Int) => {
              val p_l = new OriginalDMVParser( maxLength = maxLength, randomSeed = l)
              // p_l.zerosInit( trainSet ++ testSet )
              p_l.theta.setEventsAndCounts( counts )
              // p_l.theta.incrementCounts( counts )
              p_l
            }
          )
        }
      } else {
        if( parserType == "TopDownDMVParser" ) {
          println( "Using TopDownDMVParser" )
          new TopDownDMVParser(
            { trainSet ++ testSet }.map{ _.string.length }.max,
            randomSeed = randomSeed
          )
        } else if( parserType == "OriginalDMVParser" ) {
          println( "Using OriginalDMVParser" )
          new OriginalDMVParser(
            { trainSet ++ testSet }.map{ _.string.length }.max,
            randomSeed = randomSeed
          )
        } else if( parserType == "NoValenceUPOSParser" ) {
          println( "Using NoValenceUPOSParser" )
          new NoValenceUPOSParser(
            { trainSet ++ testSet }.map{ _.string.length }.max,
            uposCount = uposCount,
            randomSeed = randomSeed
          )
        } else if( parserType == "NoValenceParser" ) {
          println( "Using NoValenceParser" )
          new NoValenceParser(
            { trainSet ++ testSet }.map{ _.string.length }.max,
            randomSeed = randomSeed
          )
        } else if( parserType == "HeadOutAdjHeadNoValence" ) {
          println( "Using HeadOutAdjHeadNoValence" )
          new HeadOutAdjHeadNoValenceParser(
            { trainSet ++ testSet }.map{ _.string.length }.max,
            randomSeed = randomSeed
          )
        } else if( parserType == "HeadOutInterpolatedAdjHeadNoValence" ) {
          println( "Using HeadOutInterpolatedAdjHeadNoValence" )
          new HeadOutInterpolatedAdjHeadNoValenceParser(
            maxLength = { trainSet ++ testSet }.map{ _.string.length }.max,
            backoffAlpha = backoffAlpha,
            notBackoffAlpha = notBackoffAlpha,
            randomSeed = randomSeed
          )
        } else {
          println( "parser type not recognized -- defaulting to OriginalDMVParser" )
          new OriginalDMVParser(
            maxLength = maxLength,
            randomSeed = randomSeed
          )
        }
      }

    p.zerosInit( trainSet ++ testSet )

    if( printInitialGrammar ) {
      println( "INITIAL GRAMMAR" )
      // p.theta.printOut()
    }

    val shuffledTrainSet = r.shuffle( trainSet.toList )

    // Batch VB is just a special case of minibatch VB where the first minibatch is set to the
    // entire training corpus. Use initialMiniBatchSize to control how much of the training corpus
    // we use (so we can get learning curves against corpus size).
    val firstMiniBatch:List[Utt] =
        shuffledTrainSet.take( initialMiniBatchSize )

    val subsequentMiniBatches:List[List[Utt]] =
      if( batchVB )
        Nil
      else
        shuffledTrainSet.toList.drop( initialMiniBatchSize ).grouped( miniBatchSize ).toList

    var i = 0
    var resamplingEventCounts = 0
    ( firstMiniBatch :: subsequentMiniBatches ).foreach{ mb =>

      val startTime = System.currentTimeMillis
      val printIterScores =
        (initialMiniBatchSize != miniBatchSize) && ( i == 0 )
      // p.miniBatchVB(

      p.streamingBayesUpdate(
        mb,
        { if( convergeInitialMiniBatch && i == 0 ) 0 else incIters },
        incConvergence,
        batchVB || printIterScores,
        printItersReached
      )
      val endTime = System.currentTimeMillis
      totalDur += endTime - startTime
      miniBatchDur += endTime - startTime


      // // DELETE BELOW
      // println( s"iteration $i grammar" )
      // p.theta.printOut()
      // println( s"end $i -------\n\n\n" )

      val timePerSentence =
        if( i == 0 )
          miniBatchDur/(((initialMiniBatchSize + (evalEvery-1)*miniBatchSize)).toDouble)
        else
          miniBatchDur/((evalEvery*miniBatchSize).toDouble)

      sentencesProcessed = initialMiniBatchSize + miniBatchSize*i

      if( particleFilter ) {
        val ess = p.ess
        if( ess < numParticles/2 ) {
          if( printResamplingEvents) println( s"it${sentencesProcessed}:resamplingESS:${ess}")
          p.resampleParticles
          resamplingEventCounts += 1
        } else {
          if( printResamplingEvents) println( s"it${sentencesProcessed}:notResamplingESS:${ess}")
        }
      }

      if( sentencesProcessed%evalEvery == 0 ) {
        miniBatchDur = 0

        var heldOutLogProb = 0D
        val parseStartTime = System.currentTimeMillis
        testSet.foreach{ s =>
          if( constituencyEval ) {
            val Parse( id, conParse, depParse ) = p.viterbiParse( s )
            println( s"it${sentencesProcessed}:constituency:${id} ${conParse}" )
            println( s"it${sentencesProcessed}:dependency:${id} ${printDependencyParse(depParse)}" )
          } else {
            val Parse( id, _, depParse ) = p.viterbiDepParse( s )
            println( s"it${sentencesProcessed}:dependency:${id} ${printDependencyParse(depParse)}" )
          }
          heldOutLogProb += p.logProb( s.string )
        }
        val parseEndTime = System.currentTimeMillis

        println( s"it${sentencesProcessed}:logProb:${heldOutLogProb}" )
        println( s"it${sentencesProcessed}:trainTimePerSentence:${timePerSentence}ms/sentence" )
        println( s"it${sentencesProcessed}:testTimePerSentence:${ (parseEndTime - parseStartTime ) / testSet.size}ms/sentence" )
        println( s"it${sentencesProcessed}:particlePerplexity:${p.particlePerplexity}" )
        println( s"it${sentencesProcessed}:ess:${p.ess}" )
      }
      i += 1

      if(
        logEvalRate &&
        math.log10( sentencesProcessed )%1 == 0 &&
        sentencesProcessed > evalEvery &&
        sentencesProcessed != initialMiniBatchSize
      ) {
        evalEvery *= 10
        println( s"after processing $sentencesProcessed we eval every $evalEvery" )
      }
    }
    println( s"overall training took ${totalDur/(trainSet.size.toDouble)}ms/sentence" )

    val trainingSentCount =
      ( firstMiniBatch :: subsequentMiniBatches ).map{ _.size }.sum
    var heldOutLogProb = 0D
    val parseStartTime = System.currentTimeMillis
    testSet.foreach{ s =>
      if( constituencyEval ) {
        val Parse( id, conParse, depParse ) = p.viterbiParse( s )
        println( s"it${trainingSentCount}:constituency:${id} ${conParse}" )
        println( s"it${trainingSentCount}:dependency:${id} ${printDependencyParse(depParse)}" )
      } else {
        val Parse( id, _, depParse ) = p.viterbiDepParse( s )
        println( s"it${trainingSentCount}:dependency:${id} ${printDependencyParse(depParse)}" )
      }
      heldOutLogProb += p.logProb( s.string )
    }
    val parseEndTime = System.currentTimeMillis
    println( s"it${trainingSentCount}:logProb:${heldOutLogProb}" )
    println( s"it${trainingSentCount}:testTimePerSentence:${ (parseEndTime - parseStartTime ) / testSet.size}ms/sentence" )
    println( s"it${trainingSentCount}:particlePerplexity:${p.particlePerplexity}" )

    if( particleFilter ) {
      println( s"resampled ${resamplingEventCounts} times" )
    }

    if( printFinalGrammar ) {
      println( "FINAL GRAMMAR" )
      // p.theta.printOut()
    }


  }
}



