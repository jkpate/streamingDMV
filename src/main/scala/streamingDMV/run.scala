package streamingDMV

import joptsimple.OptionParser
import joptsimple.OptionSet


import streamingDMV.io._
import streamingDMV.parsers._
import streamingDMV.parameters._
import streamingDMV.labels.{Parse,Utt}


object run {
  def main( args:Array[String] ) {
    val optsParser = new OptionParser()

    optsParser.accepts( "trainStrings" ).withRequiredArg
    optsParser.accepts( "testStrings" ).withRequiredArg
    optsParser.accepts( "fullyNormalizing" )
    optsParser.accepts( "incConvergence" ).withRequiredArg
    optsParser.accepts( "incIters" ).withRequiredArg
    optsParser.accepts( "logEvalRate" )
    optsParser.accepts( "evalEvery" ).withRequiredArg
    optsParser.accepts( "alpha" ).withRequiredArg
    optsParser.accepts( "randomSeed" ).withRequiredArg
    optsParser.accepts( "miniBatchSize" ).withRequiredArg
    optsParser.accepts( "initialMiniBatchSize" ).withRequiredArg
    optsParser.accepts( "printItersReached" )
    optsParser.accepts( "batchVB" )
    optsParser.accepts( "constituencyEval" )
    optsParser.accepts( "printInitialGrammar" )
    optsParser.accepts( "printFinalGrammar" )

    val opts = optsParser.parse( args:_* )

    val trainStrings = opts.valueOf( "trainStrings" ).toString
    val testStrings = opts.valueOf( "testStrings" ).toString
    val fullyNormalizing = opts.has( "fullyNormalizing" )
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
    val batchVB = opts.has( "batchVB" )
    val logEvalRate = opts.has( "logEvalRate" )
    var evalEvery =
      if(opts.has( "evalEvery" )) opts.valueOf( "evalEvery" ).toString.toInt else 0
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
    println( s"incConvergence: ${incConvergence}" )
    println( s"incIters: ${incIters}" )
    println( s"miniBatchSize: ${miniBatchSize}" )
    println( s"initialMiniBatchSize: ${initialMiniBatchSize}" )
    println( s"printItersReached: ${printItersReached}" )
    println( s"logEvalRate: ${logEvalRate}" )
    println( s"evalEvery: ${evalEvery}" )
    println( s"alpha: ${alpha}" )
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

    val p = new TopDownDMVParser( { trainSet ++ testSet }.map{ _.string.length }.max )

    p.zerosInit( trainSet ++ testSet )

    val r = new util.Random( randomSeed )
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
    var miniBatchDur = 0D
    var totalDur = 0D
    ( firstMiniBatch :: subsequentMiniBatches ).foreach{ mb =>

      val startTime = System.currentTimeMillis
      p.miniBatchVB( mb, incIters, incConvergence, batchVB, printItersReached )
      val endTime = System.currentTimeMillis
      totalDur += endTime - startTime
      miniBatchDur += endTime - startTime

      val timePerSentence =
        if( i == 0 )
          miniBatchDur/(evalEvery*initialMiniBatchSize)
        else
          miniBatchDur/(evalEvery*miniBatchSize)

      if( i%evalEvery == 0 ) {
        println( s"training took ${timePerSentence}ms/sentence" )
        miniBatchDur = 0

        val parseStartTime = System.currentTimeMillis
        testSet.foreach{ s =>
          if( constituencyEval ) {
            val Parse( id, conParse, depParse ) = p.viterbiParse( s )
            println( s"it${i}:constituency:${id} ${conParse}" )
            println( s"it${i}:dependency:${id} ${printDependencyParse(depParse)}" )
          } else {
            val Parse( id, _, depParse ) = p.viterbiParse( s )
            println( s"it${i}:dependency:${id} ${printDependencyParse(depParse)}" )
          }
        }
        val parseEndTime = System.currentTimeMillis
        println( s"test took ${ (parseEndTime - parseStartTime ) / testSet.size}ms" )
      }
      i += 1

      if(
        logEvalRate &&
        math.log10(initialMiniBatchSize + ((i-1)*miniBatchSize) )%1 == 0 &&
        i != evalEvery &&
        i != initialMiniBatchSize
      ) {
        evalEvery *= 10
        println( s"$i eval every $evalEvery" )
      }
    }
    println( s"overall training took ${totalDur/trainSet.size}ms/sentence" )

    val parseStartTime = System.currentTimeMillis
    testSet.foreach{ s =>
      if( constituencyEval ) {
        val Parse( id, conParse, depParse ) = p.viterbiParse( s )
        println( s"convergence:constituency:${id} ${conParse}" )
        println( s"convergence:dependency:${id} ${printDependencyParse(depParse)}" )
      } else {
        val Parse( id, _, depParse ) = p.viterbiParse( s )
        println( s"convergence:dependency:${id} ${printDependencyParse(depParse)}" )
      }
    }
    val parseEndTime = System.currentTimeMillis
    println( s"test took ${ (parseEndTime - parseStartTime ) / testSet.size}ms" )

  }
}



