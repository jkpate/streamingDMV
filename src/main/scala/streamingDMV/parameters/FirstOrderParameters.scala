package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.{CPT,LogCPT}
import streamingDMV.math.LogSum
import collection.mutable.{Set=>MSet}

abstract class FirstOrderArcFactoredParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  // squarelyNormalized:Int = 0,
  // approximate:Boolean = false,
  // randomSeed:Int
  parameterSpec:ParameterSpec
) extends NOPOSArcFactoredParameters(
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha,
  // squarelyNormalized,
  // approximate,
  // randomSeed
  parameterSpec
) {

  def rootEvents( r:Int, rObs:String ) = Seq[Event]( RootEvent( r ) )

  def chooseEvents( h:Int, hObs:String, dir:AttDir, d:Int, dObs:String ) = {
    Seq[ChooseEvent]( ChooseEvent( h, dir, d ) )
  }

  def zerosInit( corpus:List[Utt] ) {
    // val rootEvents = MSet[RootEvent]()
    // val stopEvents = MSet[StopEvent]()
    // val chooseEvents = MSet[ChooseEvent]()
    // println( "FirstOrderArcFactoredParameters zeros init" )

    p_root.clear
    p_stop.clear
    p_choose.clear

    if( approximate ) {
      p_root.counts.approximateCounts.initializeCountsTable
      p_root.denomCounts.approximateCounts.initializeCountsTable

      p_stop.counts.approximateCounts.initializeCountsTable
      p_stop.denomCounts.approximateCounts.initializeCountsTable

      p_choose.counts.approximateCounts.initializeCountsTable
      p_choose.denomCounts.approximateCounts.initializeCountsTable
    }


    // corpus.map{_.string}.foreach{ s =>
    // println( s"inside init corpus length: ${corpus.length}" )
    // println( s"inside init first utterance: ${corpus.head}" )
    // println( s"my zero is: ${myZero}" )
    val windowSize = math.ceil( corpus.size.toDouble/20 )
    var i = 1
    corpus.foreach{ case Utt( _, s, w ) =>
      (0 until s.length).foreach{ t =>
        val h = s(t)
        val hW = if( w.length > 0 ) { w(t) } else { "" }

        // rootEvents += RootEvent( h )
        // stopEvents ++= possibleStopEvents( h )
        // p_root.increment( RootEvent( h ), myZero )
        rootEvents(h, hW).foreach{ rEvent =>
          rEvent match {
            case rootEvent:RootEvent => p_root.increment( rootEvent, myZero )
            case chooseEvent:ChooseEvent => p_choose.increment( chooseEvent, myZero )
          }
        }
        p_stop.increment( possibleStopEvents( h ), myZero, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          // chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
          // p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), myZero, updateEvents = true )
          val d= s(i)
          val dW = if( w.isEmpty ) "" else w(i)
          chooseEvents( h, hW, LeftAtt, d, dW ).foreach{ e =>
            p_choose.increment( e, myZero, updateEvents = true )
          }

        }
        ( t+1 until s.length ).foreach{ j =>
          // chooseEvents += ChooseEvent( h, RightAtt, s(j) )
          // p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), myZero, updateEvents = true )
          val d= s(j)
          val dW = if( w.isEmpty ) "" else w(j)
          chooseEvents( h, hW, LeftAtt, d, dW ).foreach{ e =>
            p_choose.increment( e, myZero, updateEvents = true )
          }
        }
      }

      if( i %windowSize == 0 ) {
        println( s"$i/${corpus.size} processed" )
      }
      i += 1
    }

      // println( s"at end of zeros init, printing grammar" )
      // printOut()
      // println( s"done printing grammar at end of zeros init" )

      // // println( s"  > ${p_choose.denoms.size} choose left-hand sides" )
      // println( s"zerosInit complete" )


    // p_root.setEvents( rootEvents.toSet )
    // p_stop.setEvents( stopEvents.toSet )
    // p_choose.setEvents( chooseEvents.toSet )
  }

  def harmonicCounts( miniBatch:List[Utt] ) = {
    // println( "FirstOrderArcFactoredParameters harmonic counts" )
    // val rootEvents = MSet[RootEvent]()
    // val stopEvents = MSet[StopEvent]()
    // val chooseEvents = MSet[ChooseEvent]()
    // println( "FirstOrderArcFactoredParameters zeros init" )

    // p_root.clear
    // p_stop.clear
    // p_choose.clear
    val rootCounts =
      if( logSpace )
        new LogCPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )

      // squarelyNormalized = 2 for p_stop b/c we always want to consider both Stop and NotStop
    val stopCounts =
      if( logSpace )
        new LogCPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
    val chooseCounts =
      if( logSpace )
        new LogCPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )

    if( approximate ) {
      rootCounts.counts.approximateCounts.initializeCountsTable
      rootCounts.denomCounts.approximateCounts.initializeCountsTable

      stopCounts.counts.approximateCounts.initializeCountsTable
      stopCounts.denomCounts.approximateCounts.initializeCountsTable

      chooseCounts.counts.approximateCounts.initializeCountsTable
      chooseCounts.denomCounts.approximateCounts.initializeCountsTable
    }

    val count = if( logSpace ) 0D else 1D
    miniBatch.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        rootCounts.increment( RootEvent( h ), count )
        stopCounts.increment( possibleStopEvents( h ), count, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              LogSum( 0D, -1 *math.log( t - i ) )
            else
              1D + (1D / ( t - i ) )

          // println( ChooseEvent( h, LeftAtt, s(i) ) + ": " + harmonicCount )
          chooseCounts.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              LogSum( 0D, -1 *math.log( j - t ) )
            else
              1D + ( 1D / ( j - t ) )

          chooseCounts.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
        }
      }
    }

    // val toReturn =
    DMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts
    )

    // println( s"returning... " )
    // toReturn.printTotalCountsByType 
    // println( s">>" )

    // toReturn
  }

  def randomCounts( miniBatch:List[Utt] ) = {
    // println( "FirstOrderArcFactoredParameters harmonic counts" )
    // val rootEvents = MSet[RootEvent]()
    // val stopEvents = MSet[StopEvent]()
    // val chooseEvents = MSet[ChooseEvent]()
    // println( "FirstOrderArcFactoredParameters zeros init" )

    // p_root.clear
    // p_stop.clear
    // p_choose.clear
    val rootCounts =
      if( logSpace )
        new LogCPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )

      // squarelyNormalized = 2 for p_stop b/c we always want to consider both Stop and NotStop
    val stopCounts =
      if( logSpace )
        new LogCPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
    val chooseCounts =
      if( logSpace )
        new LogCPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )

    if( approximate ) {
      rootCounts.counts.approximateCounts.initializeCountsTable
      rootCounts.denomCounts.approximateCounts.initializeCountsTable

      stopCounts.counts.approximateCounts.initializeCountsTable
      stopCounts.denomCounts.approximateCounts.initializeCountsTable

      chooseCounts.counts.approximateCounts.initializeCountsTable
      chooseCounts.denomCounts.approximateCounts.initializeCountsTable
    }

    def count = if( logSpace ) math.log( rand.nextDouble ) else rand.nextDouble
    miniBatch.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        rootCounts.increment( RootEvent( h ), count )
        stopCounts.increment( possibleStopEvents( h ), count, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              LogSum( 0D, -1 *math.log( t - i ) )
            else
              1D + (1D / ( t - i ) )

          // println( ChooseEvent( h, LeftAtt, s(i) ) + ": " + harmonicCount )
          chooseCounts.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              LogSum( 0D, -1 *math.log( j - t ) )
            else
              1D + ( 1D / ( j - t ) )

          chooseCounts.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
        }
      }
    }

    // val toReturn =
    DMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts
    )

    // println( s"returning... " )
    // toReturn.printTotalCountsByType 
    // println( s">>" )

    // toReturn
  }


}

