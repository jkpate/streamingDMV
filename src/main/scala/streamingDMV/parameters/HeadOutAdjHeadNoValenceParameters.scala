package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.{CPT,LogCPT}

import collection.mutable.{Set=>MSet}

class HeadOutAdjHeadNoValenceParameters(
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

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, NoValence, Stop ),
      StopEvent( h, LeftAtt, NoValence, NotStop ),
      StopEvent( h, RightAtt, NoValence, Stop ),
      StopEvent( h, RightAtt, NoValence, NotStop )
    )
  }

  def zerosInit( corpus:List[Utt] ) {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
    p_root.clear
    p_stop.clear
    p_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        p_root.increment( RootEvent( h ), myZero )
        p_stop.increment( possibleStopEvents( h ), myZero, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), myZero )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), myZero )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), myZero )
          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), myZero )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

  }

  def harmonicCounts( corpus:List[Utt] ) = {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
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

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // p_root.increment( RootEvent( h ), myZero )
        // p_stop.increment( possibleStopEvents( h ), myZero )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              -1 *math.log( i - t )
            else
              1D / ( i - t )

          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), harmonicCount )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              -1 *math.log( j - t )
            else
              1D / ( j - t )

          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), harmonicCount )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

    DMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts
    )


  }

  def randomCounts( corpus:List[Utt] ) = {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
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

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // p_root.increment( RootEvent( h ), myZero )
        // p_stop.increment( possibleStopEvents( h ), myZero )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              math.log( rand.nextDouble )
            else
              rand.nextDouble

          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), harmonicCount )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              -1 *math.log( j - t )
            else
              1D / ( j - t )

          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), harmonicCount )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

    DMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts
    )


  }

}

