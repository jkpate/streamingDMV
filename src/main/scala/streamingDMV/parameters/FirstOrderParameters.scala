package streamingDMV.parameters

import streamingDMV.labels._
import collection.mutable.{Set=>MSet}

abstract class FirstOrderArcFactoredParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  squarelyNormalized:Int = 0,
  approximate:Boolean = false,
  randomSeed:Int
) extends NOPOSArcFactoredParameters(
  rootAlpha,
  stopAlpha,
  chooseAlpha,
  squarelyNormalized,
  approximate,
  randomSeed
) {

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

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // rootEvents += RootEvent( h )
        // stopEvents ++= possibleStopEvents( h )
        p_root.increment( RootEvent( h ), Double.NegativeInfinity )
        p_stop.increment( possibleStopEvents( h ), Double.NegativeInfinity )

        ( 0 until t ).foreach{ i =>
          // chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), Double.NegativeInfinity )

        }
        ( t+1 until s.length ).foreach{ j =>
          // chooseEvents += ChooseEvent( h, RightAtt, s(j) )
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), Double.NegativeInfinity )
        }
      }
    }

    // println( s"  > ${p_choose.denoms.size} choose left-hand sides" )


    // p_root.setEvents( rootEvents.toSet )
    // p_stop.setEvents( stopEvents.toSet )
    // p_choose.setEvents( chooseEvents.toSet )
  }


}

