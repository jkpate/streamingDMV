package streamingDMV.parameters

import streamingDMV.labels._
import collection.mutable.{Set=>MSet}

abstract class FirstOrderArcFactoredParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) extends ArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha ) {

  def zerosInit( corpus:List[Utt] ) {
    // val rootEvents = MSet[RootEvent]()
    // val stopEvents = MSet[StopEvent]()
    // val chooseEvents = MSet[ChooseEvent]()

    p_root.clear
    p_stop.clear
    p_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // rootEvents += RootEvent( h )
        // stopEvents ++= possibleStopEvents( h )
        p_root.increment( RootEvent( h ), 0D )
        p_stop.increment( possibleStopEvents( h ), 0D )

        ( 0 until t ).foreach{ i =>
          // chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), 0D )
        }
        ( t+1 until s.length ).foreach{ j =>
          // chooseEvents += ChooseEvent( h, RightAtt, s(j) )
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), 0D )
        }
      }
    }

    // p_root.setEvents( rootEvents.toSet )
    // p_stop.setEvents( stopEvents.toSet )
    // p_choose.setEvents( chooseEvents.toSet )
  }

}

