package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

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
        p_stop.increment( possibleStopEvents( h ), myZero )

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

}

