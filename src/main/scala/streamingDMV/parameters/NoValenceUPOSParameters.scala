package streamingDMV.parameters

import streamingDMV.labels._

import breeze.linalg._
import breeze.numerics._


class NoValenceUPOSParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  uposCount:Int
) extends UPOSArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha, uposCount ) {

  def zerosMatrix = DenseMatrix.zeros[Double]( uposCount, uposCount )

  def zerosInit( corpus:List[Utt] ) {

    p_root.clear
    p_stop.clear
    p_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // rootEvents += RootEvent( h )
        // stopEvents ++= possibleStopEvents( h )
        p_root.increment( RootEvent( h ), DenseMatrix.zeros[Double](uposCount, 1 ) )
        p_stop.increment( possibleStopEvents( h ), DenseMatrix.zeros[Double](1, uposCount ) )

        ( 0 until t ).foreach{ i =>
          // chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), zerosMatrix )
        }
        ( t+1 until s.length ).foreach{ j =>
          // chooseEvents += ChooseEvent( h, RightAtt, s(j) )
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), zerosMatrix )
        }
      }
    }

    // p_root.setEvents( rootEvents.toSet )
    // p_stop.setEvents( stopEvents.toSet )
    // p_choose.setEvents( chooseEvents.toSet )
  }

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, NoValence, Stop ),
      StopEvent( h, LeftAtt, NoValence, NotStop ),
      StopEvent( h, RightAtt, NoValence, Stop ),
      StopEvent( h, RightAtt, NoValence, NotStop )
    )
  }

}

