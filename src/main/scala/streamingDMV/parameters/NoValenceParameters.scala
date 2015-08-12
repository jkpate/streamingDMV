package streamingDMV.parameters

import streamingDMV.labels._

class NoValenceParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) extends FirstOrderArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha ) {

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, NoValence, Stop ),
      StopEvent( h, LeftAtt, NoValence, NotStop ),
      StopEvent( h, RightAtt, NoValence, Stop ),
      StopEvent( h, RightAtt, NoValence, NotStop )
    )
  }

}

