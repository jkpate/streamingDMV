package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

class HeadOutAdjHeadNoValenceParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) extends ArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha ) {

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, NoValence, Stop ),
      StopEvent( h, LeftAtt, NoValence, NotStop ),
      StopEvent( h, RightAtt, NoValence, Stop ),
      StopEvent( h, RightAtt, NoValence, NotStop )
    )
  }
}

