package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

class TopDownDMVParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) extends ArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha ) {

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, Outermost, Stop ),
      StopEvent( h, LeftAtt, Outermost, NotStop ),
      StopEvent( h, LeftAtt, Inner, Stop ),
      StopEvent( h, LeftAtt, Inner, NotStop ),
      StopEvent( h, RightAtt, Outermost, Stop ),
      StopEvent( h, RightAtt, Outermost, NotStop ),
      StopEvent( h, RightAtt, Inner, Stop ),
      StopEvent( h, RightAtt, Inner, NotStop )
    )
  }

}

