package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

class OriginalDMVParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  // squarelyNormalized:Int = 0,
  // approximate:Boolean = false,
  // randomSeed:Int
  parameterSpec:ParameterSpec
) extends FirstOrderArcFactoredParameters(
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
      StopEvent( h, LeftAtt, Outer, Stop ),
      StopEvent( h, LeftAtt, Outer, NotStop ),
      StopEvent( h, LeftAtt, Innermost, Stop ),
      StopEvent( h, LeftAtt, Innermost, NotStop ),
      StopEvent( h, RightAtt, Outer, Stop ),
      StopEvent( h, RightAtt, Outer, NotStop ),
      StopEvent( h, RightAtt, Innermost, Stop ),
      StopEvent( h, RightAtt, Innermost, NotStop )
    )
  }

}


