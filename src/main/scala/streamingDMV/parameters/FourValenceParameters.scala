package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

class FourValenceParameters(
  parameterSpec:ParameterSpec
) extends FirstOrderArcFactoredParameters(
  parameterSpec
) {

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, ThreeDependentValence, Stop ),
      StopEvent( h, LeftAtt, ThreeDependentValence, NotStop ),
      StopEvent( h, LeftAtt, TwoDependentValence, Stop ),
      StopEvent( h, LeftAtt, TwoDependentValence, NotStop ),
      StopEvent( h, LeftAtt, OneDependentValence, Stop ),
      StopEvent( h, LeftAtt, OneDependentValence, NotStop ),
      StopEvent( h, LeftAtt, NoDependentValence, Stop ),
      StopEvent( h, LeftAtt, NoDependentValence, NotStop ),
      StopEvent( h, RightAtt, ThreeDependentValence, Stop ),
      StopEvent( h, RightAtt, ThreeDependentValence, NotStop ),
      StopEvent( h, RightAtt, TwoDependentValence, Stop ),
      StopEvent( h, RightAtt, TwoDependentValence, NotStop ),
      StopEvent( h, RightAtt, OneDependentValence, Stop ),
      StopEvent( h, RightAtt, OneDependentValence, NotStop ),
      StopEvent( h, RightAtt, NoDependentValence, Stop ),
      StopEvent( h, RightAtt, NoDependentValence, NotStop )
    )
  }

}



