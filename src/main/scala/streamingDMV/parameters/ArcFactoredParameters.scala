package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

abstract class ArcFactoredParameters[C<:DependencyCounts](
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) {

  def zerosInit( corpus:List[Utt] ):Unit

  def randomizeCounts( seed:Int, scale:Int ):Unit

  def incrementCounts( counts:C ):Unit

  def decrementCounts( counts:C ):Unit

  // def printOut():Unit

}

