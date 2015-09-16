package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

abstract class ArcFactoredParameters[C<:DependencyCounts](
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) {

  var fullyNormalized:Boolean = false
  def zerosInit( corpus:List[Utt] ):Unit

  def toCounts:C

  def randomizeCounts( seed:Int, scale:Int ):Unit

  def incrementCounts( counts:C ):Unit
  // def setEvents( counts:C ):Unit
  def setEventsAndCounts( counts:C ):Unit

  def decrementCounts( counts:C ):Unit

  def printOut( logSpace:Boolean = false ):Unit

  def printTotalCountsByType { }

  def logSpace = false

}

