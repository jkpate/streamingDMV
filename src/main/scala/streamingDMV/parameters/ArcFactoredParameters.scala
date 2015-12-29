package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

abstract class ArcFactoredParameters[C<:DependencyCounts](
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double
  parameterSpec:ParameterSpec
) {
  val rootAlpha = parameterSpec.rootAlpha
  val stopAlpha = parameterSpec.stopAlpha
  val chooseAlpha = parameterSpec.chooseAlpha
  val squarelyNormalized = parameterSpec.squarelyNormalized
  val approximate = parameterSpec.approximate
  val randomSeed = parameterSpec.randomSeed
  val alphabetSize = parameterSpec.alphabetSize
  val logSpace = parameterSpec.logSpace

  val myZero = if( parameterSpec.logSpace ) Double.NegativeInfinity else 0D
  val myOne = if( parameterSpec.logSpace ) 0D else 1D

  var fullyNormalized:Boolean = false
  def zerosInit( corpus:List[Utt] ):Unit
  def harmonicCounts( corpus:List[Utt] ):C

  def toCounts:C

  def randomizeCounts( seed:Int, scale:Int ):Unit

  def incrementCounts( counts:C, updateEvents:Boolean = true ):Unit
  // def setEvents( counts:C ):Unit
  def setEventsAndCounts( counts:C ):Unit

  def decrementCounts( counts:C, integerDec:Boolean = false ):Unit

  def printOut( logSpace:Boolean = false ):Unit

  def printTotalCountsByType:Unit


}

