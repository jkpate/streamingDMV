package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT
import streamingDMV.math.LogSum

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
  val backoffAlpha = parameterSpec.backoffAlpha
  val notBackoffAlpha = parameterSpec.notBackoffAlpha
  val squarelyNormalized = parameterSpec.squarelyNormalized
  val approximate = parameterSpec.approximate
  val randomSeed = parameterSpec.randomSeed
  val alphabetSize = parameterSpec.alphabetSize
  val logSpace = parameterSpec.logSpace

  val myZero = if( parameterSpec.logSpace ) Double.NegativeInfinity else 0D
  val myOne = if( parameterSpec.logSpace ) 0D else 1D

  def logPlus( nums:Double* ) = { nums.reduce( LogSum(_,_) ) }
  def realPlus( nums:Double* ) = { nums.sum }
  def logTimes( nums:Double* ) = { nums.sum }
  def realTimes( nums:Double* ) = { nums.reduce(_*_) }

  def logDiv( a:Double, b:Double ) = { a - b }
  def realDiv( a:Double, b:Double ) = { a / b }
  def myDiv = if( logSpace ) logDiv _ else realDiv _

  // in scala 2.10+ partially applied varargs functions take Seq[_]...
  val myPlusSeq = if( logSpace ) logPlus _ else realPlus _
  def myPlus( nums:Double* ) = myPlusSeq( nums )

  val myTimesSeq = if( logSpace ) logTimes _ else realTimes _
  def myTimes( nums:Double* ) = myTimesSeq( nums )

  var fullyNormalized:Boolean = false
  def zerosInit( corpus:List[Utt] ):Unit
  def harmonicCounts( corpus:List[Utt] ):C
  def randomCounts( corpus:List[Utt] ):C

  def cacheLGammas:Unit

  def toCounts:C

  def randomizeCounts( seed:Int, scale:Int ):Unit

  def incrementCounts( counts:C, updateEvents:Boolean = true ):Unit
  // def setEvents( counts:C ):Unit
  def setEventsAndCounts( counts:C ):Unit

  def decrementCounts( counts:C, integerDec:Boolean = false ):Unit

  def printOut( logSpace:Boolean = false ):Unit

  def printTotalCountsByType:Unit


}

