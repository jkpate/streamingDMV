package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.WeightsCPT

import collection.mutable.{Set=>MSet}
import math.{exp,log,sqrt}

trait WeightsParameters extends NOPOSArcFactoredParameters {

  override val p_root =
    new WeightsCPT[RootEvent](
      decay,
      epsilon
    )

  override val p_choose =
    new WeightsCPT[ChooseEvent](
      decay,
      epsilon
    )

  override val p_stop =
    new WeightsCPT[StopEvent](
      decay,
      epsilon
    )

  override def apply( r:RootEvent ) = {
    p_root.normalized( r )
  }

  override def apply( s:StopEvent ) = {
    p_stop.normalized( s )
  }

  override def apply( c:ChooseEvent ) = {
    p_choose.normalized( c )
  }

  def sgd( counts:DMVCounts, kappa:Double ) {
    updateWeights( counts, kappa )
  }
  def updateWeights( counts:DMVCounts, kappa:Double ) {
    p_root.updateWeights( counts.rootCounts, kappa )
    p_stop.updateWeights( counts.stopCounts, kappa )
    p_choose.updateWeights( counts.chooseCounts, kappa )
  }

  def clearMemory {
    p_root.clearMemory
    p_stop.clearMemory
    p_choose.clearMemory
  }

  def l2NormDerivative( kappa:Double ) = {
    2*kappa * (
      p_root.sumWeights + 
      p_stop.sumWeights + 
      p_choose.sumWeights
    )
  }

  def l2Norm( kappa:Double ) = {
    kappa * sqrt(
      p_root.sumSquaredWeights + 
      p_stop.sumSquaredWeights + 
      p_choose.sumSquaredWeights
    )
  }



}

