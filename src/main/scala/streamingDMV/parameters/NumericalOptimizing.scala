package streamingDMV.parameters

import streamingDMV.labels.DMVCounts

trait NumericalOptimizing {

  def step( counts:DMVCounts, rEvents:Double, cEvents:Double, sEvents:Double, initial:Boolean =
    false ):Unit
}

