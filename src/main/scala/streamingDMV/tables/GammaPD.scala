package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

// import com.twitter.algebird.CMSMonoid

import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp,abs,floor,max,pow}
import streamingDMV.math.LogSum

// Gamma-Poisson distribution for dependent counts
class GammaPD(
  alpha:Double,
  beta:Double,
  randomSeed:Int = 15
) extends CPT[DepCount](
  alpha,
  randomSeed = randomSeed
) {
  // use counts to store the lengths 
  // use denomCounts to store expected number of times each length appears.
  def gammaMean( a:Double, b:Double ) = {
    a/b
  }

  def gammaLogMean( a:Double, b:Double ) = {
    G.digamma( a ) - G.digamma( b )
  }

  override def normalized( event:DepCount ) = {
    gammaMean(
      a = counts( event ) ,
      b = denomCounts( event.normKey )
    )
  }

  // override def expDigammaNormalized( event:E ) = {
  //   exp(
  //   )
  // }

}



