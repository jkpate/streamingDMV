package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

// import com.twitter.algebird.CMSMonoid

import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp,abs,floor,max}
import streamingDMV.math.LogSum

class SteppingCPT[E<:Event with Product](
  alpha:Double,
  kappa:Double = 0.9,
  tau:Double = 1,
  randomSeed:Int = 15
) extends CPT[E](
  alpha = alpha,
  randomSeed = randomSeed
) {
  // probs maps an Event to a pair that is the probability for that event and the step at which it
  // was last updated
  var probs = Map[E,Tuple2[Double,Int]]().withDefaultValue( (0D, 0 ) )

  def stepSize( step:Int ) = math.pow( step + tau, -1 * kappa )

  var currentStep = 0

  override def apply( event:E ) = {
    val ( p, updatedStep ) = probs( event )

    // println( s"(p, updatedStep): ($p, $updatedStep)" )

    // Fast-forward stepwise decay
    if( updatedStep < currentStep ) {
      var decayedP = p * ( 1 - stepSize( updatedStep ) )
      ( updatedStep+2 to currentStep ).foreach{ s =>
        decayedP *= ( 1 - stepSize( s ) )
      }

      probs += event -> ( decayedP , currentStep )

      decayedP
    } else {
      p
    }

  }

                                // scale amplifies all counts to full training set size
  def step( other:CPT[E], scale:Double, initial:Boolean = false ) {

    if( !initial ) {
      other.multiplyBy( scale )

      val rho = stepSize( currentStep+1 )

      // println( s"rho: $rho" )

      other.counts.exactCounts.keys.foreach{ event =>
        probs += event -> ( (
            rho * other.expDigammaNormalized( event ) +
              (1 - rho ) * apply( event )
          ),
          currentStep
        )
      }

      currentStep += 1
    } else {
      other.counts.exactCounts.keys.foreach{ event =>
        // println( s"$event: ${other.expDigammaNormalized( event )}" )
        probs += event -> ( (
            other.expDigammaNormalized( event )
          ),
          0
        )
      }
    }
  }

  def makeUniform {
    assert( denoms.size > 0 )

    denoms.foreach{ case (n, events) =>
      probs ++= events.map{ event =>
        event -> ( 1D / events.size, 0 )
      }.toMap
    }

  }

  override def printOut( logSpace:Boolean = false ) {
    denoms.foreach{ case (n, events) =>
      println( s"$n:" )
      events.foreach{ e =>
        if( logSpace )
          println( s"  $e: ${math.log(probs(e)._1)}" )
        else
          println( s"  $e: ${probs(e)._1}" )
      }
    }
  }


}


