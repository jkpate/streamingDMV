package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

// import com.twitter.algebird.CMSMonoid

import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp,abs,floor,max,pow,sqrt}
import streamingDMV.math.LogSum

class AdaDeltaCPT[E<:Event with Product](
  alpha:Double,
  rho:Double = 0.9,
  eps:Double = 1E-9,
  randomSeed:Int = 15
) extends CPT[E](
  alpha = alpha,
  randomSeed = randomSeed
) {
  // probs maps an Event to a pair that is the probability for that event and the step at which it
  // was last updated
  var probs = Map[E,Tuple2[Double,Int]]().withDefaultValue( (0D, 0 ) )

  var sumSquaredGradient = Map[E,Double]().withDefaultValue( 0D )
  var sumDeltas = Map[E,Double]().withDefaultValue( 0D )


  var currentStep = 0

  override def apply( event:E ) = {
    // val ( p, updatedStep ) = probs( event )

    // // println( s"(p, updatedStep): ($p, $updatedStep)" )

    // // Fast-forward stepwise decay
    // if( updatedStep < currentStep ) {


    //   decayedP
    //   // probs( event )._1
    // } else {
    //   p
    // }

    // probs(event)._1
    fastForward( event )._3

  }

  // fast forward and return new sumSquaredGradient, new sumDeltas, and probability
  def fastForward( event:E ) = {
    val ( p , updatedStep ) = probs( event )
    if( updatedStep < currentStep ) {
      val n = event.normKey
      val events = denoms(n)

      val unseenG_t = taylorExpDigamma( alpha ) / taylorExpDigamma(alpha * events.size)

      var ffP = p
      var ffGrads = sumSquaredGradient( event )
      var ffDeltas = sumDeltas( event )

      ( updatedStep + 1 to currentStep ).foreach{ stepNum =>
        ffGrads =
          rho * ffGrads +
          (1 - rho) * unseenG_t

        val delta = (
          (
            sqrt( ffDeltas ) + eps
          ) / (
            sqrt( ffGrads )
          )
        ) * unseenG_t

        ffP += delta

        ffDeltas = (
          rho * ffDeltas +
          (1-rho) * pow( delta , 2 )
        )

      }

      probs += event -> ( ffP, currentStep )
      sumSquaredGradient += event -> ffGrads
      sumDeltas += event -> ffDeltas

      ( ffGrads, ffDeltas, ffP )

    } else {
      (
        sumSquaredGradient( event ),
        sumDeltas( event ),
        p
      )
    }
  }

                                // scale amplifies all counts to full training set size
  def step( other:CPT[E], scale:Double, initial:Boolean = false ) {

    if( !initial ) {
      other.multiplyBy( scale )


      // println( s"rho: $rho" )

      other.denomCounts.keys.foreach{ n =>
        val events = denoms(n)
        // val denom = G.digamma( other.denomCounts( n ) + alpha*events.size )
        val denom = taylorExpDigamma( other.denomCounts( n ) + alpha*events.size )
        events.foreach{ event =>

          val (ssGrad, ssDelta, p ) = fastForward( event )

          val g_t = taylorExpDigamma( other( event ) + alpha ) / denom

          val newSSGrad = rho * ssGrad + (1 - rho ) * pow( g_t, 2 )

          sumSquaredGradient += event -> newSSGrad

          // We're maximizing, so take a step in the direction of the gradient, not opposite the
          // gradient.
          val delta = (
            (
              sqrt( ssDelta ) + eps
            ) / (
              sqrt( newSSGrad )
            )
          ) * g_t

          probs += event -> ( probs(event)._2 + delta , currentStep + 1)

          sumDeltas += event -> (
            rho * ssDelta +
            ( 1 - rho ) * pow( delta, 2 )
          )

                // probs += event -> ( (
                //     rho * p +
                //       (1 - rho ) * apply( event )
                //   ),
                //   currentStep+1
                // )

        }
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
    println( "HWEE" )
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



