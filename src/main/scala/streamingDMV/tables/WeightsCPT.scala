package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

import math.{exp,pow,sqrt}


// TODO refactor tables so methods all make sense....
class WeightsCPT[E<:Event with Product](
  decay:Double = 0.9,
  epsilon:Double = 1E-4
) extends CPT[E]( 0, 0, 0, 0, false, 0 ) {

  // for now, store the weights in "counts" tables. Ignore totalCount -- it's ok if it goes
  // negative.


  val sumSquaredGradient = MMap[E,Double]().withDefaultValue( 0D )
  val sumSquaredDelta = MMap[E,Double]().withDefaultValue( 0D )
  val stepsSinceUpdate = MMap[E,Int]().withDefaultValue( 0 )

  def clearMemory {
    sumSquaredGradient.clear
    sumSquaredDelta.clear
    stepsSinceUpdate.clear
    cachedProbs.clear
  }

  val cachedProbs = MMap[E,Double]()
  override def normalized( event:E ) = {
    // all features are indicator functions for now, so no need to dot-product
    // println( event )
    // println( "number of events: " + denoms(event.normKey).size )
    // println( "numerator: " + apply( event ) )
    // println( "denominator: " + denoms( event.normKey ).toSeq.map{e=>exp(apply(e))}.sum )
    // println( "denomCounts: " + denomCounts( event.normKey ) )
    val score = 
    cachedProbs.getOrElseUpdate(
      event,
      exp(
        // apply( event ) - denomCounts( event.normKey )
        apply( event )
      ) / denomCounts( event.normKey )
    )
    // println( "score: " + score )
    if( !( score > 0 && score <= 1 + 1E-5 ) ) {
      println( event )
      println( apply( event ) )
      println( denomCounts( event.normKey ) )
    }
    score

  }

  def sumSquaredWeights = {
    counts.values.map{ pow( _, 2 ) }.sum
  }

  def sumWeights = {
    counts.values.sum
  }


  // used in updating each weight according to the gradient.
  def updateWeight( event:E, g_t:Double ) {
    val n = event.normKey

    // First, fastforward decay
    val kappa = 0
    // val decayMagnitude =  pow( decay*( pow( normalized(event) + 2 * kappa * apply(event), 2)), stepsSinceUpdate( event ) )
    // minimize hashing of event for efficiency
    // decay the gradient prior to update
    // val newSumSquaredGradient = ( sumSquaredGradient( event ) * decayMagnitude ) * decay +
    //   (1-decay) * pow( g_t, 2 )

    val newSumSquaredGradient = sumSquaredGradient( event ) * decay +
      (1-decay) * pow( g_t, 2 )

    sumSquaredGradient( event ) = newSumSquaredGradient

    // decay deltas after update
    val newSumSquaredDelta = sumSquaredDelta( event ) // * decayMagnitude
    // stepsSinceUpdate -= event

    // step in the direction of the gradient to maximize the objective function
    val newDelta = -1 * (
    // val newDelta = (
      // sqrt( newSumSquaredDelta + epsilon ) / sqrt( newSumSquaredGradient + epsilon )
      // ) * g_t
      g_t * stepSize )


    // println( s"$event\t$g_t\t$newDelta" )

    // increment by negative exponentiated weight because decrement sets a floor at 0 (to prevent negative
    // probabilities in multinomial models)
    // denomCounts.increment( event.normKey , -1*exp( apply( event ) ), allowNegative = true )
    denomCounts.increment( event.normKey , -1*exp( apply( event ) ), allowNegative = true )
    counts.increment( event , newDelta, allowNegative = true )
    denomCounts.increment( event.normKey , exp( apply( event ) ), allowNegative = false )

    // now decay deltas
    sumSquaredDelta( event ) = decay*newSumSquaredDelta + ( 1 - decay )*pow( newDelta, 2 )


    // if( updateEvents ) {
    //   denoms += n -> { denoms( n ) + event }
    // }
  }

  var stepSize = 0.01
  def sgd( otherCounts:CPT[E], kappa:Double ) {
    stepSize = 0.01
    updateWeights( otherCounts, kappa )
  }
  def updateWeights( otherCounts:CPT[E], kappa:Double ) {
    // This computes equation (4) of Berg-Kirkpatrick et al (2011)
    // Right now, assume one-hot indicator features
    // val gradient = otherCounts.denomCounts.keys.flatMap{ n =>
    //   denoms(n).map{ event =>
    //     // val modelPrediction = denoms(n).map{ e => exp(apply(e)) }.sum
    //     // val modelPrediction = denomCounts( n )
    //     // A HACK FOR THE SPECIAL CASE OF ONE-HOT INDICATOR FEATURE FUNCTIONS
    //     val modelPrediction = 1
    //     // to get model expectation for indicator features, just exponentiate the weight
    //     ( event , ( otherCounts( event ) - modelPrediction ) - 2 * kappa * apply(event) )
    //   }
    // }

    // gradient.foreach{ case ( event, g_t ) => updateWeight( event, g_t ) }

    var gradient = Map[E,Double]()


    // ONE-HOT feature encoding for rule expansions
    denoms.foreach{ case( n, events ) =>
      events.foreach{ event =>
              // // val modelPrediction = events.map{ event =>
              // //   normalized(event) * otherCounts(event)
              // // }.sum
              // // otherCounts.denoms.keys.foreach{ n =>
              // //   denoms(n).foreach{ event =>
              // // val modelPrediction = denomCounts( n )
              // // A HACK FOR THE SPECIAL CASE OF ONE-HOT INDICATOR FEATURE FUNCTIONS
              // // val modelPrediction = normalized( event )
              // val modelPrediction = 1
              // // val modelPrediction = denoms(n).map{ e => normalized(e) }.sum
              // // to get model expectation for indicator features, just exponentiate the weight
              // // ( event , ( otherCounts( event ) - modelPrediction ) )
              // // updateWeight( event, ( otherCounts( event ) - modelPrediction ) - 2 * kappa * apply( event ) )
              // updateWeight( event, otherCounts( event )* ( 1 - otherCounts(event) * modelPrediction)  - 2 * kappa * apply( event ) )
        if( otherCounts.counts.exactCounts.isDefinedAt( event ) ) {
          val g_t = 
                otherCounts(event) * (
                  normalized( event ) - 1
                  // 1 - normalized( event )
                ) + 2 * kappa * apply(event)
          // println( s"$event\t${otherCounts(event)}\t${normalized(event)}\t$g_t" )
          gradient +=
            (
              event -> (
                g_t
              )
            )
        } else {
          // stepsSinceUpdate(event) += 1
          gradient +=
            (
              event -> (
                otherCounts(event) * (
                  normalized( event )
                  // 0 - normalized( event )
                ) + 2 * kappa * apply(event)
              )
            )
        }
        cachedProbs -= event
      }
    }
    gradient.foreach{ case ( event, g_t ) => updateWeight( event, g_t ) }
    stepSize *= 0.9
  }


  // modified for efficient log-linear look-ups: counts holds weights, denomCounts holds
  // sums of exponentiated weights
  override def increment( event:E, inc:Double, updateEvents:Boolean = true ) = {
    // println( " === NON-LOG SPACE INCREMENT ===" )
    val n = event.normKey

    // increment by negative exponentiated weight because decrement sets a floor at 0 (to prevent negative
    // probabilities in multinomial models)
    denomCounts.increment( n, -1 * exp( counts.exactCounts.getOrElse( event, Double.NegativeInfinity ) ), allowNegative = true )
    // denomCounts.decrement( n, exp( counts.exactCounts.getOrElse( event, Double.NegativeInfinity ) ), integerDec = false )
    counts.increment( event, inc, allowNegative = true )
    denomCounts.increment( n, exp( apply( event ) ), allowNegative = false )
    // denomCounts.increment( n, exp( inc ) )

    // denomCounts += n -> { denomCounts.getOrElse( n, 0D ) + inc }

    if( updateEvents ) {
      // denoms.getOrElseUpdate( n, MSet() ) += event
      denoms += n -> { denoms( n ) + event }
    }
  }

}

