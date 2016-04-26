package streamingDMV.tables

import streamingDMV.labels._

import org.apache.commons.math3.special.{Gamma=>G}

import scala.collection.mutable.{Map=>MMap,Set=>MSet}

// asymmetric Dirichlet Multinomial CPT for backing off
// class CPT[E<:Event,N<:NormKey]( alpha:Double ) {
class BackoffCPT[E<:Event with BackingOffEvent with Product](
  alpha:Map[BackoffDecision,Double],
  eps:Double = 0.1,
  delta:Double = 0.01,
  val approximate:Boolean = false,
  randomSeed:Int = 15
) {
  var counts = new TableWrapper[E]( approximate, eps, delta, randomSeed )
  var denomCounts = new TableWrapper[NormKey with Product]( approximate, eps, delta, 37*randomSeed )
  var denoms = MMap[NormKey,MSet[E]]()

  def totalCounts = denomCounts.values.sum

  var fullyNormalized:Boolean = false

  def apply( event:E ) =
    counts( event )

  def normalized( event:E ) = {
    val n = event.normKey
    ( counts( event ) + alpha( event.backOff )  ) / (
      denomCounts( n ) + alpha.values.sum
    )
  }

  def taylorExpDigamma( v:Double ) = {
    // Mark's fast digamma approximation
    // return v-0.5 if v > 1.0 else 0.5*v*v
    if( v >= 1.0 )
      v-0.5+0.061/v
    else
      v*v*(0.561 + 0.8*(v-0.5)*(v-1.0))
  }

  def expDigammaNormalized( event:E ) = {
    val n = event.normKey

    if( alpha( event.backOff ) == 0 )
      0
    else
      taylorExpDigamma( 
        ( counts( event ) + alpha( event.backOff )  ) 
      ) / taylorExpDigamma(
        denomCounts( n ) + alpha.values.sum
      )
  }

  def trueLogProb( other:BackoffCPT[E] ) = {
    other.denoms.map{ case (denom,otherEvents) =>
      val totalEvents = denoms( denom )

      assert( totalEvents.size == 2 )
      val withOtherNumerator = 
        totalEvents.map{ e =>
          G.logGamma( counts( e ) + other( e ) + alpha( e.backOff ) )
        }.reduce(_+_)

      val withOtherDenom =
        G.logGamma(
          totalEvents.map{ e =>
            counts( e ) + other( e ) + (alpha.values.sum )
          }.sum
        )

      val myNumerator = 
        totalEvents.map{ e =>
          G.logGamma( counts( e ) + alpha( e.backOff ) )
        }.reduce(_+_)

      val myDenom =
        G.logGamma(
          totalEvents.map{ e =>
            counts( e ) + (alpha.values.sum )
          }.sum
        )

      (
        withOtherNumerator - withOtherDenom
      ) - (
        myNumerator - myDenom
      )

    }.reduce(_+_)
  }

  def increment( event:E, inc:Double, updateEvents:Boolean = true ) = {
    val n = event.normKey
    if( inc > 0 ) {
      counts.increment( event, inc )
      denomCounts.increment( n, inc )
    }

    if( updateEvents ) {
      denoms.getOrElseUpdate( n, MSet() ) += event
    }
  }

  def increment( events:Seq[E], inc:Double, updateEvents:Boolean ) {
    events.foreach{ increment( _, inc, updateEvents ) }
  }

  def increment( other:BackoffCPT[E], updateEvents:Boolean ) {
    // other.counts.foreach{ case( k, v) =>
    //   increment( k, v )
    // }
    counts.increment( other.counts )
    denomCounts.increment( other.denomCounts )
    other.denoms.flatMap{_._2}.foreach{ event =>
      increment( event, other(event), updateEvents )
    }
  }

  def decrement( other:BackoffCPT[E], integerDec:Boolean = false ) {
    // other.counts.foreach{ case( k, v) =>
    //   decrement( k, v )
    // }
    counts.decrement( other.counts, integerDec )
    denomCounts.decrement( other.denomCounts, integerDec )
  }

  def divideBy( x:Double ) {
    // counts.keys.foreach{ counts(_) /= x }
    counts.divideBy( x )
    denomCounts.divideBy( x )
  }

  def multiplyBy( x:Double ) {
    // counts.keys.foreach{ counts(_) /= x }
    counts.multiplyBy( x )
    denomCounts.multiplyBy( x )
  }

  def decrement( event:E, dec:Double, integerDec:Boolean ) = {
    // counts( event ) -= dec
    counts.decrement( event , dec, integerDec )

    val n = event.normKey
    // denomCounts( n ) -= dec
    denomCounts.decrement( n , dec, integerDec )
  }

  def clear {
    counts.clear
    denomCounts.clear
    denoms.clear
  }

  def setEvents( other:BackoffCPT[E] ) {
    clear
    denoms = other.denoms.clone
    denomCounts = other.denomCounts.clone
    // events = other.events.clone
  }

  def setEventsAndCounts( other:BackoffCPT[E] ) {
    clear
    denoms = other.denoms.clone
    denomCounts = other.denomCounts.clone
    // events = other.events.clone
    counts = other.counts.clone
  }


  def setEvents( events:Set[E] ) {
    clear
    events.groupBy( _.normKey ).foreach{ case (n, events) =>
      // counts ++= events.toSeq.map{ e =>
      //   e -> 0D
      // }
      // denomCounts += n -> 0D
      denomCounts.increment( n, 0D )
      denoms += n -> MSet( events.toSeq:_* )
    }
  }

  def randomizeCounts( r:util.Random, scale:Int ) {
    counts.keys.foreach( increment( _, r.nextDouble() * scale ) )
  }

  def printOut( logSpace:Boolean = false ) {
    denoms.foreach{ case (n, events) =>
      println( s"$n:" )
      events.foreach{ e =>
        if( logSpace )
          println( s"  $e: ${math.log(counts(e))}" )
        else
          println( s"  $e: ${counts(e)}" )
      }
    }
  }



}

