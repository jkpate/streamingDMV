package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

import org.apache.commons.math3.special.{Gamma=>G}


// symmetric Dirichlet Multinomial CPT
// class CPT[E<:Event,N<:NormKey]( alpha:Double ) {
class CPT[E<:Event]( alpha:Double ) {
  var events = MSet[E]()
  var counts = MMap[E,Double]().withDefaultValue(0D)
  var denomCounts = MMap[NormKey,Double]()
  var denoms = MMap[NormKey,MSet[E]]()

  def apply( event:E ) =
    counts( event )

  def normalized( event:E ) = {
    val n = event.normKey
    ( counts( event ) + alpha ) / (
      denomCounts( n ) + (alpha * denoms(n).size )
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
    taylorExpDigamma( 
      ( counts( event ) + alpha  ) 
    ) / taylorExpDigamma(
      denomCounts( n ) + (alpha * denoms(n).size )
    )
  }

  def trueLogProb( other:CPT[E] ) = {
    other.denoms.map{ case (denom,otherEvents) =>
      val totalEvents = denoms( denom )
      // otherEvents.foreach{ e => assert( totalEvents.contains( e ) ) }

      // val eventsUnion = ( otherEvents ++ myEvents )
      val withOtherNumerator = 
        totalEvents.map{ e =>
          G.logGamma( counts( e ) + other( e ) + alpha )
        }.reduce(_+_)

      val withOtherDenom =
        G.logGamma(
          totalEvents.map{ e =>
            val n = e.normKey
            denomCounts( n ) + other.denomCounts( n ) + (alpha * totalEvents.size )
          }.sum
        )

      val myNumerator = 
        totalEvents.map{ e =>
          G.logGamma( counts( e ) + alpha )
        }.reduce(_+_)

      val myDenom =
        G.logGamma(
          totalEvents.map{ e =>
            val n = e.normKey
            denomCounts( n ) + (alpha * totalEvents.size )
          }.sum
        )

      // println(
      //   s"($withOtherNumerator}/$withOtherDenom) / ($myNumerator/$myDenom)"
      // )

      // math.exp(
        (
          withOtherNumerator - withOtherDenom
        ) - (
          myNumerator - myDenom
        )
      // )

    }.reduce(_+_)
  }

  def increment( event:E, inc:Double ) = {
    // to have a faster zerosInit
    if( inc > 0 ) counts += event -> { counts.getOrElse( event, 0D ) + inc }

    val n = event.normKey
    denomCounts +=
      n -> { denomCounts.getOrElse( n, 0D ) + inc }

    denoms.getOrElseUpdate( n, MSet() ) += event
  }

  def increment( events:Seq[E], inc:Double ) {
    events.foreach{ increment( _, inc ) }
  }

  def increment( other:CPT[E] ) {
    other.counts.foreach{ case( k, v) =>
      increment( k, v )
    }
  }

  def decrement( other:CPT[E] ) {
    other.counts.foreach{ case( k, v) =>
      decrement( k, v )
    }
  }

  def divideBy( x:Double ) {
    counts.keys.foreach{ counts(_) /= x }
  }

  def decrement( event:E, dec:Double ) = {
    counts( event ) -= dec
    val n = event.normKey
    denomCounts( n ) -= dec
  }

  def clear {
    counts.clear
    denomCounts.clear
    denoms.clear
  }

  def setEvents( events:Set[E] ) {
    clear
    events.groupBy( _.normKey ).foreach{ case (n, events) =>
      counts ++= events.toSeq.map{ e =>
        e -> 0D
      }
      denomCounts += n -> 0D
      denoms += n -> MSet( events.toSeq:_* )
    }
  }

  def setEvents( other:CPT[E] ) {
    clear
    denoms = other.denoms
    denomCounts = other.denomCounts
    events = other.events
  }

  def setEventsAndCounts( other:CPT[E] ) {
    clear
    denoms = other.denoms
    denomCounts = other.denomCounts
    events = other.events
    counts = other.counts
  }

  def randomizeCounts( r:util.Random, scale:Int ) {
    counts.keys.foreach( increment( _, r.nextDouble() * scale ) )
  }

  def size = counts.size

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

