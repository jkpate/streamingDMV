package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

// asymmetric Dirichlet Multinomial CPT for backing off
// class CPT[E<:Event,N<:NormKey]( alpha:Double ) {
class BackoffCPT[E<:Event with BackingOffEvent]( alpha:Map[BackoffDecision,Double] ) {
  var events = MSet[E]()
  val counts = MMap[E,Double]()
  val denomCounts = MMap[NormKey,Double]()
  val denoms = MMap[NormKey,MSet[E]]()

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

        // val numerator =
        //   taylorExpDigamma( 
        //     ( counts( event ) + alpha( event.backOff )  )
        //   )

        // if( numerator == 0 ) {
        //   0D
        // } else {
        //   numerator / taylorExpDigamma(
        //     denomCounts( n ) + (alpha( event.backOff ) * denoms(n).size )
        //   )
        // }

        // println("/-------")
        // println( event )
        // println( event.backOff )
        // println( alpha.keys.mkString("\t","\n\t","\n\n") )
        // println( "\t\t" + alpha( Backoff ) )
        // println( "\t\t" + alpha( NotBackoff ) )
        // println( "numerator " +
        //   ( counts( event ) + alpha( event.backOff )  ) 
        // )

        // println( "denominator " + 
        //   ( denomCounts( n ) + (alpha( event.backOff ) * denoms(n).size ) )
        // )
        // println("\\-------")

    if( alpha( event.backOff ) == 0 )
      0
    else
      taylorExpDigamma( 
        ( counts( event ) + alpha( event.backOff )  ) 
      ) / taylorExpDigamma(
        denomCounts( n ) + alpha.values.sum
      )
  }

  def increment( event:E, inc:Double ) = {
    // counts.getOrElseUpdate( event, 0D ) += inc
    counts += event -> { counts.getOrElse( event, 0D ) + inc }

    val n = event.normKey
    // denomCounts( n ) += inc
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

