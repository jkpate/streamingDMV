package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

// import com.twitter.algebird.CMSMonoid

import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp}


// symmetric Dirichlet Multinomial CPT
// class CPT[E<:Event,N<:NormKey]( alpha:Double ) {
class CPT[E<:Event with Product](
  alpha:Double,
  squarelyNormalized:Int = 0,
  eps:Double = 0.1,
  delta:Double = 1E-3,
  val approximate:Boolean = false,
  randomSeed:Int = 15
) {
  // var counts = MMap[E,Double]().withDefaultValue(0D)
  var counts = new TableWrapper[E]( approximate, eps, delta, randomSeed, false )
  // var denomCounts = MMap[NormKey,Double]()
  var denomCounts = new TableWrapper[NormKey with Product]( approximate, eps,
    2*delta, 37*randomSeed )
  var denoms = MMap[NormKey,MSet[E]]()
  // var denoms = MMap[NormKey,TableWrapper[Event]]()

  def apply( event:E ) =
    counts( event )

  def normalized( event:E ) = {
    // println( s"  I am approximate: $approximate" )
    // println( s"  my counts are approximate: ${counts.approximate}" )
    val n = event.normKey
    if( squarelyNormalized  > 0  )
      ( counts( event ) + alpha/squarelyNormalized ) / (
        denomCounts( n ) + (alpha)
      )
    else
      ( counts( event ) + alpha ) / (
        denomCounts( n ) + (alpha * denoms(n).size)
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

    val score = 
      if( squarelyNormalized > 0 )
        // taylorExpDigamma( 
        exp( G.digamma( 
          ( counts( event ) + alpha/squarelyNormalized  ) 
        // ) / taylorExpDigamma(
        ) ) / exp( G.digamma(
          denomCounts( n ) + (alpha)
        ) )
      else
        // taylorExpDigamma( 
        exp( G.digamma( 
          ( counts( event ) + alpha  ) 
        // ) / taylorExpDigamma(
        ) ) / exp( G.digamma(
          denomCounts( n ) + (alpha * denoms(n).size)
        ) )

    if( !( score > 0 ) ) {
      println( s"$event\t$score" )
      // println( "  " + taylorExpDigamma( ( counts( event ) + alpha  ) ) )
      println( "  " + ( counts( event ) + alpha  ) )
      println( "  " + exp( G.digamma( ( counts( event ) + alpha  ) ) ) )
      // println( "    " + taylorExpDigamma( denomCounts( n ) + (alpha *
      println( "    " + ( denomCounts( n ) + (alpha * denoms(n).size)) )
      println( "    " + G.digamma( denomCounts( n ) + (alpha * denoms(n).size)) )
    }
    assert( score > 0 )

    score
  }

  // from https://code.google.com/p/fastapprox/source/browse/trunk/fastapprox/src/fastgamma.h
  def fastLogGamma( v:Double ) = {
    if( v < 3 ) {
      0D
    } else {
      val logTerm = log( v * ( 1 + v ) * ( 2 + v ) )
      val vp3 = 3 + v

      - 2.081061466 - v +
      0.0833333 / vp3 - logTerm +
      (2.5 + v) * log(vp3)
    }
  }

  // FIX ME BEFORE USING SQUARELYSAMPLING WITH PARTICLE FILTER
  def trueLogProb( other:CPT[E] ) = {
    // counts should always come from sampleTreeCounts or extractPartialCounts
    // and be exact
    assert( ! other.approximate )


    // other.denoms *should* be empty for CPT[ChooseEvent] if there is only one word
    if( other.denoms.isEmpty ) {
      other.denoms.map{ case (denom,otherEvents) =>
        val totalEvents = denoms( denom )
        // otherEvents.foreach{ e => assert( totalEvents.contains( e ) ) }

        // val eventsUnion = ( otherEvents ++ myEvents )
        val withOtherNumerator = 
          totalEvents.map{ e =>
            // G.logGamma( counts( e ) + other( e ) + alpha )
            fastLogGamma( counts( e ) + other( e ) + alpha )
          }.reduce(_+_)

        val withOtherDenom =
          // G.logGamma(
          fastLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              denomCounts( n ) + other.denomCounts( n ) + (alpha * totalEvents.size )
            }.sum
          )

        val myNumerator = 
          totalEvents.map{ e =>
            // G.logGamma( counts( e ) + alpha )
            fastLogGamma( counts( e ) + alpha )
          }.reduce(_+_)

        val myDenom =
          // G.logGamma(
          fastLogGamma(
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
    } else { 0D }
  }

  def increment( event:E, inc:Double ) = {
    // to have a faster zerosInit
    // if( inc > 0 ) counts += event -> { counts.getOrElse( event, 0D ) + inc }
    println( " === NON-LOG SPACE INCREMENT ===" )
    val n = event.normKey
    if( inc > 0 ) {
      counts.increment( event, inc )
      denomCounts.increment( n, inc )
    }

    // denomCounts += n -> { denomCounts.getOrElse( n, 0D ) + inc }

    denoms.getOrElseUpdate( n, MSet() ) += event
  }

  def increment( events:Seq[E], inc:Double ) {
    events.foreach{ increment( _, inc ) }
  }

  def increment( other:CPT[E] ) {
    // println( other.counts.approximate )
    // other.counts.foreach{ case( k, v) =>
    //   increment( k, v )
    // }
    counts.increment( other.counts )
    denomCounts.increment( other.denomCounts )
  }

  def decrement( other:CPT[E] ) {
    assert( counts.logSpace == other.counts.logSpace )
    assert( denomCounts.logSpace == other.denomCounts.logSpace )
    // other.counts.foreach{ case( k, v) =>
    //   decrement( k, v )
    // }
    counts.decrement( other.counts )
    denomCounts.decrement( other.denomCounts )
  }

  def divideBy( x:Double ) {
    // counts.keys.foreach{ counts(_) /= x }
    counts.divideBy( x )
    denomCounts.divideBy( x )
  }

  def decrement( event:E, dec:Double ) = {
    // counts( event ) -= dec
    counts.decrement( event , dec )

    val n = event.normKey
    // denomCounts( n ) -= dec
    denomCounts.decrement( n , dec )
  }

  def clear {
    counts.clear
    denomCounts.clear
    denoms.clear
  }

  def setEvents( events:Set[E] ) {
    clear
    events.groupBy( _.normKey ).foreach{ case (n, events) =>
      // counts ++= events.toSeq.map{ e =>
      //   e -> 0D
      // }
      // denomCounts += n -> 0D
      // denomCounts.increment( n, 0D )
      denoms += n -> MSet( events.toSeq:_* )
    }
  }

  def setEvents( other:CPT[E] ) {
    assert( counts.logSpace == other.counts.logSpace )
    assert( denomCounts.logSpace == other.denomCounts.logSpace )
    clear
    denoms = other.denoms//.clone
    denomCounts = other.denomCounts//.clone
    // events = other.events.clone
  }

  def setEventsAndCounts( other:CPT[E] ) {
    clear
    // TODO make it so we don't need denoms *at all*
    //      will involve hard-coding backoff chain with e.g. .noContext() method
    //      Except then how do we compute trueLogProb?
    //      Aha! denomCounts never changes -- take it as a parameter from
    //      ParticleFilterNOPOSParser.
    denoms = other.denoms//.clone
    // events = other.events.clone

    // denomCounts.increment( other.denomCounts.clone )
    // counts.increment( other.counts.clone )

    println( s"setting logSpace counts: ${other.counts.logSpace}" )

    denomCounts.setCounts( other.denomCounts )
    counts.setCounts( other.counts )
  }

  def randomizeCounts( r:util.Random, scale:Int ) {
    // counts.keys.foreach( increment( _, r.nextDouble() * scale ) )
    counts.randomize( r, scale )
  }

  def size = denoms.values.map{_.size}.sum

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

