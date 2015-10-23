package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

// import com.twitter.algebird.CMSMonoid

import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp,abs}
import streamingDMV.math.LogSum


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
  var counts = new TableWrapper[E](
    approximate = approximate,
    eps = eps,
    delta = delta,
    randomSeed = randomSeed,
    logSpace = false
  )
  // var denomCounts = MMap[NormKey,Double]()
  var denomCounts =
    new TableWrapper[NormKey with Product](
      approximate,
      eps,
      2*delta,
      37*randomSeed,
      logSpace = false
    )
  var denoms = Map[NormKey,Set[E]]().withDefaultValue(Set())
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
        exp(
          G.digamma( 
            ( counts( event ) + alpha/squarelyNormalized  ) 
          ) - G.digamma(
            denomCounts( n ) + (alpha)
          )
        )
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

      // def validateCPT = {
      //   val result = denoms.forall{ case ( n, events ) =>
      //     if( !( 
      //         abs(
      //           denomCounts(n) - events.map(counts(_)).sum
      //         ) < 0.0001
      //       )
      //     ) {
      //       println( events.map{e => s"$e: ${counts(e)}" }.mkString("[\n\t","\n\t","\n]\n") )
      //       println( events.map{counts(_)}.sum )
      //       println( n )
      //       println( denomCounts(n) )
      //     }

      //     abs( denomCounts(n) - events.map(counts(_)).sum ) < 0.0001
      //   }

      //   assert( result )
      // }

  // FIX ME BEFORE USING SQUARELYSAMPLING WITH PARTICLE FILTER
  def trueLogProb( other:CPT[E] ) = {
    // counts should always come from sampleTreeCounts or extractPartialCounts
    // and be exact
    assert( ! other.approximate )
    assert( ! approximate )


    // other.denoms *should* be empty for CPT[ChooseEvent] if there is only one word
    if( !other.denoms.isEmpty ) {
          // val withOther = other.denoms.map{ case (denom,otherEvents) =>
          //   val totalEvents = denoms( denom )
          //   // otherEvents.foreach{ e => assert( totalEvents.contains( e ) ) }

          //   // val eventsUnion = ( otherEvents ++ myEvents )
          //   val withOtherNumerator = 
          //     totalEvents.map{ e =>
          //       assert( other(e) >= 0 )
          //       G.logGamma( counts( e ) + other( e ) + alpha )
          //       // fastLogGamma( counts( e ) + other( e ) + alpha )
          //     }.reduce(_+_)

          //   val withOtherDenom =
          //     G.logGamma(
          //     // fastLogGamma(
          //       totalEvents.map{ e =>
          //         val n = e.normKey
          //         denomCounts( n ) + other.denomCounts( n ) + (alpha * totalEvents.size )
          //       // }.reduce(LogSum(_,_))
          //       }.sum
          //     )

          //   // println(
          //   //   s"($withOtherNumerator}/$withOtherDenom) / ($myNumerator/$myDenom)"
          //   // )

          //   // math.exp(

          //   withOtherNumerator - withOtherDenom

          // }.reduce(_+_)

          // val withoutOther = other.denoms.map{ case (denom,otherEvents) =>
          //   val totalEvents = denoms( denom )

          //   val myNumerator = 
          //     totalEvents.map{ e =>
          //       G.logGamma( counts( e ) + alpha )
          //       // fastLogGamma( counts( e ) + alpha )
          //     }.reduce(_+_)

          //   val myDenom =
          //     G.logGamma(
          //     // fastLogGamma(
          //       totalEvents.map{ e =>
          //         val n = e.normKey
          //         assert( other.denomCounts(n) >= 0 )
          //         denomCounts( n ) + (alpha * totalEvents.size )
          //       // }.reduce(LogSum(_,_))
          //       }.sum
          //     )

          //   myNumerator - myDenom

          // }.reduce(_+_)

          // withOther - withoutOther




      other.denoms.keys.toVector.map{ denom =>
        val totalEvents = denoms( denom ).toVector

        // var withOtherSum = 0D
        val withOtherNumerator = totalEvents.map{ e =>
          // withOtherSum += counts( e ) + other( e ) + alpha
          // G.logGamma( counts( e ) + other( e ) + alpha )
          fastLogGamma( counts( e ) + other( e ) + alpha )
        }.sum

        // var withOtherDenomSum = 0D
        val withOtherDenom =
          // G.logGamma(
          fastLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              // withOtherDenomSum += denomCounts( n ) + other.denomCounts(n) + (alpha * totalEvents.size )
              denomCounts( n ) + other.denomCounts(n)  + (alpha * totalEvents.size )
            }.sum
          )

        // var withoutOtherSum = 0D
        val myNumerator = totalEvents.map{ e =>
          // withoutOtherSum += counts( e ) + alpha
          // G.logGamma( counts( e ) + alpha )
          fastLogGamma( counts( e ) + alpha )
        }.sum

        var withoutOtherDenomSum = 0D
        val myDenom =
          // G.logGamma(
          fastLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              // withoutOtherDenomSum += denomCounts( n ) + ( alpha * totalEvents.size )
              denomCounts( n ) + ( alpha * totalEvents.size )
            }.sum
          )


        // val trueLogProbFactor = 
        //   ( withOtherNumerator + myDenom ) - ( myNumerator + withOtherDenom )
        val trueLogProbFactor = 
          ( withOtherNumerator - withOtherDenom ) - ( myNumerator - myDenom )

        if( ! ( trueLogProbFactor <= 0D & trueLogProbFactor > Double.NegativeInfinity ) ) {
          println( "  IN TRUE LOG PROB" )

          other.printOut()

          println( s"  withOtherNumerator: $withOtherNumerator" )
          println( s"  withOtherDenom: $withOtherDenom" )
          println( s"  myNumerator: $myNumerator" )
          println( s"  myDenom: $myDenom" )
          // println( s"  withOtherSum: $withOtherSum" )
          // println( s"  withOtherDenomSum: $withOtherDenomSum" )
          // println( s"  withoutOtherSum: $withoutOtherSum" )
          // println( s"  withoutOtherDenomSum: $withoutOtherDenomSum" )
          println( trueLogProbFactor )

        }


        trueLogProbFactor

      }.sum
    } else { 0D }
  }

  def increment( event:E, inc:Double, updateEvents:Boolean = true ) = {
    // to have a faster zerosInit
    // if( inc > 0 ) counts += event -> { counts.getOrElse( event, 0D ) + inc }
    // println( " === NON-LOG SPACE INCREMENT ===" )
    val n = event.normKey
    if( inc > 0 ) {
      counts.increment( event, inc )
      denomCounts.increment( n, inc )
    }

    // denomCounts += n -> { denomCounts.getOrElse( n, 0D ) + inc }

    if( updateEvents ) {
      // denoms.getOrElseUpdate( n, MSet() ) += event
      denoms += n -> { denoms( n ) + event }
    }
  }

  def increment( events:Seq[E], inc:Double ) {
    events.foreach{ increment( _, inc ) }
  }

  def increment( other:CPT[E], updateEvents:Boolean ) {
    // println( other.counts.approximate )
    other.denoms.flatMap{_._2}.foreach{ event =>
      // val inc = other(event)
      // val beforeIncCount = counts( event )
      // val beforeIncDenomCount = denomCounts( event.normKey )
      increment( event, other(event), updateEvents )

      // assert( counts( event ) - inc == beforeIncCount )
      // assert( denomCounts( event.normKey ) - inc == beforeIncDenomCount )
    }
    // validateCPT
    // println( "SUCCESS" )
    // other.
    // counts.increment( other.counts )
    // denomCounts.increment( other.denomCounts )
  }

  def decrement( other:CPT[E], integerDec:Boolean = false ) {
    assert( counts.logSpace == other.counts.logSpace )
    assert( denomCounts.logSpace == other.denomCounts.logSpace )
      // other.counts.foreach{ case( k, v) =>
      //   decrement( k, v )
      // }

      // other.denoms.foreach{ case (n, otherEvents ) =>
      //   val eventsSum = other.denoms(n).map( counts(_) ).sum
      //   if( ! ( abs( other.denomCounts(n) - eventsSum ) < 0.00001 ) ) {
      //     println( s"$n" )
      //     println( s"  count: ${other.denoms(n).size}" )
      //     println( s"  sum: $eventsSum" )
      //     println( s"  denom: ${other.denomCounts(n)}" )
      //   }
      //   assert( abs( other.denomCounts(n) - eventsSum ) < 0.00001 )
      // }
      // other.denoms.foreach{ case (n, otherEvents ) =>
      //   val eventsSum = denoms(n).map( counts(_) ).sum
      //   if( ! ( abs( denomCounts(n) - eventsSum ) < 0.00001 ) ) {
      //     println( s"$n" )
      //     println( s"  count: ${denoms(n).size}" )
      //     println( s"  sum: $eventsSum" )
      //     println( s"  denom: ${denomCounts(n)}" )
      //   }
      //   assert( abs( denomCounts(n) - eventsSum ) < 0.00001 )
      // }

    counts.decrement( other.counts, integerDec )
    denomCounts.decrement( other.denomCounts, integerDec )

      // other.denoms.foreach{ case (n, otherEvents ) =>
      //   val eventsSum = denoms(n).map( counts(_) ).sum
      //   if( ! ( abs( denomCounts(n) - eventsSum ) < 0.00001 ) ) {
      //     println( s"$n" )
      //     println( s"  count: ${denoms(n).size}" )
      //     println( s"  sum: $eventsSum" )
      //     println( s"  denom: ${denomCounts(n)}" )
      //   }
      //   assert( abs( denomCounts(n) - eventsSum ) < 0.00001 )
      // }

    // validateCPT

  }

  def divideBy( x:Double ) {
    // counts.keys.foreach{ counts(_) /= x }
    counts.divideBy( x )
    denomCounts.divideBy( x )
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
    // denoms.clear
    denoms = Map().withDefaultValue( Set() )
  }

  def setEvents( events:Set[E] ) {
    clear
    events.groupBy( _.normKey ).foreach{ case (n, events) =>
      // counts ++= events.toSeq.map{ e =>
      //   e -> 0D
      // }
      // denomCounts += n -> 0D
      // denomCounts.increment( n, 0D )
      // denoms += n -> MSet( events.toSeq:_* )
      denoms += n -> Set( events.toSeq:_* )
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
    // println( s"    before clear other has ${other.denoms.values.map{_.size}.sum} rules" )
    clear
    // println( s"    after clear other has ${other.denoms.values.map{_.size}.sum} rules" )
    // TODO make it so we don't need denoms *at all*
    //      will involve hard-coding backoff chain with e.g. .noContext() method
    //      Except then how do we compute trueLogProb?
    //      Aha! denomCounts never changes -- take it as a parameter from
    //      ParticleFilterNOPOSParser.
    denoms = other.denoms//.clone
    // println( s"    I now has ${denoms.values.map{_.size}.sum} rules" )
    if( ! other.denoms.isEmpty ) assert( !denoms.isEmpty )
    // events = other.events.clone

    // denomCounts.increment( other.denomCounts.clone )
    // counts.increment( other.counts.clone )

    // println( s"setting logSpace counts: ${other.counts.logSpace}" )
    // println( other.counts.values.map{exp(_)}.sum + " events" )
    // println( other.denoms.values.map{_.size}.sum + " rules" )

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

