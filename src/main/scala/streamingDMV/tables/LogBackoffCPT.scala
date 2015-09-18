package streamingDMV.tables

import streamingDMV.labels._
import streamingDMV.math._

import collection.mutable.{Map=>MMap,Set=>MSet}

// import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp}

class LogBackoffCPT[E<:Event with BackingOffEvent with Product](
  alpha:Map[BackoffDecision,Double],
  eps:Double = 0.1,
  delta:Double = 0.01,
  approximate:Boolean = false,
  randomSeed:Int = 15
) extends BackoffCPT[E](
  alpha,
  eps,
  delta,
  approximate,
  randomSeed
){

  counts = new TableWrapper[E]( approximate, eps, delta, randomSeed, true )
  denomCounts = new TableWrapper[NormKey with Product]( approximate, eps, delta,
    37*randomSeed, true )

  override def normalized( event:E ) = {
    val n = event.normKey
    LogSum( counts( event ) , log( alpha( event.backOff )  ) ) - LogSum (
      denomCounts( n ) , log( alpha.values.sum )
    )
  }

  override def expDigammaNormalized( event:E ) = {
    val n = event.normKey

    if( alpha( event.backOff ) == 0 )
      0
    else
      LogSpaceExpDigamma( 
        LogSum( counts( event ) , log( alpha( event.backOff )  )  )
      ) - LogSpaceExpDigamma(
        LogSum( denomCounts( n ) , log( alpha.values.sum ) )
      )
  }

  override def increment( event:E, inc:Double, updateEvents:Boolean = true ) = {
    // to have a faster zerosInit
    // if( inc > 0 ) counts += event -> { counts.getOrElse( event, 0D ) + inc }
    val n = event.normKey
    if( inc > Double.NegativeInfinity ) {
      counts.increment( event, inc )
      denomCounts.increment( n, inc )
    }

    // denomCounts += n -> { denomCounts.getOrElse( n, 0D ) + inc }

    if( updateEvents ) {
      denoms.getOrElseUpdate( n, MSet() ) += event
    }
  }

  override def trueLogProb( other:BackoffCPT[E] ) = {
    // counts should always come from sampleTreeCounts or extractPartialCounts
    // and be exact
    assert( ! other.approximate )
    assert( other.counts.logSpace )


    // other.denoms *should* be empty for CPT[ChooseEvent] if there is only one word
    if( other.denoms.isEmpty ) {
      other.denoms.map{ case (denom,otherEvents) =>
        val totalEvents = denoms( denom )

        val withOtherNumerator = 
          totalEvents.map{ e =>
            LogSpaceLogGamma(
              LogSum( counts( e ),
                LogSum( other( e ) , log( alpha( e.backOff ) ))
              )
            )
          }.reduce(LogSum(_,_))

        val withOtherDenom =
          LogSpaceLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              LogSum( denomCounts( n ) ,
                LogSum( other.denomCounts( n ), log(alpha.values.sum )))
            }.reduce(LogSum(_,_))
          )

        val myNumerator = 
          totalEvents.map{ e =>
            LogSpaceLogGamma( LogSum( counts( e ) , alpha(e.backOff) ) )
          }.reduce(LogSum(_,_))

        val myDenom =
          LogSpaceLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              LogSum( denomCounts( n ) , log(alpha.values.sum ) )
            }.reduce(LogSum(_,_))
          )

          (
            withOtherNumerator - withOtherDenom
          ) - (
            myNumerator - myDenom
          )
      }.reduce(LogSum(_,_))
    } else { 0D }
  }



}

