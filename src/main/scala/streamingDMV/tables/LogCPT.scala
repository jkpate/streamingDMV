package streamingDMV.tables

import streamingDMV.labels._
import streamingDMV.math._

import collection.mutable.{Map=>MMap,Set=>MSet}

// import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp}

class LogCPT[E<:Event with Product](
  alpha:Double,
  squarelyNormalized:Int = 0,
  eps:Double = 0.1,
  delta:Double = 1E-3,
  approximate:Boolean = false,
  randomSeed:Int = 15
) extends CPT[E](
  alpha,
  squarelyNormalized,
  eps,
  delta,
  approximate,
  randomSeed
) {

  // println( s"TableWrappers become LogSpace" )
  counts = new TableWrapper[E]( approximate, eps, delta, randomSeed, true )
  denomCounts = new TableWrapper[NormKey with Product]( approximate, eps, 2*delta, 37*randomSeed, true )

  assert( counts.logSpace )
  assert( denomCounts.logSpace )

  override def normalized( event:E ) = {
    val n = event.normKey
    if( squarelyNormalized  > 0  )
      LogSum( counts( event ) , log( alpha ) - log( squarelyNormalized ) ) -
        LogSum( denomCounts( n ) , log(alpha) )
    else
      LogSum( counts( event ) , log( alpha ) ) -
        LogSum( denomCounts( n ) ,
          log (alpha * denoms(n).size) )
  }

  override def expDigammaNormalized( event:E ) = {
    val n = event.normKey

    // println( s"Log Space expDigammaNormalized" )

    val score = 
      if( squarelyNormalized > 0 )
        LogSpaceExpDigamma( 
          LogSum( counts( event ) , log( alpha ) - log( squarelyNormalized ) ) 
        ) - LogSpaceExpDigamma(
          LogSum( denomCounts( n ) , log(alpha) )
        )
      else
        LogSpaceExpDigamma( 
          LogSum( counts( event ) , log( alpha )  )
        ) - LogSpaceExpDigamma(
          LogSum( denomCounts( n ) , log(alpha * denoms(n).size) )
        )

    if( !( score > Double.NegativeInfinity && score <= 0D) ) {
      println( s"$event\t$score" )
      println( "  " + LogSum( counts( event ) , log( alpha )  ) )
      println( "  " + LogSpaceExpDigamma( LogSum( counts( event ) , log( alpha ) ) ) )
      println( "    " + LogSum( denomCounts( n ) , log(alpha * denoms(n).size)) )
      println( "    " + LogSpaceExpDigamma( LogSum( denomCounts( n ) , log(alpha * denoms(n).size ) ) ) )
    }
    assert( score > Double.NegativeInfinity && score <= 0D )

    score

  }

  override def increment( event:E, inc:Double, updateEvents:Boolean = true ) = {
    // to have a faster zerosInit
    // if( inc > 0 ) counts += event -> { counts.getOrElse( event, 0D ) + inc }
    val n = event.normKey

    if( updateEvents ) {
        // if( ! denoms.keySet.contains(n) ) {
        //   denoms += n -> MSet()
        // }
        // denoms(n) += event
      // denoms.getOrElseUpdate( n, MSet() ) += event
      // denoms.getOrElseUpdate( n, MSet() ) += event
      denoms += n -> { denoms( n ) + event }
    }

    if( inc > Double.NegativeInfinity ) {
      counts.increment( event, inc )
      denomCounts.increment( n, inc )

    }

  }


      // def fastLogSpaceLogGamma( v:Double ) = {
      //             // log(3)
      //   if( v <= 1.0986122886681 ) {
      //     Double.NegativeInfinity
      //   } else {
      //     val logTerm =                 // log(2)
      //       v + LogSum( 0D, v ) + LogSum( 0.6931471805599453, v )
      //     val vp3 = LogSum( 1.0986122886681 , v )

      //     var toReturn =
      //       LogSum(
      //                           // log( 2.5 )
      //         log(vp3) + LogSum( 0.9162907318741551, v),
      //         // log( 0.0833333 )
      //         -2.4849070497880805 - vp3
      //       )

      //     toReturn = LogDifference(
      //       toReturn,
      //       LogSum(
      //           // log( 2.081061466 )
      //         0.7328780837364683,
      //         LogSum( v, log( logTerm ) )
      //       )
      //     )


      //     toReturn

      //   }
      // }

  // FIX ME BEFORE USING SQUARELYSAMPLING WITH PARTICLE FILTER
  override def trueLogProb( other:CPT[E] ) = {
    // counts should always come from sampleTreeCounts or extractPartialCounts
    // and be exact
    assert( ! other.approximate )
    assert( other.counts.logSpace )


    // other.denoms *should* be empty for CPT[ChooseEvent] if there is only one word
    if( ! other.denoms.isEmpty ) {
      other.denoms.map{ case (denom,otherEvents) =>
        val totalEvents = denoms( denom )

        val withOtherNumerator = 
          totalEvents.map{ e =>
              // println(
              //   s"$e: " + 
              //     LogSum( counts( e ),
              //       LogSum( other( e ) , log( alpha ))
              //     ) + " ; " +
              //     LogSpaceLogGamma(
              //       LogSum( counts( e ),
              //         LogSum( other( e ) , log( alpha ))
              //       )
              //     ) + s" ${ log( alpha ) } "
              // )
            LogSpaceLogGamma(
              LogSum( counts( e ),
                LogSum( other( e ) , log( alpha ))
              )
            )
          }.reduce(LogSum(_,_))

        val withOtherDenom =
          LogSpaceLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              LogSum( denomCounts( n ) ,
                LogSum( other.denomCounts( n ), log(alpha * totalEvents.size )))
            }.reduce(LogSum(_,_))
          )

        val myNumerator = 
          totalEvents.map{ e =>
            LogSpaceLogGamma( LogSum( counts( e ) , alpha ) )
          }.reduce(LogSum(_,_))

        val myDenom =
          LogSpaceLogGamma(
            totalEvents.map{ e =>
              val n = e.normKey
              LogSum( denomCounts( n ) , log(alpha * totalEvents.size ) )
            }.reduce(LogSum(_,_))
          )


          // println( s"==}> $withOtherNumerator" )
          // println( s"==}> $withOtherDenom" )
          // println( s"==}> $myNumerator" )
          // println( s"==}> $myDenom" )

          (
            withOtherNumerator - withOtherDenom
          ) - (
            myNumerator - myDenom
          )
      // }.reduce(LogSum(_,_))
      }.reduce(_+_)
    } else { 0D }
  }


  override def printOut( logSpace:Boolean = false ) {
    denoms.foreach{ case (n, events) =>
      println( s"$n:" )
      events.foreach{ e =>
        if( logSpace )
          println( s"  $e: ${counts(e)}" )
        else
          println( s"  $e: ${exp(counts(e))}" )
      }
    }
  }

}

