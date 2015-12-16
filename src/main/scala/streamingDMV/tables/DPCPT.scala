package streamingDMV.tables

import streamingDMV.labels._
import streamingDMV.math._
import collection.mutable.{Map=>MMap,Set=>MSet}

// import com.twitter.algebird.CMSMonoid

import org.apache.commons.math3.special.{Gamma=>G}

import math.{log,log1p,exp,abs,floor,max}

class DPCPT[E<:Event with Product with GeneratingString](
  alpha:Double,
  baseDistribution:(String=>Double),
  randomSeed:Int = 15
) extends CPT[E](
  alpha,
  squarelyNormalized = 0,
  randomSeed = 15
) {

  override def normalized( event:E ) = {
    val n = event.normKey

    val score = 
      ( counts( event ) + alpha * baseDistribution( event.gen )  ) / (
        denomCounts( n ) + (alpha )
      )

    score
  }

  override def expDigammaNormalized( event:E ) = {
    val n = event.normKey

        // println( "numerator: " +
        //   ( counts( event ) + alpha * baseDistribution( event.gen )  ) 
        // )
        // println( "digamma( numerator ): " +
        //   G.digamma( counts( event ) + alpha * baseDistribution( event.gen )  ) 
        // )
        // println( "exp( digamma( numerator ) ): " +
        //   exp( G.digamma( counts( event ) + alpha * baseDistribution( event.gen )  ) )
        // )

        // println( "denominator: " +
        //   ( denomCounts( n ) + (alpha ) )
        // )
        // println( "digamma( denominator ): " +
        //   G.digamma( denomCounts( n ) + (alpha ) )
        // )

    var score = 
      exp( G.digamma( 
        ( counts( event ) + alpha * baseDistribution( event.gen )  ) 
      ) - G.digamma(
        denomCounts( n ) + (alpha )
      ) )

        // println( s"score: $score\n\n" )

    if( !( score > 1E-20 ) ) {
      // A hack for numerical problems with digammas of tiny probabilities from monkey model
      score = normalized( event )
    }

    if( !( score > 0 && score <= 1D + 1E-10 ) ) {
      println( s"$event\t$score" )
      println( "  " + ( counts( event ) + alpha* baseDistribution( event.gen )  ) )
      println( "  " + exp( G.digamma( ( counts( event ) + alpha * baseDistribution( event.gen ) ) ) ) )
      println( "    " + ( denomCounts( n ) + alpha ) )
      println( "    " + G.digamma( denomCounts( n ) + alpha ) )
    }
    assert( score > 0 )
    assert( score <= 1D + 1E-10 )

    score
  }

}

class LogDPCPT[E<:Event with Product with GeneratingString](
  alpha:Double,
  baseDistribution:(String=>Double),
  randomSeed:Int = 15
) extends LogCPT[E](
  alpha,
  squarelyNormalized = 0,
  randomSeed = 15
) {

  // println( "USING LogDPCPT" )

  val logAlpha = log( alpha )

  override def normalized( event:E ) = {
    val n = event.normKey

    val score = 
      LogSum( counts( event ) , logAlpha + baseDistribution( event.gen )  ) - (
        LogSum( denomCounts( n ) , logAlpha ) 
      )

    if( !( score > Double.NegativeInfinity && score <= 0D) ) {
      println( s"$event\t$score" )
      println( "  counts(event): " + counts(event) )
      println( "  log(alpha): " + logAlpha )
      println( "  numerator: " + LogSum( counts( event ) , logAlpha  ) )
      println( "  digamma(numerator): " + LogSpaceExpDigamma( LogSum( counts( event ) , logAlpha ) ) )
      println( "    denomCounts(n): " + denomCounts(n) )
      println( "    denominator: " + LogSum( denomCounts( n ) , logAlpha) )
      println( "    digamma(denominator): " + LogSpaceExpDigamma( LogSum( denomCounts( n ) , logAlpha ) ) )
    }



    score
  }

  override def expDigammaNormalized( event:E ) = {
    val n = event.normKey

        // println( event )
        // println( "numerator: " +
        //   LogSum( counts(event), logAlpha + baseDistribution( event.gen ) )
        // )
        // println( "digamma( numerator ): " +
        //   LogSpaceExpDigamma(
        //     LogSum( counts(event), logAlpha + baseDistribution( event.gen ) )
        //   )
        // )

        // println( "denominator: " +
        //   LogSum( denomCounts( n ) , logAlpha ) 
        // )
        // println( "digamma( denominator ): " +
        //   LogSpaceExpDigamma(
        //     LogSum( denomCounts( n ) , logAlpha ) 
        //   )
        // )
        // println( s"  $event ratio: " +  (
        //     LogSpaceExpDigamma(
        //       LogSum( counts(event), logAlpha + baseDistribution( event.gen ) )
        //     ) - LogSpaceExpDigamma(
        //       LogSum( denomCounts( n ) , logAlpha ) 
        //     )
        //   )
        // )

    // val score = 
    //   exp( G.digamma( 
    //     ( counts( event ) + alpha * baseDistribution( event.gen )  ) 
    //   ) ) / exp( G.digamma(
    //     denomCounts( n ) + (alpha )
    //   ) )

    var score =
      LogSpaceExpDigamma(
        LogSum( counts(event), logAlpha + baseDistribution( event.gen ) )
      ) - LogSpaceExpDigamma(
        LogSum( denomCounts( n ) , logAlpha ) 
      )

    // println( s"score: $score\n\n" )

    if( !( score > -1E100 ) ) {
      // A hack for numerical problems with digammas of tiny probabilities from monkey model
      println( "." )
      score = normalized( event )
    }

    if( !( score > Double.NegativeInfinity && score <= 0D) ) {
      println( s"$event\t$score" )
      println( "  counts(event): " + counts(event) )
      println( "  log(alpha): " + log(alpha) )
      println( "  numerator: " + LogSum( counts( event ) , log( alpha )  ) )
      println( "  digamma(numerator): " + LogSpaceExpDigamma( LogSum( counts( event ) , log( alpha ) ) ) )
      println( "    denomCounts(n): " + denomCounts(n) )
      println( "    denominator: " + LogSum( denomCounts( n ) , log(alpha * denoms(n).size)) )
      println( "    digamma(denominator): " + LogSpaceExpDigamma( LogSum( denomCounts( n ) , log(alpha * denoms(n).size ) ) ) )
    }

    assert( score > Double.NegativeInfinity )
    assert( score <= 0D )

    score
  }

}



