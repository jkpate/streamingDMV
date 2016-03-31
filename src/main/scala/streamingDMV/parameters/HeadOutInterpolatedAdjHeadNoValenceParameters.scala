package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables._

import collection.mutable.{Set=>MSet}

import math.exp

class HeadOutInterpolatedAdjHeadNoValenceParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  // backoffAlpha:Double,
  // notBackoffAlpha:Double,
  // squarelyNormalized:Int = 0,
  // approximate:Boolean = false,
  // randomSeed:Int
  parameterSpec:ParameterSpec
// ) extends NOPOSArcFactoredParameters(
) extends ArcFactoredParameters[BackoffChooseDMVCounts](
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha/*,
  // squarelyNormalized,
  // approximate,
  // randomSeed*/
  parameterSpec
) {

  val rand = new util.Random( randomSeed )

  val backoffAlpha = parameterSpec.backoffAlpha
  val notBackoffAlpha = parameterSpec.notBackoffAlpha

  val p_root =
    if( logSpace )
      new LogCPT[RootEvent](
        rootAlpha,
        squarelyNormalized,
        approximate = approximate,
        eps = 1E-6,
        delta = 3E-2,
        randomSeed = rand.nextInt
      )
    else
      new CPT[RootEvent](
        rootAlpha,
        squarelyNormalized,
        approximate = approximate,
        eps = 1E-6,
        delta = 3E-2,
        randomSeed = rand.nextInt
      )

    // squarelyNormalized = 2 for p_stop b/c we always want to consider both Stop and NotStop
  val p_stop =
    if( logSpace )
      new LogCPT[StopEvent](
        stopAlpha,
        squarelyNormalized = 2,
        approximate = approximate,
        eps = 1E-6,
        delta = 3E-2,
        randomSeed = rand.nextInt
      )
    else
      new CPT[StopEvent](
        stopAlpha,
        squarelyNormalized = 2,
        approximate = approximate,
        eps = 1E-6,
        delta = 3E-2,
        randomSeed = rand.nextInt
      )

  val p_choose =
    if( logSpace )
      new LogCPT[ChooseEvent](
        chooseAlpha,
        squarelyNormalized,
        approximate = approximate,
        eps = 1E-7,
        delta = 1E-2,
        randomSeed = rand.nextInt
      )
    else
      new CPT[ChooseEvent](
        chooseAlpha,
        squarelyNormalized,
        approximate = approximate,
        eps = 1E-7,
        delta = 1E-2,
        randomSeed = rand.nextInt
      )

  val lambda_choose =
    if( logSpace )
      new LogBackoffCPT[LambdaChooseEvent](
        Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
      )
    else
      new BackoffCPT[LambdaChooseEvent](
        Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
      )

  def cacheLGammas {
    throw new UnsupportedOperationException( "particle filter for backoff model not implemented!" )
  }

  def toCounts = {
    // val c =
    BackoffChooseDMVCounts(
        p_root,
        p_stop,
        p_choose,
        lambda_choose
      )

    // // println( c.chooseCounts.denoms.size + " choose LHS in toCounts" )
    // println( "in toCounts " )
    // c.printTotalCountsByType

    // c
  }

  def randomizeCounts( seed:Int, scale:Int ) {
    val r = new util.Random( seed )
    p_root.randomizeCounts( r, scale )
    p_stop.randomizeCounts( r, scale )
    p_choose.randomizeCounts( r, scale )
    lambda_choose.randomizeCounts( r, scale )
  }

  def apply( l:LambdaChooseEvent ) = {
    if( fullyNormalized )
      lambda_choose.normalized( l )
    else
      lambda_choose.expDigammaNormalized( l )
  }

  def apply( r:RootEvent ) = {
    if( fullyNormalized )
      p_root.normalized( r )
    else
      p_root.expDigammaNormalized( r )
  }

  def apply( s:StopEvent ) = {
    if( fullyNormalized )
      p_stop.normalized( s )
    else
      p_stop.expDigammaNormalized( s )
  }

  def apply( c:ChooseEvent ) = {
    if( fullyNormalized )
      p_choose.normalized( c )
    else
      p_choose.expDigammaNormalized( c )
  }

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, NoValence, Stop ),
      StopEvent( h, LeftAtt, NoValence, NotStop ),
      StopEvent( h, RightAtt, NoValence, Stop ),
      StopEvent( h, RightAtt, NoValence, NotStop )
    )
  }

  def zerosInit( corpus:List[Utt] ) {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
    // p_root.clear
    // p_stop.clear
    // p_choose.clear
    // lambda_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        p_root.increment( RootEvent( h ), myZero )
        p_stop.increment( possibleStopEvents( h ), myZero, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), myZero )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), myZero )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), LeftAtt, Backoff ), myZero )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), LeftAtt, NotBackoff ), myZero )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), myZero )

          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), myZero )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), RightAtt, Backoff ), myZero )
            lambda_choose.increment( LambdaChooseEvent( h, s(k), RightAtt, NotBackoff ), myZero )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

  }

  def harmonicCounts( corpus:List[Utt] ) = {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
    // p_root.clear
    // p_stop.clear
    // p_choose.clear

    val rootCounts =
      if( logSpace )
        new LogCPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )

      // squarelyNormalized = 2 for p_stop b/c we always want to consider both Stop and NotStop
    val stopCounts =
      if( logSpace )
        new LogCPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
    val chooseCounts =
      if( logSpace )
        new LogCPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )

    val chooseLambdaCounts =
      if( logSpace )
        new LogBackoffCPT[LambdaChooseEvent](
          Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
        )
      else
        new BackoffCPT[LambdaChooseEvent](
          Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
        )


    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // p_root.increment( RootEvent( h ), myZero )
        // p_stop.increment( possibleStopEvents( h ), myZero )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              -1 *math.log( i - t )
            else
              1D / ( i - t )

          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), harmonicCount )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              -1 *math.log( j - t )
            else
              1D / ( j - t )

          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), harmonicCount )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

    BackoffChooseDMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts,
      chooseLambdaCounts
    )
  }

  def randomCounts( corpus:List[Utt] ) = {
    // don't use CPT.setEvents because .groupBy is too slow when we have trigrams
    // p_root.clear
    // p_stop.clear
    // p_choose.clear

    val rootCounts =
      if( logSpace )
        new LogCPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )

      // squarelyNormalized = 2 for p_stop b/c we always want to consider both Stop and NotStop
    val stopCounts =
      if( logSpace )
        new LogCPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )
    val chooseCounts =
      if( logSpace )
        new LogCPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )
      else
        new CPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )

    val chooseLambdaCounts =
      if( logSpace )
        new LogBackoffCPT[LambdaChooseEvent](
          Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
        )
      else
        new BackoffCPT[LambdaChooseEvent](
          Map( Backoff -> backoffAlpha, NotBackoff -> notBackoffAlpha )
        )


    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        // p_root.increment( RootEvent( h ), myZero )
        // p_stop.increment( possibleStopEvents( h ), myZero )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              math.log( rand.nextDouble )
            else
              rand.nextDouble

          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

          ((i+1) until t).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), LeftAtt, s(i) ), harmonicCount )
          }
        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              -1 *math.log( j - t )
            else
              1D / ( j - t )

          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
          ((t+1) until j).foreach{ k =>
            p_choose.increment( ChooseEvent( h, s(k), RightAtt, s(j) ), harmonicCount )
          }
        }
      }
    }
    println( s"${p_choose.size} choose rules" )

    BackoffChooseDMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts,
      chooseLambdaCounts
    )


  }

  def incrementCounts( counts:BackoffChooseDMVCounts, updateEvents:Boolean = true ) {
    p_root.increment( counts.rootCounts, updateEvents )
    p_stop.increment( counts.stopCounts , updateEvents )
    p_choose.increment( counts.chooseCounts , updateEvents )
    lambda_choose.increment( counts.lambdaChooseCounts , updateEvents )


  }

  def decrementCounts( counts:BackoffChooseDMVCounts, integerDec:Boolean ) {
    p_root.decrement( counts.rootCounts, integerDec )
    p_stop.decrement( counts.stopCounts, integerDec )
    p_choose.decrement( counts.chooseCounts, integerDec )
    lambda_choose.decrement( counts.lambdaChooseCounts, integerDec )

  }

  def setEvents( counts:BackoffChooseDMVCounts ) {
    p_root.setEvents( counts.rootCounts )
    p_stop.setEvents( counts.stopCounts )
    p_choose.setEvents( counts.chooseCounts )
    lambda_choose.setEvents( counts.lambdaChooseCounts )
  }

  def setEventsAndCounts( counts:BackoffChooseDMVCounts ) {
    p_root.setEventsAndCounts( counts.rootCounts )
    p_stop.setEventsAndCounts( counts.stopCounts )
    p_choose.setEventsAndCounts( counts.chooseCounts )
    lambda_choose.setEventsAndCounts( counts.lambdaChooseCounts )
  }

  def printTotalCountsByType {
    if( logSpace ) {
      println( s"  > ${p_root.counts.values.map{exp(_)}.sum} root events" )
      println( s"  > ${p_root.denomCounts.values.map{exp(_)}.sum} root denom events" )
      println( s"  > ${p_stop.counts.values.map{exp(_)}.sum} stop events" )
      println( s"  > ${p_stop.denomCounts.values.map{exp(_)}.sum} stop denom events" )
      println( s"  > ${p_choose.counts.values.map{exp(_)}.sum} choose events" )
      println( s"  > ${p_choose.denomCounts.values.map{exp(_)}.sum} choose denom events" )
      println( s"  > ${p_choose.denoms.size} choose LHS" )
      println( s"  > ${lambda_choose.counts.values.map{exp(_)}.sum} choose events" )
      println( s"  > ${lambda_choose.denomCounts.values.map{exp(_)}.sum} choose denom events" )
      println( s"  > ${lambda_choose.denoms.size} choose LHS" )
    } else {
      println( s"  > ${p_root.counts.values.sum} root events" )
      println( s"  > ${p_root.denomCounts.values.sum} root denom events" )
      println( s"  > ${p_stop.counts.values.sum} stop events" )
      println( s"  > ${p_stop.denomCounts.values.sum} stop denom events" )
      println( s"  > ${p_choose.counts.values.sum} choose events" )
      println( s"  > ${p_choose.denomCounts.values.sum} choose denom events" )
      println( s"  > ${p_choose.denoms.size} choose LHS" )
      println( s"  > ${lambda_choose.counts.values.sum} choose events" )
      println( s"  > ${lambda_choose.denomCounts.values.sum} choose denom events" )
      println( s"  > ${lambda_choose.denoms.size} choose LHS" )
    }
  }

  def printOut( logSpace:Boolean = false ) {
    println( "p_root:" )
    p_root.printOut( logSpace )
    println( "p_stop:" )
    p_stop.printOut( logSpace )
    println( "p_choose:" )
    p_choose.printOut( logSpace )
    println( "lambda_choose:" )
    lambda_choose.printOut( logSpace )
  }


}

