package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.math.LogSum
import streamingDMV.tables.{CPT,BackoffCPT}


trait BackoffIndepDepsParameters extends FirstOrderArcFactoredParameters {

  // BackoffCPT doesn't support logspace yet
  assert( !logSpace )


  val lambda_stop = new BackoffCPT[LambdaStopEvent](
    alpha = Map(
      Backoff -> backoffAlpha,
      NotBackoff -> notBackoffAlpha
    ),
    approximate = false
  )
  val lambda_choose = new BackoffCPT[LambdaChooseEvent](
    alpha = Map(
      Backoff -> backoffAlpha,
      NotBackoff -> notBackoffAlpha
    ),
    approximate = false
  )

  override def apply( r:RootEvent ) = {
    val RootEvent( rInt, _, a ) = r
    if( fullyNormalized )
      myTimes(
        p_root.normalized( RootEvent( rInt ) ) ,
          p_root.normalized( RootEvent( a ) ) 
      )
    else
      myTimes(
        p_root.expDigammaNormalized( RootEvent( rInt ) ) ,
          p_root.expDigammaNormalized( RootEvent( a ) ) 
      )
  }

  override def apply( s:StopEvent ) = {
    val StopEvent( h, hAnn, dir, v, dec ) = s
    val boKey =
        LambdaStopEvent( h, hAnn, dir, v, Backoff )
    val notBOKey =
        LambdaStopEvent( h, hAnn, dir, v, NotBackoff )

    if( fullyNormalized )
      myPlus(
        myTimes(
          lambda_stop.normalized( boKey ),
            p_stop.normalized( StopEvent( h, dir, v, dec ) ) 
        ),
        myTimes(
          lambda_stop.normalized( notBOKey ),
            p_stop.normalized( StopEvent( h, hAnn, dir, v, dec ) )
        )
      )
    else
      myPlus(
        myTimes(
          lambda_stop.expDigammaNormalized( boKey ),
            p_stop.expDigammaNormalized( StopEvent( h, dir, v, dec ) ) 
        ),
        myTimes(
          lambda_stop.expDigammaNormalized( notBOKey ),
            p_stop.expDigammaNormalized( StopEvent( h, hAnn, dir, v, dec ) )
        )
      )
  }

  override def apply( c:ChooseEvent ) = {
    val ChooseEvent( h, _, hAnn, _, dir, d, _, dAnn ) = c

    val boKey =
      LambdaChooseEvent( h, hAnn, dir, Backoff )
    val notBOKey =
      LambdaChooseEvent( h, hAnn, dir, NotBackoff )

    if( fullyNormalized )
      myPlus(
        myTimes(
          lambda_choose.normalized( boKey ),
            p_choose.normalized( ChooseEvent( h, "", -1D, -1, dir, d, "", -1D ) ),
            p_choose.normalized( ChooseEvent( h, "", -1D, -1, annotDir( dir ), -1, "", dAnn ) )
        ),
        myTimes(
          lambda_choose.normalized( notBOKey ),
            p_choose.normalized( ChooseEvent( h, "", hAnn, -1, dir, d, "", -1D ) ),
            p_choose.normalized( ChooseEvent( h, "", hAnn, -1, annotDir( dir ), -1, "", dAnn ) )
        )
      )
    else
      myPlus(
        myTimes(
          lambda_choose.expDigammaNormalized( boKey ),
            p_choose.expDigammaNormalized( ChooseEvent( h, "", -1D, -1, dir, d, "", -1D ) ),
            p_choose.expDigammaNormalized( ChooseEvent( h, "", -1D, -1, annotDir( dir ), -1, "", dAnn ) )
        ),
        myTimes(
          lambda_choose.expDigammaNormalized( notBOKey ),
            p_choose.expDigammaNormalized( ChooseEvent( h, "", hAnn, -1, dir, d, "", -1D ) ),
            p_choose.expDigammaNormalized( ChooseEvent( h, "", hAnn, -1, annotDir( dir ), -1, "", dAnn ) )
        )
      )
  }

  def annotatedRootEvents( r:Int, rAnn:Double ) =
    Seq[Event]( RootEvent( r, "", -1D ), RootEvent( -1, "", rAnn ) )

  def possibleAnnotatedStopEvents( h:Int, hAnn:Double ) = {
    possibleStopEvents( h ).map{ case StopEvent( h, _, dir, v, dec ) =>
      StopEvent( h, hAnn, dir, v, dec )
    }
  }
  // Cheap hack -- just replace Stop with Backoff and NotStop with NotBackoff.
  // There's no real correspondence here, I just don't want to remove duplicates to get just the h,
  // ann, dir, and v
  def possibleLambdaStopEvents( h:Int, hAnn:Double ) = {
    possibleStopEvents( h ).map{ case StopEvent( h, _, dir, v, dec ) =>
      dec match {
        case Stop => LambdaStopEvent( h, hAnn, dir, v, Backoff )
        case NotStop => LambdaStopEvent( h, hAnn, dir, v, NotBackoff )
      }
    }
  }

  // External counts is used when we need to get choose events that are not decomposed according to
  // our backoff structure. It was introduced for the harmonic counts method, which returns a
  // DMVCounts object.
  def annotatedChooseEvents( h:Int, hObs:String, hAnn:Double, dir:AttDir, d:Int, dObs:String,
    dAnn:Double, externalCounts:Boolean = false ) = {
    if( externalCounts ) {
      Seq[ChooseEvent]( ChooseEvent( h, hObs, hAnn, -1, dir, d, dObs, dAnn ) )
    } else {
      dir match {
        case LeftAtt =>
          Seq[ChooseEvent](
            ChooseEvent( h, hObs, hAnn, -1, LeftAtt, d, dObs, -1D ),
            ChooseEvent( h, hObs, hAnn, -1, LeftAnnotAtt, -1, dObs, dAnn ),
            ChooseEvent( h, hObs, -1D, -1, LeftAtt, d, dObs, -1D ),
            ChooseEvent( h, hObs, -1D, -1, LeftAnnotAtt, -1, dObs, dAnn )
          )
        case RightAtt =>
          Seq[ChooseEvent](
            ChooseEvent( h, hObs, hAnn, -1, RightAtt, d, dObs, -1D ),
            ChooseEvent( h, hObs, hAnn, -1, RightAnnotAtt, -1, dObs, dAnn ),
            ChooseEvent( h, hObs, -1D, -1, RightAtt, d, dObs, -1D ),
            ChooseEvent( h, hObs, -1D, -1, RightAnnotAtt, -1, dObs, dAnn )
          )
            // case LeftAtt =>
            //   Seq[ChooseEvent](
            //     ChooseEvent( h,"",  hAnn, -1, LeftAtt, d, "", Double.NaN ),
            //     ChooseEvent( h,"",  hAnn, -1, LeftAnnotAtt, -1,  "", dAnn ),
            //     ChooseEvent( h,"", Double.NaN, -1,  LeftAtt, d, "", Double.NaN),
            //     ChooseEvent( h,"", Double.NaN,  -1, LeftAnnotAtt, -1,  "", dAnn )
            //   )
            // case RightAtt =>
            //   Seq[ChooseEvent](
            //     ChooseEvent( h,"",  hAnn, -1, RightAtt, d, "", Double.NaN ),
            //     ChooseEvent( h,"",  hAnn, -1, RightAnnotAtt,  -1, "", dAnn ),
            //     ChooseEvent( h,"", Double.NaN, -1,  RightAtt, d, "", Double.NaN),
            //     ChooseEvent( h,"", Double.NaN,  -1, RightAnnotAtt,  -1, "", dAnn )
            //   )
      }
    }
  }

  def lambdaChooseEvents( h:Int, hAnn:Double, dir:AttDir ) = {
    Seq[LambdaChooseEvent](
      LambdaChooseEvent( h, hAnn, dir, Backoff ),
      LambdaChooseEvent( h, hAnn, dir, NotBackoff )
    )
  }

  override def zerosInit( corpus:List[Utt] ) {

    println( "FirstOrderBackoffIndepDepsParameters zeros init" )

    p_root.clear
    p_stop.clear
    lambda_stop.clear
    p_choose.clear
    lambda_choose.clear

    val windowSize = math.ceil( corpus.size.toDouble/20 )
    var i = 1
    corpus.foreach{ case Utt( id, s, w, a ) =>
      // println( s"zeros init for $id: ${s.mkString("[ ", ", ", " ]")}  ${a.mkString("[ ", ", ", " ]")}" )
      (0 until s.length).foreach{ t =>
        val h = s(t)
        val hW = if( w.length > 0 ) { w(t) } else { "" }
        val hAnn = if( a.length > 0 ) { a(t) } else { -1D }

        annotatedRootEvents(h, hAnn).foreach{ rEvent =>
          rEvent match {
            case rootEvent:RootEvent => p_root.increment( rootEvent, myZero, updateEvents = true )
            case chooseEvent:ChooseEvent => p_choose.increment( chooseEvent, myZero, updateEvents = true)
          }
        }
        p_stop.increment( possibleAnnotatedStopEvents( h, hAnn ), myZero, updateEvents = true )

        lambda_stop.increment( possibleLambdaStopEvents( h, hAnn ), myZero, updateEvents = true )
        lambdaChooseEvents( h, hAnn, LeftAtt ).foreach{ e =>
          lambda_choose.increment( e, myZero, updateEvents = true )
        }

        ( 0 until t ).foreach{ i =>
          val d= s(i)
          val dW = if( w.isEmpty ) "" else w(i)
          val dAnn = if( a.length > 0 ) { a(i) } else { -1D }
          annotatedChooseEvents( h, hW, hAnn, LeftAtt, d, dW, dAnn ).foreach{ e =>
            // println( s"adding $e:" )
            p_choose.increment( e, myZero, updateEvents = true )
          }

        }
        ( t+1 until s.length ).foreach{ j =>
          val d= s(j)
          val dW = if( w.isEmpty ) "" else w(j)
          val dAnn = if( a.length > 0 ) { a(j) } else { -1D }
          annotatedChooseEvents( h, hW, hAnn, RightAtt, d, dW, dAnn ).foreach{ e =>
            p_choose.increment( e, myZero, updateEvents = true )
          }
        }
      }

      if( i %windowSize == 0 ) {
        println( s"$i/${corpus.size} processed" )
      }
      i += 1
    }

  }

  // This looks a bit weird, because we're not getting "harmonic counts" for the lambda terms. We
  // aren't getting them because the lambda terms are reconstructed from the DMV-style counts.
  // Instead, we're overriding because we need to include the annotations and because of the
  // independence assumption -- we need to generate root and choose words and annotations
  // independently.
  override def harmonicCounts( minibatch:List[Utt] ) = {

    val rootCounts =
        new CPT[RootEvent](
          rootAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )

    val stopCounts =
        new CPT[StopEvent](
          stopAlpha,
          squarelyNormalized = 2,
          approximate = approximate,
          eps = 1E-6,
          delta = 3E-2,
          randomSeed = rand.nextInt
        )

    val chooseCounts =
        new CPT[ChooseEvent](
          chooseAlpha,
          squarelyNormalized,
          approximate = approximate,
          eps = 1E-7,
          delta = 1E-2,
          randomSeed = rand.nextInt
        )


    val count = if( logSpace ) 0D else 1D
    // val windowSize = math.ceil( minibatch.size.toDouble/20 )
    // var i = 1
    minibatch.foreach{ case Utt( _, s, w, a ) =>
      (0 until s.length).foreach{ t =>
        val h = s(t)
        val hW = if( w.length > 0 ) { w(t) } else { "" }
        val hAnn = if( a.length > 0 ) { a(t) } else { -1D }

        annotatedRootEvents(h, hAnn).foreach{ rEvent =>
          rEvent match {
            case rootEvent:RootEvent => rootCounts.increment( rootEvent, count )
            case chooseEvent:ChooseEvent => chooseCounts.increment( chooseEvent, count )
          }
        }
        stopCounts.increment( possibleAnnotatedStopEvents( h, hAnn ), count, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          val harmonicCount =
            if( logSpace ) 
              LogSum( 0D, -1 *math.log( t - i ) )
            else
              1D + (1D / ( t - i ) )
          val d= s(i)
          val dW = if( w.isEmpty ) "" else w(i)
          val dAnn = if( a.length > 0 ) { a(i) } else { -1D }
          annotatedChooseEvents( h, hW, hAnn, LeftAtt, d, dW, dAnn, true ).foreach{ e =>
            chooseCounts.increment( e, harmonicCount, updateEvents = true )
          }

        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount =
            if( logSpace ) 
              LogSum( 0D, -1 *math.log( j - t ) )
            else
              1D + ( 1D / ( j - t ) )
          val d= s(j)
          val dW = if( w.isEmpty ) "" else w(j)
          val dAnn = if( a.length > 0 ) { a(j) } else { -1D }
          annotatedChooseEvents( h, hW, hAnn, RightAtt, d, dW, dAnn, true ).foreach{ e =>
            // println( ">> " + e )
            chooseCounts.increment( e, harmonicCount, updateEvents = true )
          }
        }
      }

      // if( i %windowSize == 0 ) {
      //   println( s"$i/${minibatch.size} processed" )
      // }
      // i += 1
    }

    DMVCounts(
      rootCounts,
      stopCounts,
      chooseCounts
    )

  }

  def annotDir( dir:AttDir ) =
    dir match {
      case LeftAtt => LeftAnnotAtt
      case RightAtt => RightAnnotAtt
    }


  override def incrementCounts( counts:DMVCounts, updateEvents:Boolean = true ) {
    // println( "FirstOrderBackoffIndepDepsParameters.incrementCounts received:" )
    // counts.printTotalCountsByType

    // Don't need to decompose root counts into backed-off and non-backedoff counts
    p_root.increment( counts.rootCounts, updateEvents )


    // need to ensure that lambda_stop does not change until after counts are separated
    val separateLambdaStop = new BackoffCPT[LambdaStopEvent](
      alpha = Map(
        Backoff -> backoffAlpha,
        NotBackoff -> notBackoffAlpha
      ),
      approximate = false
    )

    counts.stopCounts.events.foreach{ fullKey =>
      val StopEvent( h, hAnn, dir, v, dec )  = fullKey
      val eventBOKey = StopEvent( h, dir, v, dec )

      val boKey = LambdaStopEvent( h, hAnn, dir, v, Backoff )
      val notBOKey = LambdaStopEvent( h, hAnn, dir, v, NotBackoff )


      val count = counts.stopCounts( fullKey )

      if( fullyNormalized ) {
        val boCount = myTimes( lambda_stop.normalized( boKey ) , count )
        val notBOCount = myTimes( lambda_stop.normalized( notBOKey ) , count )
        p_stop.increment( eventBOKey, boCount, updateEvents = updateEvents )
        p_stop.increment( fullKey, notBOCount, updateEvents = updateEvents )

        separateLambdaStop.increment( boKey, boCount, updateEvents = updateEvents )
        separateLambdaStop.increment( notBOKey, notBOCount, updateEvents = updateEvents )
      } else {
        // We have the marginal of the arc, but we need to break it down to the part due to back-off
        // and the part not due to back-off. The variational scores sub-normalize, so to get
        // *counts*, we need to re-normalize the variational scores to get the proper attribution.
        // I did not need to do this in the code for my dissertation because I explicitly broke down
        // the counts during the outside pass (so the normalization term was implicit in outside and
        // inside scores)
        val boScore = lambda_stop.expDigammaNormalized( boKey )
        val notBOScore = lambda_stop.expDigammaNormalized( notBOKey )
        val scoreSum = boScore + notBOScore

        val boProp = boScore / scoreSum
        val notBOProp = notBOScore / scoreSum

        val boCount = myTimes( boProp , count )
        val notBOCount = myTimes( notBOProp , count )

        p_stop.increment( eventBOKey, boCount, updateEvents = updateEvents )
        p_stop.increment( fullKey, notBOCount, updateEvents = updateEvents )

        separateLambdaStop.increment( boKey, boCount, updateEvents = updateEvents )
        separateLambdaStop.increment( notBOKey, notBOCount, updateEvents = updateEvents )
      }

    }

    lambda_stop.increment( separateLambdaStop, updateEvents = updateEvents )
    // lambda_stop.printOut()

    // need to ensure that lambda_choose does not change until after counts are separated
    val separateLambdaChoose = new BackoffCPT[LambdaChooseEvent](
      alpha = Map(
        Backoff -> backoffAlpha,
        NotBackoff -> notBackoffAlpha
      ),
      approximate = false
    )

    counts.chooseCounts.events.foreach{ fullKey =>
      val ChooseEvent( h, _, hAnn, _, dir, d, _, dAnn ) = fullKey

      val boChooseLex = ChooseEvent( h, "", -1D, -1, dir, d, "", -1D )
      val boChooseAnn = ChooseEvent( h, "", -1D, -1, annotDir( dir ), -1, "", dAnn )
      val fullChooseLex = ChooseEvent( h, "", hAnn, -1, dir, d, "", -1D )
      val fullChooseAnn = ChooseEvent( h, "", hAnn, -1, annotDir( dir ), -1, "", dAnn )

      val boKey = LambdaChooseEvent( h, hAnn, dir, Backoff )
      val notBOKey = LambdaChooseEvent( h, hAnn, dir, NotBackoff )


      val count = counts.chooseCounts( fullKey )

      if( fullyNormalized ) {
        val boCount = myTimes( lambda_choose.normalized( boKey ) , count )
        val notBOCount = myTimes( lambda_choose.normalized( notBOKey ) , count )

        p_choose.increment( boChooseLex, boCount, updateEvents = updateEvents )
        p_choose.increment( boChooseAnn, boCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseLex, notBOCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseAnn, notBOCount, updateEvents = updateEvents )

        separateLambdaChoose.increment( boKey, boCount, updateEvents = updateEvents )
        separateLambdaChoose.increment( notBOKey, notBOCount, updateEvents = updateEvents )
      } else {
        // We have the marginal of the arc, but we need to break it down to the part due to back-off
        // and the part not due to back-off. The variational scores sub-normalize, so to get
        // *counts*, we need to re-normalize the variational scores to get the proper attribution.
        // I did not need to do this in the code for my dissertation because I explicitly broke down
        // the counts during the outside pass (so the normalization term was implicit in outside and
        // inside scores)

        val boScore = lambda_choose.expDigammaNormalized( boKey )
        val notBOScore = lambda_choose.expDigammaNormalized( notBOKey )
        val scoreSum = boScore + notBOScore

        val boProp = boScore / scoreSum
        val notBOProp = notBOScore / scoreSum

        val boCount = myTimes( boProp , count )
        val notBOCount = myTimes( notBOProp , count )

        p_choose.increment( boChooseLex, boCount, updateEvents = updateEvents )
        p_choose.increment( boChooseAnn, boCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseLex, notBOCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseAnn, notBOCount, updateEvents = updateEvents )

        separateLambdaChoose.increment( boKey, boCount, updateEvents = updateEvents )
        separateLambdaChoose.increment( notBOKey, notBOCount, updateEvents = updateEvents )
      }

    }

    lambda_choose.increment( separateLambdaChoose, updateEvents = updateEvents )
    // lambda_choose.printOut()
  }

  override def decrementCounts( counts:DMVCounts, integerDec:Boolean ) {
    // Don't need to decompose root counts into backed-off and non-backedoff counts
    p_root.decrement( counts.rootCounts, integerDec )


    // need to ensure that lambda_stop does not change until after counts are separated
    val separateLambdaStop = new BackoffCPT[LambdaStopEvent](
      alpha = Map(
        Backoff -> backoffAlpha,
        NotBackoff -> notBackoffAlpha
      ),
      approximate = false
    )

    counts.stopCounts.events.foreach{ fullKey =>
      val StopEvent( h, hAnn, dir, v, dec )  = fullKey
      val eventBOKey = StopEvent( h, dir, v, dec )

      val boKey = LambdaStopEvent( h, hAnn, dir, v, Backoff )
      val notBOKey = LambdaStopEvent( h, hAnn, dir, v, NotBackoff )


      val count = counts.stopCounts( fullKey )

      if( fullyNormalized ) {
        val boCount = myTimes( lambda_stop.normalized( boKey ) , count )
        val notBOCount = myTimes( lambda_stop.normalized( notBOKey ) , count )

        p_stop.decrement( eventBOKey, boCount, integerDec = integerDec )
        p_stop.decrement( fullKey, notBOCount, integerDec = integerDec )

        separateLambdaStop.decrement( boKey, boCount, integerDec = integerDec )
        separateLambdaStop.decrement( notBOKey, notBOCount, integerDec = integerDec )
      } else {
        // We have the marginal of the arc, but we need to break it down to the part due to back-off
        // and the part not due to back-off. The variational scores sub-normalize, so to get
        // *counts*, we need to re-normalize the variational scores to get the proper attribution.
        // I did not need to do this in the code for my dissertation because I explicitly broke down
        // the counts during the outside pass (so the normalization term was implicit in outside and
        // inside scores)
        val boScore = lambda_stop.expDigammaNormalized( boKey )
        val notBOScore = lambda_stop.expDigammaNormalized( notBOKey )
        val scoreSum = boScore + notBOScore

        val boProp = boScore / scoreSum
        val notBOProp = notBOScore / scoreSum

        val boCount = myTimes( boProp , count )
        val notBOCount = myTimes( notBOProp , count )

        p_stop.decrement( eventBOKey, boCount, integerDec = integerDec )
        p_stop.decrement( fullKey, notBOCount, integerDec = integerDec )

        separateLambdaStop.decrement( boKey, boCount, integerDec = integerDec )
        separateLambdaStop.decrement( notBOKey, notBOCount, integerDec = integerDec )
      }

    }

    lambda_stop.decrement( separateLambdaStop, integerDec = integerDec )

    // need to ensure that lambda_choose does not change until after counts are separated
    val separateLambdaChoose = new BackoffCPT[LambdaChooseEvent](
      alpha = Map(
        Backoff -> backoffAlpha,
        NotBackoff -> notBackoffAlpha
      ),
      approximate = false
    )

    counts.chooseCounts.events.foreach{ fullKey =>
      val ChooseEvent( h, _, hAnn, _, dir, d, _, dAnn ) = fullKey

      val boChooseLex = ChooseEvent( h, "", -1D, -1, dir, d, "", -1D )
      val boChooseAnn = ChooseEvent( h, "", -1D, -1, annotDir( dir ), -1, "", dAnn )
      val fullChooseLex = ChooseEvent( h, "", hAnn, -1, dir, d, "", -1D )
      val fullChooseAnn = ChooseEvent( h, "", hAnn, -1, annotDir( dir ), -1, "", dAnn )

      val boKey = LambdaChooseEvent( h, hAnn, dir, Backoff )
      val notBOKey = LambdaChooseEvent( h, hAnn, dir, NotBackoff )


      val count = counts.chooseCounts( fullKey )

      if( fullyNormalized ) {
        val boCount = myTimes( lambda_choose.normalized( boKey ) , count )
        val notBOCount = myTimes( lambda_choose.normalized( notBOKey ) , count )

        p_choose.decrement( boChooseLex, boCount, integerDec = integerDec )
        p_choose.decrement( boChooseAnn, boCount, integerDec = integerDec )

        p_choose.decrement( fullChooseLex, notBOCount, integerDec = integerDec )
        p_choose.decrement( fullChooseAnn, notBOCount, integerDec = integerDec )

        separateLambdaChoose.decrement( boKey, boCount, integerDec = integerDec )
        separateLambdaChoose.decrement( notBOKey, notBOCount, integerDec = integerDec )
      } else {
        // We have the marginal of the arc, but we need to break it down to the part due to back-off
        // and the part not due to back-off. The variational scores sub-normalize, so to get
        // *counts*, we need to re-normalize the variational scores to get the proper attribution.
        // I did not need to do this in the code for my dissertation because I explicitly broke down
        // the counts during the outside pass (so the normalization term was implicit in outside and
        // inside scores)

        val boScore = lambda_choose.expDigammaNormalized( boKey )
        val notBOScore = lambda_choose.expDigammaNormalized( notBOKey )
        val scoreSum = boScore + notBOScore

        val boProp = boScore / scoreSum
        val notBOProp = notBOScore / scoreSum

        val boCount = myTimes( boProp , count )
        val notBOCount = myTimes( notBOProp , count )

        p_choose.decrement( boChooseLex, boCount, integerDec = integerDec )
        p_choose.decrement( boChooseAnn, boCount, integerDec = integerDec )

        p_choose.decrement( fullChooseLex, notBOCount, integerDec = integerDec )
        p_choose.decrement( fullChooseAnn, notBOCount, integerDec = integerDec )

        separateLambdaChoose.decrement( boKey, boCount, integerDec = integerDec )
        separateLambdaChoose.decrement( notBOKey, notBOCount, integerDec = integerDec )
      }

    }

    lambda_choose.decrement( separateLambdaChoose, integerDec = integerDec )
  }

}



