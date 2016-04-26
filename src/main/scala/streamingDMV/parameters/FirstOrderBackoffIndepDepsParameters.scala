package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.{CPT,BackoffCPT}


trait BackoffIndepDepsParameters extends FirstOrderArcFactoredParameters {

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
      p_root.normalized( RootEvent( rInt ) ) * 
        p_root.normalized( RootEvent( a ) ) 
    else
      p_root.expDigammaNormalized( RootEvent( rInt ) ) * 
        p_root.expDigammaNormalized( RootEvent( a ) ) 
  }

  override def apply( s:StopEvent ) = {
    val StopEvent( h, hAnn, dir, v, dec ) = s
    val boKey =
        LambdaStopEvent( h, hAnn, dir, v, Backoff )
    val notBOKey =
        LambdaStopEvent( h, hAnn, dir, v, NotBackoff )

    if( fullyNormalized )
      (
        lambda_stop.normalized( boKey ) *
          p_stop.normalized( StopEvent( h, dir, v, dec ) ) 
      ) + (
        lambda_stop.normalized( notBOKey ) *
          p_stop.normalized( StopEvent( h, hAnn, dir, v, dec ) )
      )
    else
      (
        lambda_stop.expDigammaNormalized( boKey ) *
          p_stop.expDigammaNormalized( StopEvent( h, dir, v, dec ) ) 
      ) + (
        lambda_stop.expDigammaNormalized( notBOKey ) *
          p_stop.expDigammaNormalized( StopEvent( h, hAnn, dir, v, dec ) )
      )
  }

  override def apply( c:ChooseEvent ) = {
    val ChooseEvent( h, _, hAnn, _, dir, d, _, dAnn ) = c

    val boKey =
      LambdaChooseEvent( h, hAnn, dir, Backoff )
    val notBOKey =
      LambdaChooseEvent( h, hAnn, dir, NotBackoff )

    if( fullyNormalized )
      (
        lambda_choose.normalized( boKey ) *
          p_choose.normalized( ChooseEvent( h, dir, d ) ) *
          p_choose.normalized( ChooseEvent( h, dir, dAnn ) )
      ) + (
        lambda_choose.normalized( notBOKey ) *
          p_choose.normalized( ChooseEvent( h, hAnn, dir, d ) ) *
          p_choose.normalized( ChooseEvent( h, hAnn, dir, dAnn ) )
      )
    else
      (
        lambda_choose.expDigammaNormalized( boKey ) *
          p_choose.expDigammaNormalized( ChooseEvent( h, dir, d ) ) *
          p_choose.expDigammaNormalized( ChooseEvent( h, dir, dAnn ) )
      ) + (
        lambda_choose.expDigammaNormalized( notBOKey ) *
          p_choose.expDigammaNormalized( ChooseEvent( h, hAnn, dir, d ) ) *
          p_choose.expDigammaNormalized( ChooseEvent( h, hAnn, dir, dAnn ) )
      )
  }

  def annotatedRootEvents( r:Int, rAnn:Double ) =
    Seq[Event]( RootEvent( r ), RootEvent( rAnn ) )

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

  def annotatedChooseEvents( h:Int, hObs:String, hAnn:Double, dir:AttDir, d:Int, dObs:String,
    dAnn:Double ) = {
    Seq[ChooseEvent](
      ChooseEvent( h, hAnn, dir, d ),
      ChooseEvent( h, hAnn, dir, dAnn ),
      ChooseEvent( h, dir, d),
      ChooseEvent( h, dir, dAnn )
    )
  }

  def lambdaChooseEvents( h:Int, hAnn:Double, dir:AttDir ) = {
    Seq[LambdaChooseEvent](
      LambdaChooseEvent( h, hAnn, dir, Backoff ),
      LambdaChooseEvent( h, hAnn, dir, NotBackoff )
    )
  }

  override def zerosInit( corpus:List[Utt] ) {

    p_root.clear
    p_stop.clear
    lambda_stop.clear
    p_choose.clear
    lambda_choose.clear

    val windowSize = math.ceil( corpus.size.toDouble/20 )
    var i = 1
    corpus.foreach{ case Utt( _, s, w, a ) =>
      (0 until s.length).foreach{ t =>
        val h = s(t)
        val hW = if( w.length > 0 ) { w(t) } else { "" }
        val hAnn = if( a.length > 0 ) { a(t) } else { Double.NaN }

        annotatedRootEvents(h, hAnn).foreach{ rEvent =>
          rEvent match {
            case rootEvent:RootEvent => p_root.increment( rootEvent, myZero )
            case chooseEvent:ChooseEvent => p_choose.increment( chooseEvent, myZero )
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
          val dAnn = if( a.length > 0 ) { a(i) } else { Double.NaN }
          annotatedChooseEvents( h, hW, hAnn, LeftAtt, d, dW, dAnn ).foreach{ e =>
            p_choose.increment( e, myZero, updateEvents = true )
          }

        }
        ( t+1 until s.length ).foreach{ j =>
          val d= s(j)
          val dW = if( w.isEmpty ) "" else w(j)
          val dAnn = if( a.length > 0 ) { a(j) } else { Double.NaN }
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
        val hAnn = if( a.length > 0 ) { a(t) } else { Double.NaN }

        annotatedRootEvents(h, hAnn).foreach{ rEvent =>
          rEvent match {
            case rootEvent:RootEvent => rootCounts.increment( rootEvent, count )
            case chooseEvent:ChooseEvent => chooseCounts.increment( chooseEvent, count )
          }
        }
        stopCounts.increment( possibleAnnotatedStopEvents( h, hAnn ), count, updateEvents = true )

        ( 0 until t ).foreach{ i =>
          val harmonicCount = 1D + (1D / ( t - i ) )
          val d= s(i)
          val dW = if( w.isEmpty ) "" else w(i)
          val dAnn = if( a.length > 0 ) { a(i) } else { Double.NaN }
          annotatedChooseEvents( h, hW, hAnn, LeftAtt, d, dW, dAnn ).foreach{ e =>
            chooseCounts.increment( e, harmonicCount, updateEvents = true )
          }

        }
        ( t+1 until s.length ).foreach{ j =>
          val harmonicCount = 1D + (1D / ( j - t ) )
          val d= s(j)
          val dW = if( w.isEmpty ) "" else w(j)
          val dAnn = if( a.length > 0 ) { a(j) } else { Double.NaN }
          annotatedChooseEvents( h, hW, hAnn, RightAtt, d, dW, dAnn ).foreach{ e =>
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



  override def incrementCounts( counts:DMVCounts, updateEvents:Boolean = true ) {
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
        val boCount = lambda_stop.normalized( boKey ) * count
        val notBOCount = lambda_stop.normalized( notBOKey ) * count
        p_stop.increment( eventBOKey, boCount, updateEvents = updateEvents )
        p_stop.increment( fullKey, notBOCount, updateEvents = updateEvents )
        separateLambdaStop.increment( boKey, boCount, updateEvents = updateEvents )
        separateLambdaStop.increment( notBOKey, notBOCount, updateEvents = updateEvents )
      } else {
        val boCount = lambda_stop.expDigammaNormalized( boKey ) * count
        val notBOCount = lambda_stop.expDigammaNormalized( notBOKey ) * count
        p_stop.increment( eventBOKey, boCount, updateEvents = updateEvents )
        p_stop.increment( fullKey, notBOCount, updateEvents = updateEvents )
        separateLambdaStop.increment( boKey, boCount )
        separateLambdaStop.increment( notBOKey, notBOCount )
      }

    }

    lambda_stop.increment( separateLambdaStop, updateEvents = updateEvents )

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

      val boChooseLex = ChooseEvent( h, dir, d )
      val boChooseAnn = ChooseEvent( h, dir, dAnn )
      val fullChooseLex = ChooseEvent( h, hAnn, dir, d )
      val fullChooseAnn = ChooseEvent( h, hAnn, dir, dAnn )

      val boKey = LambdaChooseEvent( h, hAnn, dir, Backoff )
      val notBOKey = LambdaChooseEvent( h, hAnn, dir, NotBackoff )


      val count = counts.chooseCounts( fullKey )

      if( fullyNormalized ) {
        val boCount = lambda_choose.normalized( boKey ) * count
        val notBOCount = lambda_choose.normalized( notBOKey ) * count

        p_choose.increment( boChooseLex, boCount, updateEvents = updateEvents )
        p_choose.increment( boChooseAnn, boCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseLex, notBOCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseAnn, notBOCount, updateEvents = updateEvents )

        separateLambdaChoose.increment( boKey, boCount )
        separateLambdaChoose.increment( notBOKey, notBOCount )
      } else {
        val boCount = lambda_choose.expDigammaNormalized( boKey ) * count
        val notBOCount = lambda_choose.expDigammaNormalized( notBOKey ) * count

        p_choose.increment( boChooseLex, boCount, updateEvents = updateEvents )
        p_choose.increment( boChooseAnn, boCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseLex, notBOCount, updateEvents = updateEvents )
        p_choose.increment( fullChooseAnn, notBOCount, updateEvents = updateEvents )

        separateLambdaChoose.increment( boKey, boCount )
        separateLambdaChoose.increment( notBOKey, notBOCount )
      }

    }

    lambda_choose.increment( separateLambdaChoose, updateEvents = updateEvents )
  }

}



