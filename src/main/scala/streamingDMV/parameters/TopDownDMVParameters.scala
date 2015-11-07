package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.{CPT,LogCPT}
import streamingDMV.math.LogSum

import collection.mutable.{Set=>MSet}

class TopDownDMVParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  // squarelyNormalized:Int = 0,
  // approximate:Boolean = false,
  // randomSeed:Int
  parameterSpec:ParameterSpec
) extends FirstOrderArcFactoredParameters(
  // rootAlpha,
  // stopAlpha,
  // chooseAlpha,
  // squarelyNormalized,
  // approximate,
  // randomSeed
  parameterSpec
) {

  def possibleStopEvents( h:Int ) = {
    Seq(
      StopEvent( h, LeftAtt, Outermost, Stop ),
      StopEvent( h, LeftAtt, Outermost, NotStop ),
      StopEvent( h, LeftAtt, Inner, Stop ),
      StopEvent( h, LeftAtt, Inner, NotStop ),
      StopEvent( h, RightAtt, Outermost, Stop ),
      StopEvent( h, RightAtt, Outermost, NotStop ),
      StopEvent( h, RightAtt, Inner, Stop ),
      StopEvent( h, RightAtt, Inner, NotStop )
    )
  }

      // override def harmonicCounts( miniBatch:List[Utt] ) = {
      //   // println( "TopDownDMVParameters harmonic counts" )
      //   // val rootEvents = MSet[RootEvent]()
      //   // val stopEvents = MSet[StopEvent]()
      //   // val chooseEvents = MSet[ChooseEvent]()
      //   // println( "FirstOrderArcFactoredParameters zeros init" )

      //   // p_root.clear
      //   // p_stop.clear
      //   // p_choose.clear
      //   val rootCounts =
      //     if( logSpace )
      //       new LogCPT[RootEvent](
      //         rootAlpha,
      //         squarelyNormalized,
      //         approximate = approximate,
      //         eps = 1E-6,
      //         delta = 3E-2,
      //         randomSeed = rand.nextInt
      //       )
      //     else
      //       new CPT[RootEvent](
      //         rootAlpha,
      //         squarelyNormalized,
      //         approximate = approximate,
      //         eps = 1E-6,
      //         delta = 3E-2,
      //         randomSeed = rand.nextInt
      //       )

      //     // squarelyNormalized = 2 for p_stop b/c we always want to consider both Stop and NotStop
      //   val stopCounts =
      //     if( logSpace )
      //       new LogCPT[StopEvent](
      //         stopAlpha,
      //         squarelyNormalized = 2,
      //         approximate = approximate,
      //         eps = 1E-6,
      //         delta = 3E-2,
      //         randomSeed = rand.nextInt
      //       )
      //     else
      //       new CPT[StopEvent](
      //         stopAlpha,
      //         squarelyNormalized = 2,
      //         approximate = approximate,
      //         eps = 1E-6,
      //         delta = 3E-2,
      //         randomSeed = rand.nextInt
      //       )
      //   val chooseCounts =
      //     if( logSpace )
      //       new LogCPT[ChooseEvent](
      //         chooseAlpha,
      //         squarelyNormalized,
      //         approximate = approximate,
      //         eps = 1E-7,
      //         delta = 1E-2,
      //         randomSeed = rand.nextInt
      //       )
      //     else
      //       new CPT[ChooseEvent](
      //         chooseAlpha,
      //         squarelyNormalized,
      //         approximate = approximate,
      //         eps = 1E-7,
      //         delta = 1E-2,
      //         randomSeed = rand.nextInt
      //       )

      //   if( approximate ) {
      //     rootCounts.counts.approximateCounts.initializeCountsTable
      //     rootCounts.denomCounts.approximateCounts.initializeCountsTable

      //     stopCounts.counts.approximateCounts.initializeCountsTable
      //     stopCounts.denomCounts.approximateCounts.initializeCountsTable

      //     chooseCounts.counts.approximateCounts.initializeCountsTable
      //     chooseCounts.denomCounts.approximateCounts.initializeCountsTable
      //   }

      //   val count = if( logSpace ) 0D else 1D
      //   miniBatch.map{_.string}.foreach{ s =>
      //     (0 until s.length).foreach{ t =>
      //       val h = s(t)

      //       rootCounts.increment( RootEvent( h ), count )
      //       stopCounts.increment( possibleStopEvents( h ), count )


      //       if( t == 0 ) {
      //         stopCounts.increment(
      //           StopEvent( h , LeftAtt, Outermost, Stop ),
      //           count
      //         )
      //       } else {
      //         stopCounts.increment(
      //           StopEvent( h , LeftAtt, Outermost, NotStop ),
      //           count
      //         )
      //       }
      //       if( t == 1 ) {
      //         stopCounts.increment(
      //           StopEvent( h , LeftAtt, Inner, Stop ),
      //           count
      //         )
      //       } else {
      //         stopCounts.increment(
      //           StopEvent( h , LeftAtt, Inner, NotStop ),
      //           count
      //         )
      //       }

      //       if( t == s.length-1 ) {
      //         stopCounts.increment(
      //           StopEvent( h , RightAtt, Outermost, Stop ),
      //           count
      //         )
      //       } else {
      //         stopCounts.increment(
      //           StopEvent( h , RightAtt, Outermost, NotStop ),
      //           count
      //         )
      //       }
      //       if( t == s.length-2 ) {
      //         stopCounts.increment(
      //           StopEvent( h , RightAtt, Inner, Stop ),
      //           count
      //         )
      //       } else {
      //         stopCounts.increment(
      //           StopEvent( h , RightAtt, Inner, NotStop ),
      //           count
      //         )
      //       }


      //       ( 0 until t ).foreach{ i =>
      //         val harmonicCount =
      //           if( logSpace ) 
      //             LogSum( 0D, -1 *math.log( t - i ) )
      //           else
      //             1D + (1D / ( t - i ) )

      //         chooseCounts.increment( ChooseEvent( h, LeftAtt, s(i) ), harmonicCount )

      //       }
      //       ( t+1 until s.length ).foreach{ j =>
      //         val harmonicCount =
      //           if( logSpace ) 
      //             LogSum( 0D, -1 *math.log( j - t ) )
      //           else
      //             1D + 1D / ( j - t )

      //         chooseCounts.increment( ChooseEvent( h, RightAtt, s(j) ), harmonicCount )
      //       }
      //     }
      //   }

      //   DMVCounts(
      //     rootCounts,
      //     stopCounts,
      //     chooseCounts
      //   )

      // }

}

