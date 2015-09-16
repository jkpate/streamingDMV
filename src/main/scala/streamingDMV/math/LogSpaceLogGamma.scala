package streamingDMV.math

import math.log

object LogSpaceLogGamma {
  def apply( v:Double ) = {
              // log(3)
    if( v <= 1.0986122886681 ) {
      Double.NegativeInfinity
    } else {
      val logTerm =                 // log(2)
        v + LogSum( 0D, v ) + LogSum( 0.6931471805599453, v )
      val vp3 = LogSum( 1.0986122886681 , v )

      var toReturn =
        LogSum(
                            // log( 2.5 )
          log(vp3) + LogSum( 0.9162907318741551, v),
          // log( 0.0833333 )
          -2.4849070497880805 - vp3
        )

      toReturn = LogDifference(
        toReturn,
        LogSum(
            // log( 2.081061466 )
          0.7328780837364683,
          LogSum( v, log( logTerm ) )
        )
      )


      toReturn

    }
  }
}

