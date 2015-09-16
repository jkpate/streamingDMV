package streamingDMV.math

import math.{exp,log1p}

object LogSum {

  def apply( a:Double, b:Double ):Double =
    if( a == Double.NegativeInfinity )
      b
    else if( b == Double.NegativeInfinity )
      a
    else if( b < a )
      a + log1p( exp( b - a ) )
    else
      b + log1p( exp( a - b ) )

}

object LogDifference {

  def apply( a:Double, b:Double ):Double = {
    if( b-a > 1E-10 ) {
      println( s"a: $a" )
      println( s"b: $b" )
      assert( math.abs(a-b) <= 1E-10 )
    }
    if( b == Double.NegativeInfinity )
      a
    // else if ( b - a < 1E-10 )
    //   Double.NegativeInfinity
    else
      a + log1p( -1 * exp( b - a ) )
  }

}

