package streamingDMV.math


import scala.math.{log,exp}

object LogSpaceExpDigamma {
  def apply( input:Double ) = {
    var r = 0D
    var x = exp( input )
    while( x <= 5 ) {
      r -= 1/x
      x += 1
    }
    val f = 1/(x*x)
    val t = f*(-1D/12.0 + f*(1D/120.0 + f*(-1D/252.0 + f*(1/240.0 + f*(-1/132.0
        + f*(691/32760.0 + f*(-1/12.0 + f*3617/8160.0)))))));
    r + log(x) - 0.5/x + t;
  }
}

