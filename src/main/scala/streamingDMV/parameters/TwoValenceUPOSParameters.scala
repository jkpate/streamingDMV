package streamingDMV.parameters

import streamingDMV.labels._

import breeze.linalg._
import breeze.numerics._


class TwoValenceUPOSParameters(
  // rootAlpha:Double,
  // stopAlpha:Double,
  // chooseAlpha:Double,
  uposCount:Int,
  parameterSpec:ParameterSpec
// ) extends UPOSArcFactoredParameters( rootAlpha, stopAlpha, chooseAlpha, uposCount ) {
) extends UPOSArcFactoredParameters( uposCount, parameterSpec ) {

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

  def zerosMatrix = DenseMatrix.zeros[Double]( uposCount, uposCount )

  def zerosInit( corpus:List[Utt] ) {

    p_root.clear
    p_stop.clear
    p_choose.clear

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        p_root.increment( RootEvent( h ), DenseMatrix.zeros[Double](uposCount, 1 ) )
        p_stop.increment( possibleStopEvents( h ), DenseMatrix.zeros[Double](1, uposCount ) )

        ( 0 until t ).foreach{ i =>
          p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), zerosMatrix )
        }
        ( t+1 until s.length ).foreach{ j =>
          p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), zerosMatrix )
        }
      }
    }

  }

  def harmonicCounts( corpus:List[Utt] ) = {
    println( "\n\n\n\nHARMONIC COUNTS NOT IMPLEMENTED YET FOR NoValenceUPOSParameters!!!\n\n\n\n" )
    MatrixDMVCounts( uposCount, rootAlpha, stopAlpha, chooseAlpha )
  }


  def printOut( logSpace:Boolean = false ) {
    // println( "p_root:" )
    // p_root.printOut( logSpace )
    // println( "p_stop:" )
    // p_stop.printOut( logSpace )
    // println( "p_choose:" )
    // p_choose.printOut( logSpace )
    // println( "lambda_choose:" )
    // lambda_choose.printOut( logSpace )
  }

}


