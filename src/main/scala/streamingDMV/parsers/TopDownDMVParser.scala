package streamingDMV.parsers

import scala.collection.mutable.{Map=>MMap}

class TopDownDMVParser(
  maxLength:Int,
  vocabSize:Int
) {

  val theta = new DMVParameters

  val insideHeads = Array.fill( maxLength, maxLength )( Map( true -> 0D, false -> 0D ) )
  val insideM = Array.fill( maxLength, maxLength )( 0D )
  val outsideHeads = Array.fill( maxLength, maxLength )( Map( true -> 0D, false -> 0D ) )
  val outsideM = Array.fill( maxLength, maxLength )( 0D )
  var stringProb = 0D

  def clearCharts {
    (0 until maxLength ).foreach{ i =>
      (0 until maxLength ).foreach{ j =>
        if( i%2 != j%2 ) {
          insideHeads(i)(j) += true -> 0D
          if( j-i > 1 )
            insideHeads(i)(j) += false -> 0D

          outsideHeads(i)(j) += true -> 0D
          if( j-i > 1 )
            outsideHeads(i)(j) += false -> 0D
        } else if( i%2 == 1 ) {
          insideM(i)(j) = 0D
          outsideM(i)(j) = 0D
        } else {
          stringProb = 0D
          // insideHeads(i)(j) = 0D
          // outsideHeads(i)(j) = 0D
        }
      }
    }
  }

  def lexFill( index:Int ) {
    insideHeads(index)(index+1) = 1D
  }

  def synFill( i:Int, j:Int, s:Array[Int] ) {
    if( i%2 == 1 && j%2 == 0 ) {
      // Rightward
    } else if( i%2 == 0 && j%2 == 1 ) {
      // Leftward
      val head = s( j )
      ( (i+1) to (j-2) by 2 ).foreach{ k =>
        val dep = s( k )
        val cDVs = if( k-i > 1 ) Set( true, false ) else Set( true )
        val mDVs = if( k-i > 1 ) Set( true, false ) else Set( true )
      }
    } else if( i%2 == 1 && j%2 == 1 ) {
      // M
      ( (i+1) to (j-1) by 2 ).foreach{ k =>
        val head = s( k )
        insideM( i )( j ) +=
          insideHeads( i )( k ) *
            insideHeads( k )( j ) *
              theta.p_stop( Stop | head, LeftAtt, k-i == 1 ) *
              theta.p_stop( Stop | head, LeftAtt, k-i == 1 ) *
      }
      insideM( i )( j ) = 
    } else {
      // Root
    }
  }

  def insidePass( s:Array[Int] ) = {
    (1 to ( 2*s.length )).foreach{ j =>
      lexFill( j-1 )

      if( j > 1 )
        (0 to (j-2)).reverse.foreach{ i =>
          synFill( i , j )
        }
    }
  }

}


// vim: set ts=2 sw=2 et:
