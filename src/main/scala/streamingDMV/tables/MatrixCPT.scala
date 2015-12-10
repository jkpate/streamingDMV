package streamingDMV.tables

import streamingDMV.labels._
import collection.mutable.{Map=>MMap,Set=>MSet}

import breeze.linalg._
import breeze.generic._
import breeze.numerics._

class MatrixCPT[E<:Event]( alpha:Double, rows:Int, cols:Int ) {
  var events = MSet[E]()
  var counts = MMap[E,DenseMatrix[Double]]()
  var denomCounts = MMap[NormKey,DenseVector[Double]]()
  var denoms = MMap[NormKey,MSet[E]]()

  def totalCounts = sum( denomCounts.values.reduce( _:+_ ) )

  def zeroMatrix = DenseMatrix.zeros[Double]( rows, cols )
  def zeroVector = DenseVector.zeros[Double]( cols )

  def apply( event:E ) =
    counts( event )

  def normalized( event:E ) = {
    val n = event.normKey

    val toReturn = counts( event ) :+ alpha

    toReturn(*,::) :/= (
      denomCounts( n ) :+ (alpha * denoms(n).size )
    )
    toReturn
  }

  object fastExpDigamma extends UFunc {
    implicit object implDouble extends Impl[Double, Double] {
      def taylorExpDigamma( v:Double ) = {
        // Mark's fast digamma approximation
        // return v-0.5 if v > 1.0 else 0.5*v*v
        if( v >= 1.0 )
          v-0.5+0.061/v
        else
          v*v*(0.561 + 0.8*(v-0.5)*(v-1.0))
      }

      def apply( x:Double ) = {
        taylorExpDigamma( x )
      }
    }
  }

  def expDigammaNormalized( event:E ) = {
    val n = event.normKey

    val toReturn = counts( event ) :+ alpha

    val denominator = 
      denomCounts( n ) :+ (alpha * denoms(n).size )

    fastExpDigamma.inPlace( toReturn )
    fastExpDigamma.inPlace( denominator )

    toReturn(*,::) :/= denominator
    toReturn
  }

  def increment( event:E, inc:DenseMatrix[Double] ) = {
    counts += event -> { counts.getOrElse( event, zeroMatrix ) + inc }

    // if( inc.rows != rows ) {
    //   println( event )
    //   println( (inc.rows,inc.cols) )
    // }

    val n = event.normKey
    denomCounts +=
      n -> { denomCounts.getOrElse( n, zeroVector ) :+ sum( inc(::,*) ).t /*.toDenseVector*/ }

    denoms.getOrElseUpdate( n, MSet() ) += event
  }

  def increment( events:Seq[E], inc:DenseMatrix[Double] ) {
    events.foreach{ increment( _, inc ) }
  }

  def increment( other:MatrixCPT[E] ) {
    other.counts.foreach{ case( k, v) =>
      increment( k, v )
    }
  }
  def setEvents( other:MatrixCPT[E] ) {
    clear
    denoms = other.denoms.clone
    denomCounts = other.denomCounts.clone
    events = other.events.clone
  }
  def setEventsAndCounts( other:MatrixCPT[E] ) {
    clear
    denoms = other.denoms.clone
    denomCounts = other.denomCounts.clone
    events = other.events.clone
    counts = other.counts.clone
  }

  def decrement( other:MatrixCPT[E], integerDec:Boolean ) {
    // TODO implement integer decrements
    assert( !integerDec )
    other.counts.foreach{ case( k, v) =>
      decrement( k, v )
    }
  }

  def divideBy( x:Double ) {
    counts.keys.foreach{ counts(_) /= x }
  }

  def multiplyBy( x:Double ) {
    counts.keys.foreach{ counts(_) *= x }
  }

  def decrement( event:E, dec:Double ) = {
    counts( event ) -= dec
    val n = event.normKey
    denomCounts( n ) -= dec
  }

  def decrement( event:E, dec:DenseMatrix[Double] ) = {
    counts( event ) -= dec
    val n = event.normKey
    denomCounts( n ) -= sum( dec(::,*) ).t /*.toDenseVector*/
  }

  def clear {
    counts.clear
    denomCounts.clear
    denoms.clear
  }

  def setEvents( events:Set[E] ) {
    clear
    events.groupBy( _.normKey ).foreach{ case (n, events) =>
      counts ++= events.toSeq.map{ e =>
        e -> zeroMatrix
      }
      denomCounts += n -> zeroMatrix(0,::).t
      denoms += n -> MSet( events.toSeq:_* )
    }
  }

  def randomizeCounts( r:util.Random, scale:Int ) {
    counts.keys.foreach( increment( _, DenseMatrix.fill[Double](rows,cols)( r.nextDouble() * scale ) ) )
  }

  def size = counts.size

  // def printOut( logSpace:Boolean = false ) {
  //   denoms.foreach{ case (n, events) =>
  //     println( s"$n:" )
  //     events.foreach{ e =>
  //       if( logSpace )
  //         println( s"  $e: ${math.log(counts(e))}" )
  //       else
  //         println( s"  $e: ${counts(e)}" )
  //     }
  //   }
  // }

}




