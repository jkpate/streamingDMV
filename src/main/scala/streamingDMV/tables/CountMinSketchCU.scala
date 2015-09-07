package streamingDMV.tables

import streamingDMV.labels.FastHashable

import scala.util.Random
// import scala.util.hashing.MurmurHash3
import scala.math.{ceil,E,log,abs}
import scala.collection.mutable.{Seq=>MSeq}


class CountMinSketch[E<:FastHashable] (
  eps:Double,
  delta:Double,
  var randomSeed:Int
) {
  val width:Int = ceil( E/eps ).toInt
  val depth:Int = log( 1 / delta ).toInt

  val r = new Random( randomSeed )

  var hashSeeds = Array.fill( depth )( r.nextInt )

  // println( hashSeeds.mkString( "{ ", ", ", " }" ) )

  // lazy var countsTable = MSeq.fill( depth, width )( 0L )
  // var countsTable = Seq[Seq[Long]]()
  var countsTable = Vector[Long]()
  def initializeCountsTable {
    // countsTable = MSeq.fill( depth, width )( 0L )
    countsTable = Vector.fill( depth * width )( 0L )
  }

      // def clone = {
      //   val toReturn = new CountMinSketch( eps, delta, randomSeed )
      //   toReturn.countsTable = countsTable.map{_.clone}
      //   toReturn
      // }

  def clear {
    if( countsTable.size > 0 )
      // (0 until depth).foreach{ d =>
      //   (0 until width).foreach{ w => countsTable(d)(w) = 0 }
      // }
      initializeCountsTable
  }

  def apply( event:E ) = {
    // val hashes = hashSeeds.map{ seed => abs( event.fastHash( seed ) % width ) }

    // (0 until depth).map{ h =>
    //   countsTable( h * depth + hashes( h ) )
    // }.min

    var minValue = countsTable( abs( event.fastHash( hashSeeds(0) ) % width ) )
    var h = 1
    while( h < depth ) {
      val otherCount = countsTable( h*depth + abs( event.fastHash( hashSeeds(h) ) % width ) )
      if( otherCount < minValue ) minValue = otherCount
      h += 1
    }
    minValue
  }

  def apply( whichHash:Int, hash:Int ) = countsTable( whichHash*depth + hash )

  def conservativeIncrement( event:E, c:Long ) {
    if( c > 0 ) {

      val hashes =
        hashSeeds.map{ seed =>
          abs( event.fastHash( seed ) % width )
        }

      // val incrementTo = (0 until depth).map{ h =>
      //     apply( h, hashes(h) )
      //   }.min + c

      // (0 until depth).foreach{ h =>
      //   if( apply( h , hashes(h) ) < incrementTo ) {
      //     countsTable =
      //       countsTable.updated( h * depth + hashes(h), incrementTo )
      //   }
      // }


      // Find minimum increment for conservative update
      var incrementTo = countsTable( hashes(0) ) + c
      var h = 1
      val flatIndices = Array.fill[Int](depth)( -1 )
      while( h < depth ) {
        val otherCount = apply( h, hashes(h) ) + c
        if( otherCount < incrementTo ) incrementTo = otherCount
        h += 1
      }

      // increment only necessary counters
      h = 0
      while( h < depth ) {
        if( countsTable( h*depth + hashes(h) ) < incrementTo )
          countsTable =
            countsTable.updated( h * depth + hashes(h), incrementTo )
        h += 1
      }

      // println( s"  new count of $event is ${apply( event )}" )
    }
  }

  def increment( event:E, c:Long ) {
    if( c > 0 ) {
      //println( "  increment with other Event" )
      // val hashes = hashSeeds.map{ MurmurHash3.productHash( event, _ ) % width }
      val hashes = hashSeeds.map{ seed => abs( event.fastHash( seed ) % width ) }
      // val hashes = hashSeeds.map{ seed => event.fastHash( seed ) % width }

      (0 until depth).foreach{ h =>
        // countsTable( h )( hashes(h) ) += c
        countsTable =
          countsTable.updated( h * depth + hashes(h) , apply( h, hashes(h) ) + c )
      }
    }
  }

  def increment( other:CountMinSketch[E] ) {
    assert( width == other.width )
    assert( depth == other.depth )
    assert( (0 until depth).forall{ k => hashSeeds(k) == other.hashSeeds(k) } )
    //println( "  increment with other CountMinSketch" )

        // (0 until depth).foreach{ d =>
        //   (0 until width).foreach{ w =>
        //     countsTable( d )( w ) += other.countsTable( d )( w )
        //   }
        // }
    (0 until depth*width).foreach{ i =>
      countsTable =
        countsTable.updated( i, countsTable(i) + other.countsTable(i) )
    }
    // countsTable = other.countsTable.to
    // (0 until depth*width).toVector.map{ i =>
    //   countsTable(i) + other.countsTable(i) 
    // }
  }

  def set( other:CountMinSketch[E] ) {
    assert( width == other.width )
    assert( depth == other.depth )
    randomSeed = other.randomSeed
    hashSeeds = other.hashSeeds
    // println( hashSeeds.mkString("[ ", ", ", " ]") )
      // Advantage of using immutable.Vector for countsTable: succinct copying
    countsTable = other.countsTable

  }


}


