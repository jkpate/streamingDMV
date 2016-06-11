package streamingDMV.tables

import streamingDMV.math.{LogSum,LogDifference}
import streamingDMV.labels.FastHashable
// import com.twitter.algebird.CMSHasherImplicits._
// import com.twitter.algebird.{CMSMonoid,CMS,BloomFilterMonoid}

import collection.mutable.{Map=>MMap,Set=>MSet}

import math.abs

class TableWrapper[E<:FastHashable](
  val approximate:Boolean = true,
  eps:Double,
  delta:Double,
  randomSeed:Int = 15,
  val logSpace:Boolean = false
) {
  val myZero = if( logSpace ) Double.NegativeInfinity else 0D
  // println( s"TableWrapper is logspace: $logSpace" )
  // lazy val exactCounts = MMap[E,Double]().withDefaultValue(0D)
  var exactCounts = Map[E,Double]().withDefaultValue( myZero )
  lazy val approximateCounts = new CountMinSketch[E]( eps, delta, randomSeed )
  // val cmsMonoid = CMS.monoid[Long]( eps, delta, randomSeed )
  // var approximateCounts = cmsMonoid.zero
  // if( approximate )
  //   println( (approximateCounts.width, approximateCounts.depth) )

  // val bfMonoid = new BloomFilterMonoid( 3, 32, randomSeed )
  // var approximateSize = bfMonoid.zero

  def apply( event:E ) = {
    if( approximate ) {
      // if( approximateSize.contains( event.toString ).isTrue )
        approximateCounts( event )
      // else
      //   0D
    } else {
      exactCounts( event )
    }
  }

  def increment( other:TableWrapper[E] ) {
    //println( "incrementing with " + (other.approximate,approximate) )
    if( other.approximate && approximate ) {
      approximateCounts.increment( other.approximateCounts )
          // // if( approximateSize.size.estimate == 0 )
          // if( approximateCounts.totalCount == 0 )
          //   approximateCounts = other.approximateCounts ++ approximateCounts
          // else
          //   approximateCounts ++= other.approximateCounts
    } else if( ! other.approximate ) {
      assert( logSpace == other.logSpace )
      other.exactCounts.foreach{ case ( k, v ) =>
        increment( k, v, allowNegative = false )
      }
    } else {
      throw new UnsupportedOperationException( "cannot increment exact counts by approximate counts" )
    }
  }

  def setCounts( other:TableWrapper[E] ) {
    //println( s"  setting counts: ${(approximate,other.approximate)}" )
    if( other.approximate && approximate ) {
      approximateCounts.set( other.approximateCounts )
    } else if( !other.approximate ) {
      // exactCounts.clear
      exactCounts = other.exactCounts
      // increment( other )
    } else {
      throw new UnsupportedOperationException( "cannot set exact counts with approximate counts" )
    }
  }

  def clear {
    if( approximate ) {
      // approximateSize = bfMonoid.zero
      // approximateCounts = cmsMonoid.zero
      approximateCounts.clear
    } else {
      exactCounts = Map().withDefaultValue(myZero)
    }
  }

  def increment( event:E, inc:Double, allowNegative:Boolean = false ) {
    if( inc > myZero  || allowNegative ) {
      if( approximate && !allowNegative ) {
        approximateCounts.conservativeIncrement( event, inc.toInt )
      } else if( !( inc > myZero ) && approximate ) {
        throw new UnsupportedOperationException( s"Attempt to increment by non-positive amount: $inc" )
      } else {
        // exactCounts = exactCounts.updated(
        //   event,
        //   if( logSpace ) {
        //     // println( s"incrementing $event by $inc" )
        //     LogSum( exactCounts( event ) , inc )
        //   } else {
        //     exactCounts( event ) + inc
        //   }
        // )
        exactCounts += event -> {
          if( logSpace ) {
            // println( s"incrementing $event by $inc" )
            LogSum( exactCounts( event ) , inc )
          } else {
            exactCounts( event ) + inc
          }
        }
      }
    } else if( ! ( inc > myZero ) ) {
      throw new UnsupportedOperationException( s"Attempt to increment by non-positive amount: $inc" )
    }
  }

  def decrement( event:E, dec:Double, integerDec:Boolean ) {
    if( approximate && dec != 0 ) {
      // OK, so actually this is possible, but I haven't implemented it
      throw new UnsupportedOperationException( "cannot decrement approximate counts" )
    } else {
      // exactCounts(event) -= dec
      // println( s"subtracting $dec from event $event (currently ${exactCounts(event)})" )
      exactCounts = 
        exactCounts.updated(
          event,
          if( logSpace ) {
            val c = exactCounts( event )
            if( c < dec )
              Double.NegativeInfinity
            else
              LogDifference( c , dec )
          } else {
            Seq(
              0,
              if( integerDec )
                math.floor( exactCounts(event) - dec )
              else
                // - 1E-11 so that floating point imprecision becomes zero again
                ( exactCounts(event) - dec ) - 1E-11
            ).max
          }
        )
    }
  }

  def decrement( other:TableWrapper[E], integerDec:Boolean = false ) {
    if( other.approximate ) {
      throw new UnsupportedOperationException( "cannot decrement approximate counts" )
    } else {
      assert( logSpace == other.logSpace )
      other.exactCounts.foreach{ case ( k, v ) =>
        // assert( abs( v - exactCounts(k)  ) <= 0.0001 )
        // if( !(
        //   abs( v - exactCounts(k)  ) <= 0.0001
        // ) ) {
        //   println( s"TableWrapper.decrement: ${(k,v)}" )
        // }
        decrement( k, v, integerDec )
      }
    }

  }


  def divideBy( x:Double ) {
    if( approximate && x != 1 ) {
      throw new UnsupportedOperationException( "cannot divideBy approximate counts" )
    } else {
      if( logSpace ) {
        assert( x > Double.NegativeInfinity && x <= 0D )
        // println( s"\ndividing by $x" )
        exactCounts.keys.foreach{ event =>
          // println( s"  $event before: ${exactCounts( event )}" )
          exactCounts = exactCounts.updated(
            event,
            exactCounts(event) - x
          )
          // println( s"  $event after: ${exactCounts( event )}" )
        }
      } else {
        exactCounts.keys.foreach{ event =>
          exactCounts = exactCounts.updated(
            event,
            exactCounts(event) / x
          )
        }
      }
    }
  }

  def multiplyBy( x:Double ) {
    if( approximate && x != 1 ) {
      throw new UnsupportedOperationException( "cannot multiplyBy approximate counts" )
    } else {
      if( logSpace ) {
        assert( x > Double.NegativeInfinity && x <= 0D )
        exactCounts.keys.foreach{ event =>
          exactCounts = exactCounts.updated(
            event,
            exactCounts(event) + x
          )
        }
      } else {
        exactCounts.keys.foreach{ event =>
          exactCounts = exactCounts.updated(
            event,
            exactCounts(event) * x
          )
        }
      }
    }
  }

  def size = {
    if( approximate ) {
      throw new UnsupportedOperationException( "cannot get approximate size" )
      // approximateSize.size.estimate
    } else {
      exactCounts.size
    }
  }

  def values = {
    if( approximate ) {
      throw new UnsupportedOperationException( "cannot get values for approximate counts" )
    } else {
      exactCounts.values
    }
  }

  def randomize( r:util.Random, scale:Int ) {
    if( approximate ) {
      throw new UnsupportedOperationException( "cannot randomize approximate counts" )
    } else {
      // exactCounts.keys
      exactCounts.keys.foreach( increment( _, r.nextDouble() * scale, allowNegative = false ) )
    }
  }

  def keys = {
    if( approximate ) {
      throw new UnsupportedOperationException( "cannot get keys for approximate counts" )
    } else {
      exactCounts.keys
    }
  }

  // def foreach[U](f: Tuple2[E,Double] => U): Unit = {
  //   if( approximate ) {
  //     throw new UnsupportedOperationException( "cannot foreach for approximate counts" )
  //   } else {
  //     val it = exactCounts.iterator
  //     while (it.hasNext) f(it.next())
  //   }
  // }

  override def clone = {
    val toReturn = new TableWrapper[E]( approximate, eps, delta, randomSeed, logSpace )
    if( approximate ) {
      // TODO optimize here??
      // toReturn.approximateCounts = approximateCounts ++ cmsMonoid.zero
      // toReturn.approximateCounts.countsTable = approximateCounts.countsTable
      // toReturn.approximateCounts.countsTable = approximateCounts.countsTable.map{_.clone}
      toReturn.approximateCounts.set( approximateCounts )
    } else {
      toReturn.exactCounts = exactCounts
    }
    toReturn
  }
}

