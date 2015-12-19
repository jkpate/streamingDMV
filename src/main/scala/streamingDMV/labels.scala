package streamingDMV.labels

import streamingDMV.tables.XXHash
import streamingDMV.tables._
import streamingDMV.math.LogSum

import breeze.linalg._
import breeze.numerics._

import scala.util.hashing.{MurmurHash3=>MH3}

import java.nio.ByteBuffer

case class ParserSpec(
  maxLength:Int,
  alphabetSize:Int,
  randomSeed:Int,
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  backoffAlpha:Double,
  notBackoffAlpha:Double,
  squarelyNormalized:Int,
  harmonicMiniBatchInit:Boolean,
  scaleInitMiniBatchCounts:Boolean,
  approximate:Boolean,
  reservoirSize:Int,
  uposCount:Int,
  logSpace:Boolean
) {
  def toParameterSpec =
    ParameterSpec(
      rootAlpha = rootAlpha,
      stopAlpha = stopAlpha,
      chooseAlpha = chooseAlpha,
      backoffAlpha = backoffAlpha,
      notBackoffAlpha = notBackoffAlpha,
      squarelyNormalized = squarelyNormalized,
      approximate = approximate,
      randomSeed = randomSeed,
      alphabetSize = alphabetSize,
      logSpace = logSpace
    )
}
object ParserSpec {
  def withRandomSeed( parserSpec:ParserSpec, randomSeed:Int ) = {
    ParserSpec(
      maxLength = parserSpec.maxLength,
      alphabetSize = parserSpec.alphabetSize,
      randomSeed = randomSeed,
      rootAlpha = parserSpec.rootAlpha,
      stopAlpha = parserSpec.stopAlpha,
      chooseAlpha = parserSpec.chooseAlpha,
      backoffAlpha = parserSpec.backoffAlpha,
      notBackoffAlpha = parserSpec.notBackoffAlpha,
      squarelyNormalized = parserSpec.squarelyNormalized,
      harmonicMiniBatchInit = parserSpec.harmonicMiniBatchInit,
      scaleInitMiniBatchCounts = parserSpec.scaleInitMiniBatchCounts,
      approximate = parserSpec.approximate,
      reservoirSize = parserSpec.reservoirSize,
      uposCount = parserSpec.uposCount,
      logSpace = parserSpec.logSpace
    )
  }
}

case class ParameterSpec(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double,
  backoffAlpha:Double,
  notBackoffAlpha:Double,
  squarelyNormalized:Int,
  approximate:Boolean,
  randomSeed:Int,
  alphabetSize:Int,
  logSpace:Boolean
)


abstract class AnnotationStream[T] {
  val annotations:Array[T]
}

case object NullAnnotation extends AnnotationStream[Nothing] {
  val annotations = Array[Nothing]()
}
case class WordLengths( annotations:Array[Int] ) extends AnnotationStream[Int]

case class DirectedArc( hIdx:Int, dIdx:Int )

case class Parse( id:String, conParse:String, depParse:Set[DirectedArc] )
case class Utt( id:String, string:Array[Int], lexes:Array[String] = Array() ) {
  override def toString = "Utt( " + id + ", " + string.mkString("[ ",", "," ]") + ", " +
    lexes.mkString( "< ", ", ", " >" ) + " )" 
}

case class SampledCounts[C]( utt:Utt, counts:C, samplingScore:Double, trueScore:Double )

object FastHash {
  val prime_modulus = (1 << 31) - 1
  def apply( item:Int, seed:Int ) = {
    var myHash = seed * item
    myHash += myHash >> 32
    myHash & prime_modulus
  }
}

trait FastHashable extends Product {
  lazy val byteBuffer = {
    val arr = productArity
    if( arr == 0 ) {
      ByteBuffer.allocate( 4 ).putInt( super.hashCode )
    } else {
      val bytes = ByteBuffer.allocate( 4*arr ).putInt( {
          val el = productElement(0)
          el match  {
            case e:Int => e
            case _ => el.hashCode
          }
        }
      )

      var i = 1
      while( i < arr ) {
        val el = productElement( i )
        val int = el match {
          case i:Int => i
          case _ => el.hashCode
        }
        bytes.putInt( int )

        i += 1
      }

      bytes
    }
  }
  // override def hashCode = streamingDMV.tables.XXHash( this, 492876863  )
  // override val hashCode = MH3.productHash( this )
  // override val hashCode = XXHash( this, 33 )
  def fastHash( seed:Int ) = {
    val elements = productIterator
    if( elements.size > 0 ) {
      val ints = productIterator.map{ element =>
        element match {
          case e:Int => e
          case _ => { 
            element.hashCode
          }
        }
      }

      var hash = ints.next*141650963

      while( ints.hasNext ) {
        val nextInt = ints.next
        if( nextInt >= 0 )
          hash += hash*141650963 + FastHash( nextInt, seed )
      }
      hash
    } else {
      FastHash( hashCode, seed )
    }
  }
}

abstract class AttDir {
  val flip:AttDir
}
case object LeftAtt extends AttDir {
  val flip = RightAtt
  override val hashCode = 32452867
  // override val hashCode = 41
}
case object RightAtt extends AttDir {
  val flip = LeftAtt
  override val hashCode = 15485863
  // override val hashCode = 47
}

abstract class StopDecision
case object Stop extends StopDecision {
  override val hashCode = 49979693
}
case object NotStop extends StopDecision {
  override val hashCode = 86028121
  // override val hashCode = 107
}

abstract class BackoffDecision
case object Backoff extends BackoffDecision
case object NotBackoff extends BackoffDecision

abstract class Decoration

abstract class Valence extends Decoration

case object FourDependentValence extends Decoration
case object ThreeDependentValence extends Decoration
case object TwoDependentValence extends Decoration
case object OneDependentValence extends Decoration
case object NoDependentValence extends Decoration

case object NoValence extends Valence {
  override val hashCode = 104395303
  // override val hashCode = 31
}
case object Outermost extends Valence {
  override val hashCode = 122949829
  // override val hashCode = 37
}
case object Inner extends Valence {
  override val hashCode = 141650963
  // override val hashCode = 53
}
case object Outer extends Valence {
  override val hashCode = 160481219
  // override val hashCode = 79
}
case object Innermost extends Valence {
  override val hashCode = 179424691
  // override val hashCode = 57
}

case object RootDecoration extends Decoration
abstract class MDecoration extends Decoration {
  val evenLeft:Decoration
  val evenRight:Decoration
  val oddLeft:MDecoration
  val oddRight:MDecoration
}
case class DecorationPair( evenLeft:Decoration, evenRight:Decoration ) extends MDecoration {
  lazy val oddLeft = null
  lazy val oddRight = null
  // //override val hashCode = ( 73 + evenLeft.hashCode ) * 73 + evenRight.hashCode
}
case object PlainM extends MDecoration {
  val evenLeft = NoValence
  val evenRight = NoValence
  lazy val oddLeft = null
  lazy val oddRight = null
  override val hashCode = 198491329
  // override val hashCode = 17
}
case object LeftwardM extends MDecoration {
  val evenLeft = NoValence
  val evenRight = NoValence
  val oddLeft = PlainM
  val oddRight = LeftwardM
  override val hashCode = 217645199
  // override val hashCode = 89
}
case object RightwardM extends MDecoration {
  val evenLeft = NoValence
  val evenRight = NoValence
  val oddLeft = RightwardM
  val oddRight = PlainM
  override val hashCode = 236887699
  // override val hashCode = 97
}


abstract class NormKey extends FastHashable
abstract class Event /*[C]*/ extends FastHashable {
  // def normKey[N<:NormKey]:N
  def normKey:NormKey //[N<:NormKey]:N
  /*def closedSpec:C
  def openSpec:Tuple3[Int,Int,Int]*/
}

trait GeneratingString {
  val gen:String
}

// object SecondOrderChooseEvent {
//   def apply( head:Int, context:Int, dir:AttDir, v:Decoration, dep:Int ):SecondOrderChooseEvent =
//     new SecondOrderChooseEvent( head, context, dir, v, dep )
//   def apply( head:Int, context:Int, dir:AttDir, dep:Int ):SecondOrderChooseEvent =
//     new SecondOrderChooseEvent( head, context, dir, NoValence, dep )
// }
// class SecondOrderChooseEvent( head:Int, context:Int, dir:AttDir, v:Decoration, dep:Int ) extends
// ChooseEvent( head, dir, v, dep )
// object ChooseNorm {
//   def apply( head:Int, dir:AttDir ) = new ChooseNorm( head, -1, dir )
// }
case class ChooseNorm( head:Int, context:Int, dir:AttDir ) extends NormKey {
  override lazy val byteBuffer = {
    val bytes = if( context >= 0 ) ByteBuffer.allocate( 12 ) else ByteBuffer.allocate( 8 )
    bytes.putInt( head )
    if( context >= 0 ) bytes.putInt( context )
    bytes.putInt( dir.hashCode )
    bytes
  }
  // override def fastHash( seed:Int ) = {
  //   // var hash = FastHash( head, seed )
  //   var hash = head*141650963 + FastHash( dir.hashCode, seed )
  //   if( context >= 0 )
  //     hash += hash*141650963 + FastHash( context, seed )
  //   hash
  // }
  // override lazy val hashCode: Int= scala.runtime.ScalaRunTime._hashCode(ChooseNorm.this)
  // override val hashCode =
  //   ( (head + 1 ) * 83 ) + ( (context + 107) * 107) + dir.hashCode()

      // override def equals( o:Any ) = {
      //   o match {
      //     case that:ChooseNorm =>
      //         ( head == that.head ) &&
      //         ( context == that.context ) &&
      //         ( dir == that.dir )
      //     case _ => false
      //   }
      // }
}
case class ChooseEvent(
  head:Int,
  context:Int,
  dir:AttDir,
  /*v:Decoration,*/
  dep:Int,
  gen:String = ""
) extends Event with GeneratingString /*[AttDir]*/ {
  def normKey = ChooseNorm( head, context, dir )
  override lazy val byteBuffer = {
    val bytes = if( context > 0 ) ByteBuffer.allocate( 16 ) else ByteBuffer.allocate( 12 )
    bytes.putInt( head )
    if( context > 0 ) bytes.putInt( context )
    bytes.putInt( dir.hashCode )
    bytes.putInt( dep )
    bytes
  }
  // override def fastHash( seed:Int ) = {
  //   // var hash = FastHash( head, seed )
  //   var hash = head*141650963 + FastHash( dir.hashCode, seed )
  //   if( context >= 0 )
  //     hash += hash*141650963 + FastHash( context, seed )
  //   hash += hash*141650963 + FastHash( dep, seed )
  //   hash
  // }
  /*def closedSpec = dir
  def openSpec = (head, context, dep)*/
  // override val hashCode =
  //   ( (head + 107 ) * 83 ) + ( ( context + 37 ) * 37 ) + ( dep + 107 ) * 107 + dir.hashCode()

  // override def equals( o:Any ) = {
  //   o match {
  //     case that:ChooseEvent => ( head == that.head ) &&
  //         ( context == that.context ) &&
  //         ( dep == that.dep ) &&
  //         ( dir == that.dir )
  //     case _ => false
  //   }
  // }
}
object ChooseEvent {
  // some parameterizations don't use valence or context for choose

  def apply( dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( -1, -1, dir, /*NoValence,*/ dep )

  def apply( head:Int, dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( head, -1, dir, /*NoValence,*/ dep )

  def apply( head:Int, dir:AttDir, dep:Int, gen:String ):ChooseEvent =
    new ChooseEvent( head, -1, dir, /*NoValence,*/ dep, gen )

  // def apply( head:Int, context:Int, dir:AttDir, dep:Int ):ChooseEvent =
  //   new ChooseEvent( head, context, dir, /*NoValence,*/ dep )

  // def apply( head:Int, dir:AttDir, /*v:Decoration,*/ dep:Int ):ChooseEvent =
  //   new ChooseEvent( head, -1, dir, /*v,*/ dep )
}

trait BackingOffEvent {
  def backOff:BackoffDecision
}

case class LambdaChooseNorm(
  head:Int,
  context:Int,
  dir:AttDir/*,
  v:Decoration*/
) extends NormKey

case class LambdaChooseEvent(
  head:Int,
  context:Int,
  dir:AttDir,
  // v:Decoration,
  bo:BackoffDecision
) extends Event /*[Tuple2[AttDir,BackoffDecision]]*/ with BackingOffEvent {
  def normKey = LambdaChooseNorm( head, context, dir/*, v*/ )
  /*def closedSpec = (dir,bo)
  def openSpec = (head, context, 0)*/
  def backOff = bo
}

case class LambdaStopNorm(
  head:Int,
  context:Int,
  dir:AttDir,
  v:Decoration
) extends NormKey

object LambdaStopEvent {
  def apply( head:Int, dir:AttDir, bo:BackoffDecision ):LambdaStopEvent =
    LambdaStopEvent( head, -1, dir, NoValence, bo )
}

case class LambdaStopEvent(
  head:Int,
  context:Int,
  dir:AttDir,
  v:Decoration,
  bo:BackoffDecision
) extends Event /*[Tuple3[AttDir,Decoration,BackoffDecision]]*/ with BackingOffEvent {
  def normKey = LambdaStopNorm( head, context, dir, v )
  def backOff = bo
  /*def closedSpec = (dir,v,bo)
  def openSpec = (head, context, 0)*/
}



object LambdaChooseEvent {
  // def apply(
  //   head:Int,
  //   dir:AttDir,
  //   v:Decoration,
  //   bo:BackoffDecision
  // ):LambdaChooseEvent = LambdaChooseEvent( head, -1, dir, v, bo )
  def apply(
    head:Int,
    dir:AttDir,
    bo:BackoffDecision
  ):LambdaChooseEvent = LambdaChooseEvent( head, -1, dir, /*NoValence,*/ bo )
  // def apply(
  //   head:Int,
  //   context:Int,
  //   dir:AttDir,
  //   bo:BackoffDecision
  // ):LambdaChooseEvent = LambdaChooseEvent( head, context, dir, NoValence, bo )
}

object StopEvent {
  def apply( head:Int, dir:AttDir, dec:StopDecision ):StopEvent =
    StopEvent( head, dir, NoValence, dec )
  def apply( dir:AttDir, dec:StopDecision ):StopEvent =
    StopEvent( -1, dir, NoValence, dec )
}
case class StopEvent( head:Int, dir:AttDir, v:Decoration, dec:StopDecision ) extends
Event /*[Tuple3[AttDir,Decoration,StopDecision]]*/ {
  def normKey = StopNorm( head, dir, v )
  override lazy val byteBuffer = {
    val bytes = if( v != NoValence ) ByteBuffer.allocate( 16 ) else ByteBuffer.allocate( 12 )
    bytes.putInt( head )
    bytes.putInt( dir.hashCode )
    if( v != NoValence ) bytes.putInt( v.hashCode )
    bytes.putInt( dec.hashCode )
    bytes
  }

  // override def fastHash( seed:Int ) = {
  //   // var hash = FastHash( head, seed )
  //   var hash = head*141650963 + FastHash( dir.hashCode, seed )
  //   if( v != NoValence )
  //     hash += hash*141650963 + FastHash( v.hashCode, seed )
  //   hash += hash*141650963 + FastHash( dec.hashCode, seed )
  //   hash
  // }

  /*def closedSpec = (dir,v,dec)
  def openSpec = (head, 0, 0)*/
  // override lazy val hashCode: Int= scala.runtime.ScalaRunTime._hashCode(StopEvent.this)
  // override val hashCode =
  //   ( (head + 107 ) * 107 ) + dir.hashCode() + v.hashCode() + dec.hashCode()

  // override def equals( o:Any ) = {
  //   // println(".")
  //   o match {
  //     case that:StopEvent => ( head == that.head ) &&
  //         ( v == that.v ) &&
  //         ( dir == that.dir ) &&
  //         ( dec == that.dec )
  //     case _ => false
  //   }
  // }
}
case class StopNorm( head:Int, dir:AttDir, v:Decoration ) extends NormKey {
  override lazy val byteBuffer = {
    val bytes = if( v != NoValence ) ByteBuffer.allocate( 12 ) else ByteBuffer.allocate( 8 )
    bytes.putInt( head )
    bytes.putInt( dir.hashCode )
    if( v != NoValence ) bytes.putInt( v.hashCode )
    bytes
  }
  // override def fastHash( seed:Int ) = {
  //   // var hash = FastHash( head, seed )
  //   var hash = head*141650963 + FastHash( dir.hashCode, seed )
  //   if( v != NoValence )
  //     hash += hash*141650963 + FastHash( v.hashCode, seed )
  //   hash
  // }
  // override lazy val hashCode: Int= scala.runtime.ScalaRunTime._hashCode(StopNorm.this)
  // override val hashCode =
  //   ( (head + 107 ) * 107 ) + dir.hashCode() + v.hashCode()
  // override def equals( o:Any ) = {
  //   o match {
  //     case that:StopNorm => ( head == that.head ) &&
  //         ( v == that.v ) &&
  //         ( dir == that.dir )
  //     case _ => false
  //   }
  // }
}

object RootEvent {
  def apply():RootEvent = RootEvent( -1 )
}
case class RootEvent( root:Int, gen:String = "" ) extends Event with GeneratingString {
  override lazy val byteBuffer = {
    val bytes = ByteBuffer.allocate( 8 )
    bytes.putInt( root )
    bytes
  }
  def normKey = RootNorm
  /*def closedSpec = RootNorm
  def openSpec = (root, 0, 0)*/
  // override val hashCode = root
    // override def equals( o:Any ) = {
    //   o match {
    //     case that:RootEvent => root == that.root
    //     case _ => false
    //   }
    // }
}
case object RootNorm extends NormKey {
  override val hashCode = 1
  // override def equals( o:Any ) = {
  //   o match {
  //     case that:RootNorm.type => true
  //     case _ => false
  //   }
  // }
}


abstract class DependencyCounts {
  def destructivePlus[C<:DependencyCounts]( other:C ):Unit
  def totalCounts:Double
  def printTotalCountsByType:Unit

  def increment( event:Event, count:Double ):Unit
  def divideBy( x:Double ):Unit
  def multiplyBy( scale:Double )

  def scaleToMatch[C<:DependencyCounts]( other:C ):Unit

  def scaleToMatch( sentenceCount:Int, wordCount:Int ):Unit


  def printRootEvents:Unit
  def printStopEvents:Unit
  def printChooseEvents:Unit

  def logSpace = false


}

case class BackoffChooseDMVCounts(
  rootCounts:CPT[RootEvent],
  stopCounts:CPT[StopEvent],
  chooseCounts:CPT[ChooseEvent],
  lambdaChooseCounts:BackoffCPT[LambdaChooseEvent]
  // rootCounts:CPT[AbstractRootEvent],
  // stopCounts:CPT[AbstractStopEvent],
  // chooseCounts:CPT[AbstractChooseEvent]
) extends DependencyCounts {
  def destructivePlus[C<:DependencyCounts]( other:C ) {
    other match {
      case c:BackoffChooseDMVCounts => {
        rootCounts.increment( c.rootCounts, updateEvents = true )
        stopCounts.increment( c.stopCounts, updateEvents = true )
        chooseCounts.increment( c.chooseCounts, updateEvents = true )
        lambdaChooseCounts.increment( c.lambdaChooseCounts, updateEvents = true )
      }
    }
  }

  def scaleToMatch[C<:DependencyCounts]( other:C ) {
    other match {
      case c:BackoffChooseDMVCounts => {
        rootCounts.multiplyBy( c.rootCounts.totalCounts / rootCounts.totalCounts )
        stopCounts.multiplyBy( c.stopCounts.totalCounts / stopCounts.totalCounts )
        chooseCounts.multiplyBy( c.chooseCounts.totalCounts / chooseCounts.totalCounts )
        lambdaChooseCounts.multiplyBy( c.lambdaChooseCounts.totalCounts / lambdaChooseCounts.totalCounts )
      }
    }
  }

  def scaleToMatch( sentenceCount:Int, wordCount:Int ) {
    rootCounts.multiplyBy( sentenceCount  )
    stopCounts.multiplyBy( ( 2 * wordCount ) / stopCounts.totalCount )
    chooseCounts.multiplyBy( ( wordCount - sentenceCount ) / chooseCounts.totalCount )
  }

  override def logSpace = {
    rootCounts.counts.logSpace && chooseCounts.counts.logSpace && stopCounts.counts.logSpace
  }

  def divideBy( x:Double ) {
    stopCounts.divideBy( x )
    chooseCounts.divideBy( x )
    rootCounts.divideBy( x )
    lambdaChooseCounts.divideBy( x )
  }

  def multiplyBy( x:Double ) {
    stopCounts.multiplyBy( x )
    chooseCounts.multiplyBy( x )
    rootCounts.multiplyBy( x )
    lambdaChooseCounts.multiplyBy( x )
  }

  def increment( event:Event, count:Double ) {
    event match {
      case e:StopEvent => stopCounts.increment( e, count )
      case e:ChooseEvent => chooseCounts.increment( e, count )
      case e:RootEvent => rootCounts.increment( e, count )
      case e:LambdaChooseEvent => lambdaChooseCounts.increment( e, count )
    }
  }

  def printRootEvents =
    println( rootCounts.counts.keys.mkString("\t","\n\t","\n\n" ) )
  def printStopEvents =
    stopCounts.counts.keys.foreach{ e =>
      println( "\t" + e )
    }
  def printChooseEvents =
    chooseCounts.counts.keys.foreach{ e =>
      println( "\t" + e )
    }

  def printLambdaChooseEvents =
    lambdaChooseCounts.counts.keys.foreach{ e =>
      println( "\t" + e )
    }

  def totalCounts =
    if( chooseCounts.counts.logSpace )
      math.exp( rootCounts.counts.values.reduce(LogSum(_,_)) ) +
      math.exp( stopCounts.counts.values.reduce(LogSum(_,_)) ) +
      math.exp( chooseCounts.counts.values.reduce(LogSum(_,_)) ) +
      math.exp( lambdaChooseCounts.counts.values.reduce(LogSum(_,_)) )
    else
      rootCounts.counts.values.sum +
      stopCounts.counts.values.sum +
      chooseCounts.counts.values.sum +
      lambdaChooseCounts.counts.values.sum

  def printTotalCountsByType {
    println( s"  > logSpace: ${logSpace}" )
    if( logSpace ) {
      println( s"  > ${rootCounts.counts.values.map{exp(_)}.sum} root events" )
      println( s"  > ${stopCounts.counts.values.map{exp(_)}.sum} stop events" )
      println( s"  > ${chooseCounts.counts.values.map{exp(_)}.sum} choose events" )
      println( s"  > ${lambdaChooseCounts.counts.values.map{exp(_)}.sum} lambda choose events" )
      println( s"  > ${chooseCounts.denoms.size} choose LHS" )
    } else {
      println( s"  > ${rootCounts.counts.values.sum} root events" )
      println( s"  > ${stopCounts.counts.values.sum} stop events" )
      println( s"  > ${chooseCounts.counts.values.sum} choose events" )
      println( s"  > ${lambdaChooseCounts.counts.values.sum} lambda choose events" )
      println( s"  > ${chooseCounts.denoms.size} choose LHS" )
    }
  }
}

object BackoffChooseDMVCounts {
  def apply(
    rootAlpha:Double = 1D,
    stopAlpha:Double = 1D,
    chooseAlpha:Double = 1D,
    backoffMap:Map[BackoffDecision,Double],
    logSpace:Boolean
  ):BackoffChooseDMVCounts =
    if( logSpace )
      BackoffChooseDMVCounts(
        new LogCPT[RootEvent]( rootAlpha ),
        new LogCPT[StopEvent]( stopAlpha ),
        new LogCPT[ChooseEvent]( chooseAlpha ),
        new LogBackoffCPT[LambdaChooseEvent]( backoffMap )
      )
    else
      BackoffChooseDMVCounts(
        new CPT[RootEvent]( rootAlpha ),
        new CPT[StopEvent]( stopAlpha ),
        new CPT[ChooseEvent]( chooseAlpha ),
        new BackoffCPT[LambdaChooseEvent]( backoffMap  )
      )
}

case class DMVCounts(
  rootCounts:CPT[RootEvent],
  stopCounts:CPT[StopEvent],
  chooseCounts:CPT[ChooseEvent]
  // rootCounts:CPT[AbstractRootEvent],
  // stopCounts:CPT[AbstractStopEvent],
  // chooseCounts:CPT[AbstractChooseEvent]
) extends DependencyCounts {
  def destructivePlus[C<:DependencyCounts]( other:C ) {
    other match {
      case c:DMVCounts => {
        rootCounts.increment( c.rootCounts, updateEvents = true )
        stopCounts.increment( c.stopCounts, updateEvents = true )
        chooseCounts.increment( c.chooseCounts, updateEvents = true )
      }
    }
  }

  def scaleToMatch[C<:DependencyCounts]( other:C ) {
    other match {
      case c:DMVCounts => {
        rootCounts.multiplyBy( c.rootCounts.totalCount / rootCounts.totalCount )
        stopCounts.multiplyBy( c.stopCounts.totalCount / stopCounts.totalCount )
        chooseCounts.multiplyBy( c.chooseCounts.totalCount / chooseCounts.totalCount )
      }
    }
  }

  def scaleToMatch( sentenceCount:Int, wordCount:Int ) {
    rootCounts.multiplyBy( sentenceCount  )
    stopCounts.multiplyBy( ( 2 * wordCount ) / stopCounts.totalCount )
    chooseCounts.multiplyBy( ( wordCount - sentenceCount ) / chooseCounts.totalCount )
  }

  override def logSpace = {
    rootCounts.counts.logSpace && chooseCounts.counts.logSpace && stopCounts.counts.logSpace
  }

  def divideBy( x:Double ) {
    stopCounts.divideBy( x )
    chooseCounts.divideBy( x )
    rootCounts.divideBy( x )
  }

  def multiplyBy( x:Double ) {
    stopCounts.multiplyBy( x )
    chooseCounts.multiplyBy( x )
    rootCounts.multiplyBy( x )
  }

  def increment( event:Event, count:Double ) {
    event match {
      case e:StopEvent => stopCounts.increment( e, count )
      case e:ChooseEvent => chooseCounts.increment( e, count )
      case e:RootEvent => rootCounts.increment( e, count )
    }
  }

  def printRootEvents =
    println( rootCounts.counts.keys.mkString("\t","\n\t","\n\n" ) )
  def printStopEvents =
    stopCounts.counts.keys.foreach{ e =>
      println( "\t" + e + ": " + stopCounts( e ) )
    }
  def printChooseEvents =
    chooseCounts.counts.keys.foreach{ e =>
      println( "\t" + e )
    }

  // TODO implement for logSpace = true
  def cachedTotalCounts =
    rootCounts.totalCount + stopCounts.totalCount + chooseCounts.totalCount

  def totalCounts =
    if( logSpace )
      math.exp(
        rootCounts.counts.values.reduceOption(LogSum(_,_)).getOrElse( Double.NegativeInfinity ) ) +
      math.exp( stopCounts.counts.values.reduceOption(LogSum(_,_)).getOrElse( Double.NegativeInfinity ) ) +
      math.exp( chooseCounts.counts.values.reduceOption(LogSum(_,_)).getOrElse( Double.NegativeInfinity ) )
    else
      rootCounts.counts.values.sum +
      stopCounts.counts.values.sum +
      chooseCounts.counts.values.sum

  def printTotalCountsByType {
    println( s"  > logSpace: ${logSpace}" )
    if( logSpace ) {
      println( s"  > ${rootCounts.counts.values.map{exp(_)}.sum} root events" )
      println( s"  > ${stopCounts.counts.values.map{exp(_)}.sum} stop events" )
      println( s"  > ${chooseCounts.counts.values.map{exp(_)}.sum} choose events" )
      println( s"  > ${chooseCounts.denoms.size} choose LHS" )
    } else {
      println( s"  > ${rootCounts.counts.values.sum} root events" )
      println( s"  > ${stopCounts.counts.values.sum} stop events" )
      println( s"  > ${chooseCounts.counts.values.sum} choose events" )
      println( s"  > ${chooseCounts.denoms.size} choose LHS" )
    }
  }
}
object DMVCounts {
  def apply(
    rootAlpha:Double = 1D,
    stopAlpha:Double = 1D,
    chooseAlpha:Double = 1D,
    logSpace:Boolean
  ):DMVCounts =
    if( logSpace )
      DMVCounts(
        new LogCPT[RootEvent]( rootAlpha ),
        new LogCPT[StopEvent]( stopAlpha ),
        new LogCPT[ChooseEvent]( chooseAlpha )
      )
    else
      DMVCounts(
        new CPT[RootEvent]( rootAlpha ),
        new CPT[StopEvent]( stopAlpha ),
        new CPT[ChooseEvent]( chooseAlpha )
      )
}

case class MatrixDMVCounts(
  rootCounts:MatrixCPT[RootEvent],
  stopCounts:MatrixCPT[StopEvent],
  chooseCounts:MatrixCPT[ChooseEvent]
) extends DependencyCounts {
  // def destructivePlus( other:MatrixDMVCounts ) {
  def destructivePlus[C<:DependencyCounts]( other:C ) {
    other match {
      case c:MatrixDMVCounts => {
        rootCounts.increment( c.rootCounts )
        stopCounts.increment( c.stopCounts )
        chooseCounts.increment( c.chooseCounts )
      }
    }
  }

  def scaleToMatch[C<:DependencyCounts]( other:C ) {
    other match {
      case c:MatrixDMVCounts => {
        rootCounts.multiplyBy( c.rootCounts.totalCounts / rootCounts.totalCounts )
        stopCounts.multiplyBy( c.stopCounts.totalCounts / stopCounts.totalCounts )
        chooseCounts.multiplyBy( c.chooseCounts.totalCounts / chooseCounts.totalCounts )
      }
    }
  }

  def scaleToMatch( sentenceCount:Int, wordCount:Int ) { /* TODO: IMPLEMENT ME */ }

  def totalCounts =
    sum( rootCounts.counts.values.reduce(_ :+ _) ) +
    sum( stopCounts.counts.values.reduce(_ :+ _) ) +
    sum( chooseCounts.counts.values.reduce(_ :+ _) )

  def increment( event:Event, count:Double ) {
    throw new UnsupportedOperationException( "increment not yet implemented for MatrixDMVCounts" )
  }

  def divideBy( x:Double ) {
    stopCounts.divideBy( x )
    chooseCounts.divideBy( x )
    rootCounts.divideBy( x )
  }

  def multiplyBy( x:Double ) {
    stopCounts.multiplyBy( x )
    chooseCounts.multiplyBy( x )
    rootCounts.multiplyBy( x )
  }

  def printRootEvents =
  println( rootCounts.counts.keys.map{e=>e+": " + rootCounts(e)}.mkString("\t","\t","" ) )
  def printStopEvents =
    stopCounts.counts.keys.toList.sortWith(_.toString<_.toString).foreach{ e =>
      println( "\t" + e + ": " + stopCounts( e ) )
    }
  def printChooseEvents =
    chooseCounts.counts.keys.toList.sortWith(_.toString<_.toString).foreach{ e =>
      println( "\t" + e + ": " + chooseCounts( e ) )
    }


  def printTotalCountsByType {
    println(
      "> " + sum( rootCounts.counts.values.reduce(_ :+ _) ) +
      " root events" )
    println(
      "> " + sum( stopCounts.counts.values.reduce(_ :+ _) ) +
      " stop events" )
    println(
      "> " + sum( chooseCounts.counts.values.reduce(_ :+ _) ) +
      " choose events" )
  }
}
object MatrixDMVCounts {
  def apply(
    uposCount:Int,
    rootAlpha:Double = 1D,
    stopAlpha:Double = 1D,
    chooseAlpha:Double = 1D
  ):MatrixDMVCounts = MatrixDMVCounts(
    new MatrixCPT[RootEvent]( rootAlpha, uposCount, 1 ),
    new MatrixCPT[StopEvent]( stopAlpha, 1, uposCount ),
    new MatrixCPT[ChooseEvent]( chooseAlpha, uposCount, uposCount )
  )
}


