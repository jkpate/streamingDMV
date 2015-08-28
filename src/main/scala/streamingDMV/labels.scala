package streamingDMV.labels

import streamingDMV.tables.CPT
import streamingDMV.tables.MatrixCPT

import breeze.linalg._
import breeze.numerics._

case class DirectedArc( hIdx:Int, dIdx:Int )

case class Parse( id:String, conParse:String, depParse:Set[DirectedArc] )
case class Utt( id:String, string:Array[Int] )

abstract class AttDir {
  val flip:AttDir
}
case object LeftAtt extends AttDir {
  val flip = RightAtt
  override val hashCode = 41
}
case object RightAtt extends AttDir {
  val flip = LeftAtt
  override val hashCode = 47
}

abstract class StopDecision
case object Stop extends StopDecision {
  override val hashCode = 1
}
case object NotStop extends StopDecision {
  override val hashCode = 107
}

abstract class BackoffDecision
case object Backoff extends BackoffDecision
case object NotBackoff extends BackoffDecision

abstract class Decoration

abstract class Valence extends Decoration
case object NoValence extends Valence {
  override val hashCode = 31
}
case object Outermost extends Valence {
  override val hashCode = 37
}
case object Inner extends Valence {
  override val hashCode = 53
}
case object Outer extends Valence {
  override val hashCode = 79
}
case object Innermost extends Valence {
  override val hashCode = 57
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
  override val hashCode = ( 73 + evenLeft.hashCode ) * 73 + evenRight.hashCode
}
case object PlainM extends MDecoration {
  val evenLeft = NoValence
  val evenRight = NoValence
  lazy val oddLeft = null
  lazy val oddRight = null
  override val hashCode = 17
}
case object LeftwardM extends MDecoration {
  val evenLeft = NoValence
  val evenRight = NoValence
  val oddLeft = PlainM
  val oddRight = LeftwardM
  override val hashCode = 89
}
case object RightwardM extends MDecoration {
  val evenLeft = NoValence
  val evenRight = NoValence
  val oddLeft = RightwardM
  val oddRight = PlainM
  override val hashCode = 97
}


abstract class NormKey 
abstract class Event {
  // def normKey[N<:NormKey]:N
  def normKey:NormKey//[N<:NormKey]:N
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
  // override lazy val hashCode: Int= scala.runtime.ScalaRunTime._hashCode(ChooseNorm.this)
  override val hashCode =
    ( (head + 1 ) * 83 ) + ( (context + 107) * 107) + dir.hashCode()

  override def equals( o:Any ) = {
    o match {
      case that:ChooseNorm =>
          ( head == that.head ) &&
          ( context == that.context ) &&
          ( dir == that.dir )
      case _ => false
    }
  }
}
case class ChooseEvent( head:Int, context:Int, dir:AttDir, /*v:Decoration,*/ dep:Int ) extends Event {
  def normKey = ChooseNorm( head, context, dir )
  override val hashCode =
    ( (head + 107 ) * 83 ) + ( ( context + 37 ) * 37 ) + ( dep + 107 ) * 107 + dir.hashCode()

  override def equals( o:Any ) = {
    o match {
      case that:ChooseEvent => ( head == that.head ) &&
          ( context == that.context ) &&
          ( dep == that.dep ) &&
          ( dir == that.dir )
      case _ => false
    }
  }
}
object ChooseEvent {
  // some parameterizations don't use valence or context for choose

  def apply( dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( -1, -1, dir, /*NoValence,*/ dep )

  def apply( head:Int, dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( head, -1, dir, /*NoValence,*/ dep )

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
) extends Event with BackingOffEvent {
  def normKey = LambdaChooseNorm( head, context, dir/*, v*/ )
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
) extends Event with BackingOffEvent {
  def normKey = LambdaStopNorm( head, context, dir, v )
  def backOff = bo
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
case class StopEvent( head:Int, dir:AttDir, v:Decoration, dec:StopDecision ) extends Event {
  def normKey = StopNorm( head, dir, v )
  // override lazy val hashCode: Int= scala.runtime.ScalaRunTime._hashCode(StopEvent.this)
  override val hashCode =
    ( (head + 107 ) * 107 ) + dir.hashCode() + v.hashCode() + dec.hashCode()

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
  // override lazy val hashCode: Int= scala.runtime.ScalaRunTime._hashCode(StopNorm.this)
  override val hashCode =
    ( (head + 107 ) * 107 ) + dir.hashCode() + v.hashCode()
  override def equals( o:Any ) = {
    o match {
      case that:StopNorm => ( head == that.head ) &&
          ( v == that.v ) &&
          ( dir == that.dir )
      case _ => false
    }
  }
}

object RootEvent {
  def apply():RootEvent = RootEvent( -1 )
}
case class RootEvent( root:Int ) extends Event {
  def normKey = RootNorm
  override val hashCode = root
  override def equals( o:Any ) = {
    o match {
      case that:RootEvent => root == that.root
      case _ => false
    }
  }
}
case object RootNorm extends NormKey {
  override val hashCode = 1
  override def equals( o:Any ) = {
    o match {
      case that:RootNorm.type => true
      case _ => false
    }
  }
}


abstract class DependencyCounts {
  def destructivePlus[C<:DependencyCounts]( other:C ):Unit
  def totalCounts:Double
  def printTotalCountsByType:Unit

  def printRootEvents:Unit
  def printStopEvents:Unit
  def printChooseEvents:Unit

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
        rootCounts.increment( c.rootCounts )
        stopCounts.increment( c.stopCounts )
        chooseCounts.increment( c.chooseCounts )
      }
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

  def totalCounts = 
    rootCounts.counts.values.sum +
    stopCounts.counts.values.sum +
    chooseCounts.counts.values.sum
  def printTotalCountsByType {
    println( s"> ${rootCounts.counts.values.sum} root events" )
    println( s"> ${stopCounts.counts.values.sum} stop events" )
    println( s"> ${chooseCounts.counts.values.sum} choose events" )
    println( s"> ${chooseCounts.denoms.size} choose LHS" )
  }
}
object DMVCounts {
  def apply(
    rootAlpha:Double = 1D,
    stopAlpha:Double = 1D,
    chooseAlpha:Double = 1D
  ):DMVCounts = DMVCounts(
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
  def totalCounts =
    sum( rootCounts.counts.values.reduce(_ :+ _) ) +
    sum( stopCounts.counts.values.reduce(_ :+ _) ) +
    sum( chooseCounts.counts.values.reduce(_ :+ _) )

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


