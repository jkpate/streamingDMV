package streamingDMV.labels

import streamingDMV.tables.CPT

case class DirectedArc( hIdx:Int, dIdx:Int )

case class Parse( id:String, conParse:String, depParse:Set[DirectedArc] )
case class Utt( id:String, string:Array[Int] )

abstract class AttDir {
  val flip:AttDir
}
case object LeftAtt extends AttDir {
  val flip = RightAtt
}
case object RightAtt extends AttDir {
  val flip = LeftAtt
}

abstract class StopDecision
case object Stop extends StopDecision
case object NotStop extends StopDecision

abstract class BackoffDecision
case object Backoff extends BackoffDecision
case object NotBackoff extends BackoffDecision

abstract class Decoration


abstract class Valence extends Decoration
case object NoValence extends Valence
case object Outermost extends Valence
case object Inner extends Valence
case object Outer extends Valence
case object Innermost extends Valence

abstract class MDecoration extends Decoration {
  val left:Decoration
  val right:Decoration
}
case class DecorationPair( left:Decoration, right:Decoration ) extends MDecoration
abstract class NoValenceM extends MDecoration {
  val left = NoValence
  val right = NoValence
}
case object PlainM extends NoValenceM
case object LeftwardM extends NoValenceM
case object RightwardM extends NoValenceM


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
case class ChooseNorm( head:Int, context:Int, dir:AttDir ) extends NormKey
case class ChooseEvent( head:Int, context:Int, dir:AttDir, v:Decoration, dep:Int ) extends Event {
  def normKey = ChooseNorm( head, context, dir )
}
object ChooseEvent {
  // some parameterizations don't use valence or context for choose

  def apply( head:Int, dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( head, -1, dir, NoValence, dep )

  def apply( head:Int, context:Int, dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( head, context, dir, NoValence, dep )

  def apply( head:Int, dir:AttDir, v:Decoration, dep:Int ):ChooseEvent =
    new ChooseEvent( head, -1, dir, v, dep )
}

trait BackingOffEvent {
  def backOff:BackoffDecision
}

case class LambdaChooseNorm(
  head:Int,
  context:Int,
  dir:AttDir,
  v:Decoration
) extends NormKey

case class LambdaChooseEvent(
  head:Int,
  context:Int,
  dir:AttDir,
  v:Decoration,
  bo:BackoffDecision
) extends Event with BackingOffEvent {
  def normKey = LambdaChooseNorm( head, context, dir, v )
  def backOff = bo
}


object LambdaChooseEvent {
  def apply(
    head:Int,
    dir:AttDir,
    v:Decoration,
    bo:BackoffDecision
  ):LambdaChooseEvent = LambdaChooseEvent( head, -1, dir, v, bo )
  def apply(
    head:Int,
    dir:AttDir,
    bo:BackoffDecision
  ):LambdaChooseEvent = LambdaChooseEvent( head, -1, dir, NoValence, bo )
  def apply(
    head:Int,
    context:Int,
    dir:AttDir,
    bo:BackoffDecision
  ):LambdaChooseEvent = LambdaChooseEvent( head, context, dir, NoValence, bo )
}

object StopEvent {
  def apply( head:Int, dir:AttDir, dec:StopDecision ):StopEvent =
    StopEvent( head, dir, NoValence, dec )
}
case class StopEvent( head:Int, dir:AttDir, v:Decoration, dec:StopDecision ) extends Event {
  def normKey = StopNorm( head, dir, v )
}
case class StopNorm( head:Int, dir:AttDir, v:Decoration ) extends NormKey

case class RootEvent( root:Int ) extends Event {
  def normKey = RootNorm
}
case object RootNorm extends NormKey

case class DMVCounts(
  rootCounts:CPT[RootEvent],
  stopCounts:CPT[StopEvent],
  chooseCounts:CPT[ChooseEvent]
  // rootCounts:CPT[AbstractRootEvent],
  // stopCounts:CPT[AbstractStopEvent],
  // chooseCounts:CPT[AbstractChooseEvent]
) {
  def destructivePlus( other:DMVCounts ) {
    rootCounts.increment( other.rootCounts )
    stopCounts.increment( other.stopCounts )
    chooseCounts.increment( other.chooseCounts )
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


