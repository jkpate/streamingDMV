package streamingDMV.labels

import streamingDMV.tables.CPT

case class DirectedArc( hIdx:Int, dIdx:Int )

case class Parse( id:String, conParse:String, depParse:Set[DirectedArc] )
case class Utt( id:String, string:Array[Int] )

abstract class AttDir
object LeftAtt extends AttDir
object RightAtt extends AttDir

abstract class StopDecision
object Stop extends StopDecision
object NotStop extends StopDecision

abstract class Decoration

abstract class LexicalContext extends Decoration
case class WordContext( w:Int ) extends LexicalContext
case class NullContext( w:Int ) extends LexicalContext

abstract class Valence extends Decoration
object NoValence extends Valence
object Outermost extends Valence
object Inner extends Valence
object Outer extends Valence
object Innermost extends Valence

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
abstract class AbstractRootEvent extends Event
abstract class AbstractStopEvent extends Event
abstract class AbstractChooseEvent extends Event

case class SecondOrderChooseNorm( head:Int, context:Int, dir:AttDir ) extends NormKey
case class SecondOrderChooseEvent( head:Int, context:Int, dir:AttDir, dep:Int ) extends
AbstractChooseEvent {
  def normKey = SecondOrderChooseNorm( head, context, dir )
}
case class ChooseNorm( head:Int, dir:AttDir ) extends NormKey
case class ChooseEvent( head:Int, dir:AttDir, v:Decoration, dep:Int ) extends AbstractChooseEvent {
  def normKey = ChooseNorm( head, dir )
}
object ChooseEvent {
  // The DMV doesn't use valence for ChooseEvents
  def apply( head:Int, dir:AttDir, dep:Int ):ChooseEvent =
    new ChooseEvent( head, dir, NoValence, dep )
}

object StopEvent {
  def apply( head:Int, dir:AttDir, dec:StopDecision ):StopEvent =
    new StopEvent( head, dir, NoValence, dec )
}
case class StopEvent( head:Int, dir:AttDir, v:Decoration, dec:StopDecision ) extends
AbstractStopEvent {
  def normKey = StopNorm( head, dir, v )
}
case class StopNorm( head:Int, dir:AttDir, v:Decoration ) extends NormKey

case class RootEvent( root:Int ) extends AbstractRootEvent {
  def normKey = RootNorm
}
object RootNorm extends NormKey

case class DMVCounts(
  // rootCounts:CPT[RootEvent],
  // stopCounts:CPT[StopEvent],
  // chooseCounts:CPT[ChooseEvent]
  rootCounts:CPT[AbstractRootEvent],
  stopCounts:CPT[AbstractStopEvent],
  chooseCounts:CPT[AbstractChooseEvent]
) {
  def destructivePlus( other:DMVCounts ) {
    rootCounts.increment( other.rootCounts )
    stopCounts.increment( other.stopCounts )
    chooseCounts.increment( other.chooseCounts )
  }
}


