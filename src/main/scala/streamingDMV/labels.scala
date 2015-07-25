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

abstract class Valence
object Outermost extends Valence
object Inner extends Valence


abstract class NormKey
abstract class Event {
  // def normKey[N<:NormKey]:N
  def normKey:NormKey//[N<:NormKey]:N
}

case class ChooseNorm( head:Int, dir:AttDir ) extends NormKey
case class ChooseEvent( head:Int, dir:AttDir, dep:Int ) extends Event {
  def normKey = ChooseNorm( head, dir )
}

case class StopEvent( head:Int, dir:AttDir, v:Valence, dec:StopDecision ) extends Event {
  def normKey = StopNorm( head, dir, v )
}
case class StopNorm( head:Int, dir:AttDir, v:Valence ) extends NormKey



case class RootEvent( root:Int ) extends Event {
  def normKey = RootNorm
}
object RootNorm extends NormKey

case class DMVCounts(
  rootCounts:CPT[RootEvent],
  stopCounts:CPT[StopEvent],
  chooseCounts:CPT[ChooseEvent]
) {
  def destructivePlus( other:DMVCounts ) {
    rootCounts.increment( other.rootCounts )
    stopCounts.increment( other.stopCounts )
    chooseCounts.increment( other.chooseCounts )
  }
}


