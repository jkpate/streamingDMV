package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.CPT

import collection.mutable.{Set=>MSet}

class DMVParameters(
  rootAlpha:Double,
  stopAlpha:Double,
  chooseAlpha:Double
) {

  var fullyNormalized:Boolean = false

  // val p_root = new CPT[RootEvent,RootNorm.type]( rootAlpha )
  // val p_stop = new CPT[StopEvent,StopNorm]( stopAlpha )
  // val p_choose = new CPT[ChooseEvent,ChooseNorm]( chooseAlpha )
  val p_root = new CPT[RootEvent]( rootAlpha )
  val p_stop = new CPT[StopEvent]( stopAlpha )
  val p_choose = new CPT[ChooseEvent]( chooseAlpha )

  def apply( r:RootEvent ) = {
    if( fullyNormalized )
      p_root.normalized( r )
    else
      p_root.expDigammaNormalized( r )
  }

  def apply( s:StopEvent ) = {
    if( fullyNormalized )
      p_stop.normalized( s )
    else
      p_stop.expDigammaNormalized( s )
  }

  def apply( c:ChooseEvent ) = {
    if( fullyNormalized )
      p_choose.normalized( c )
    else
      p_choose.expDigammaNormalized( c )
  }

  def zerosInit( corpus:List[Utt] ) {
    val rootEvents = MSet[RootEvent]()
    val stopEvents = MSet[StopEvent]()
    val chooseEvents = MSet[ChooseEvent]()

    corpus.map{_.string}.foreach{ s =>
      (0 until s.length).foreach{ t =>
        val h = s(t)

        rootEvents += RootEvent( h )
        stopEvents ++= Seq(
          StopEvent( h, LeftAtt, Outermost, Stop ),
          StopEvent( h, LeftAtt, Outermost, NotStop ),
          StopEvent( h, LeftAtt, Inner, Stop ),
          StopEvent( h, LeftAtt, Inner, NotStop ),
          StopEvent( h, RightAtt, Outermost, Stop ),
          StopEvent( h, RightAtt, Outermost, NotStop ),
          StopEvent( h, RightAtt, Inner, Stop ),
          StopEvent( h, RightAtt, Inner, NotStop )
        )

        ( 0 until t ).foreach{ i =>
          chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
        }
        ( t+1 until s.length ).foreach{ j =>
          chooseEvents += ChooseEvent( h, RightAtt, s(j) )
        }
      }
    }

    p_root.setEvents( rootEvents.toSet )
    p_stop.setEvents( stopEvents.toSet )
    p_choose.setEvents( chooseEvents.toSet )
  }

  def randomizeCounts( seed:Int, scale:Int ) {
    val r = new util.Random( seed )
    p_root.randomizeCounts( r, scale )
    p_stop.randomizeCounts( r, scale )
    p_choose.randomizeCounts( r, scale )
  }

  def incrementCounts( counts:DMVCounts ) {
    p_root.increment( counts.rootCounts )
    p_stop.increment( counts.stopCounts )
    p_choose.increment( counts.chooseCounts )
  }

  def decrementCounts( counts:DMVCounts ) {
    p_root.decrement( counts.rootCounts )
    p_stop.decrement( counts.stopCounts )
    p_choose.decrement( counts.chooseCounts )
  }




}

