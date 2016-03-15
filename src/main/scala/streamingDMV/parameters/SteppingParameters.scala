package streamingDMV.parameters

import streamingDMV.labels._
import streamingDMV.tables.SteppingCPT

trait SteppingParameters extends FirstOrderArcFactoredParameters {

  override val p_root =
    new SteppingCPT[RootEvent](
      alpha = rootAlpha,
      kappa = kappa,
      tau = tau,
      randomSeed = randomSeed
    )

  override val p_choose =
    new SteppingCPT[ChooseEvent](
      alpha = chooseAlpha,
      kappa = kappa,
      tau = tau,
      randomSeed = randomSeed
    )

  override val p_stop =
    new SteppingCPT[StopEvent](
      alpha = stopAlpha,
      kappa = kappa,
      tau = tau,
      randomSeed = randomSeed
    )

  override def apply( r:RootEvent ) = p_root( r )
  override def apply( c:ChooseEvent ) = p_choose( c )
  override def apply( s:StopEvent ) = p_stop( s )

  var totalRootEvents = 0
  var totalChooseEvents = 0
  var totalStopEvents = 0

  override def zerosInit( corpus:List[Utt] ) {
    p_root.clear
    p_stop.clear
    p_choose.clear

    var rEvents = Set[RootEvent]()
    var cEvents = Set[ChooseEvent]()
    var sEvents = Set[StopEvent]()

    corpus.foreach{ case Utt( _, s, w ) =>
      totalRootEvents += 1
      totalChooseEvents += s.length - 1
      totalStopEvents += 2* s.length
      (0 until s.length).foreach{ t =>
        val h = s(t)
        val hW = if( w.length > 0 ) { w(t) } else { "" }

        // rootEvents += RootEvent( h )
        // stopEvents ++= possibleStopEvents( h )
        // p_root.increment( RootEvent( h ), myZero )
        rootEvents(h, hW).foreach{ rEvent =>
          rEvent match {
            case rootEvent:RootEvent => rEvents += rootEvent
            case chooseEvent:ChooseEvent => cEvents += chooseEvent
          }
        }
        // p_stop.increment( possibleStopEvents( h ), myZero )
        sEvents ++= possibleStopEvents( h )

        ( 0 until t ).foreach{ i =>
          // chooseEvents += ChooseEvent( h, LeftAtt, s(i) )
          // p_choose.increment( ChooseEvent( h, LeftAtt, s(i) ), myZero )
          cEvents += ChooseEvent( h, LeftAtt, s(i) )

        }
        ( t+1 until s.length ).foreach{ j =>
          // chooseEvents += ChooseEvent( h, RightAtt, s(j) )
          // p_choose.increment( ChooseEvent( h, RightAtt, s(j) ), myZero )
          cEvents += ChooseEvent( h, RightAtt, s(j) )
        }
      }
    }

    p_root.addEvents( rEvents )
    p_choose.addEvents( cEvents )
    p_stop.addEvents( sEvents )


    p_root.makeUniform
    p_choose.makeUniform
    p_stop.makeUniform
  }

  def step( counts:DMVCounts, rEvents:Double, cEvents:Double, sEvents:Double ) {
    p_root.step( counts.rootCounts, totalRootEvents / rEvents )
    p_choose.step( counts.chooseCounts, totalChooseEvents / rEvents )
    p_stop.step( counts.stopCounts, totalStopEvents / rEvents )

  }


}


