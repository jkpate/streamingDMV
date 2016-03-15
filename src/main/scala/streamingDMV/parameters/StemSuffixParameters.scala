package streamingDMV.parameters

import streamingDMV.labels._

trait StemSuffixParameters extends FirstOrderArcFactoredParameters {
  override def rootEvents( r:Int, rObs:String ) = {
    // Seq[Event]( RootEvent( r ) )
    ( 1 to rObs.length ).flatMap{ k =>
      val ( stem, suffix ) = rObs.splitAt( k )
      Seq(
        RootEvent( -1, stem ),
        ChooseEvent( stem, MorphAtt, suffix )
      )
    }.toSeq
  }
}

