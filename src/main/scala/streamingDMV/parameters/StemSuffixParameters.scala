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

  override def chooseEvents( h:Int, hObs:String, dir:AttDir, d:Int, dObs:String ) = {
    // ChooseEvent( h, dir, d )
    (1 to hObs.length).flatMap{ hK =>
      val (hStem, hSuffix) = hObs.splitAt( hK )

      (1 to dObs.length).flatMap{ dK =>
      val (dStem, dSuffix) = dObs.splitAt( dK )
        Seq(
          ChooseEvent( hStem, dir, dStem  ),
          ChooseEvent( dStem, MorphAtt, dSuffix )
        )
      }
    }
  }.toSeq

}

