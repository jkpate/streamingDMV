package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.{TopDownDMVParameters,StemSuffixParameters}

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class TwoValenceStemSuffixParser(
  parserSpec:ParserSpec
) extends TopDownDMVParser(
  parserSpec
) {

  override val theta = new TopDownDMVParameters( parserSpec.toParameterSpec ) with StemSuffixParameters

  // Keys of chart maps are StemSuffixDecoration, so they are different for every sentence.
  override def clearCharts {
    (0 until insideChart.length ).foreach{ i =>
      ((i+1) until insideChart.length+1 ).foreach{ j =>
        insideChart(i)(j).clear
        outsideChart(i)(j).clear
      }
    }
  }

  override def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).flatMap{ k =>
      val rW = lexString( k )
      ( 1 to rW.length ).map{ l =>
        // val ( stem, suffix ) = rW.splitAt( l )
        (
          k,
          DecorationPair(
            {
              // StemSuffixDecoration( stem, suffix, Outermost )
              MorphSplitDecoration( Outermost, l )
            }, {
              MorphSplitDecoration( Outermost, l )
              // StemSuffixDecoration( stem, suffix, Outermost )
            }
          )
        )
      }
    }
  }

  override def recoverRootMorphs( i:Int, rDec:Decoration ) = {
    val DecorationPair( l, _ ) = rDec
    val MorphSplitDecoration( _, split ) = l

    val ( stem, suffix) = lexString( i ).splitAt( split )
    (i, stem, suffix )
  }
  override def recoverRightwardMorphs( i:Int, mDV:Decoration ) = {
    val MorphSplitDecoration( _, split ) = mDV

    val ( stem, suffix) = lexString( i ).splitAt( split )
    (i, stem, suffix )
  }
  override def recoverLeftwardMorphs( i:Int, mDV:Decoration ) = {
    val MorphSplitDecoration( _, split ) = mDV

    val ( stem, suffix) = lexString( i ).splitAt( split )
    (i, stem, suffix )
  }

  override def findLeftRootChild( k:Int, rDec:Decoration ) = headTrace( 0 )( k )( rDec )
  override def findRightRootChild( k:Int, rDec:Decoration ) = headTrace( k )( intString.length )( rDec )

  override def findLeftLeftwardChild( i:Int, k:Int, cDV:Decoration ) = headTrace( i )( k )( cDV )
  override def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration ) = {
    mTrace( k )( j )( DecorationPair( mDV, hV ) )
  }

  override def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration ) = {
    mTrace( i )( k )( DecorationPair( hV, mDV ) )
  }
  override def findRightRightwardChild( k:Int, j:Int, cDV:Decoration ) = headTrace( k )( j )( cDV )


  override def rootCellFactor( k:Int, rDec:DecorationPair ) = {
    // val StemSuffixDecoration( stem, suffix, _ ) = rDec.evenLeft
    val MorphSplitDecoration( _, split ) = rDec.evenLeft
    val ( stem, suffix ) = lexString( k ).splitAt( split )
    // theta( RootEvent( intString( k ) ) )
    myTimes(
      theta( RootEvent( -1, stem ) ),
      theta( ChooseEvent( stem, MorphAtt, suffix ) )
    )
  }
  override def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )

    val MorphSplitDecoration( pValence, hSplit ) = pDec
    val MorphSplitDecoration( _, dSplit ) = cDec

    val (hStem, _ ) = lexString( i ).splitAt( hSplit )
    val (dStem, dSuffix ) = lexString( k ).splitAt( dSplit )

    myTimes(
      theta( ChooseEvent( hStem, RightAtt, dStem ) ),
      theta( ChooseEvent( dStem, MorphAtt, dSuffix ) ),
      theta( StopEvent( head, RightAtt, pValence, NotStop ) )
    )
  }

  override def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )

    val MorphSplitDecoration( pValence, hSplit ) = pDec
    val MorphSplitDecoration( _, dSplit ) = cDec

    val (hStem, _ ) = lexString( j ).splitAt( hSplit )
    val (dStem, dSuffix ) = lexString( k ).splitAt( dSplit )


    myTimes(
      theta( ChooseEvent( hStem, LeftAtt, dStem ) ),
      theta( ChooseEvent( dStem, MorphAtt, dSuffix ) ),
        theta( StopEvent( head, LeftAtt, pValence, NotStop ) )
    )
  }

  def rightArcSpecHelper( k:Int, hK:Int ) = {
    val dep = lexString( k )
    ( 1 to dep.length ).map{ dK =>
      // val (dStem, dSuffix) = dep.splitAt( dK )

      (
        k,
        DecorationPair(
          MorphSplitDecoration( Inner, hK ),
          MorphSplitDecoration( Outermost, dK )
        ),
        MorphSplitDecoration( Outermost, dK )
      )
    }.toSeq
  }

  def leftArcSpecHelper( k:Int, hK:Int ) = {
    val dep = lexString( k )
    ( 1 to dep.length ).map{ dK =>

      (
        k,
        DecorationPair(
          MorphSplitDecoration( Outermost, dK ),
          MorphSplitDecoration( Inner, hK )
        ),
        MorphSplitDecoration( Outermost, dK )
      )
    }.toSeq
  }


  override def rightwardSplitSpecs( i:Int, j:Int ) = {
    val hObs = lexString( i )
    val ks = ( (i+2) to (j-1) by 2 )

    (1 to hObs.length).flatMap{ hK =>

      if( j == intString.length ) {
        Seq(
          (
            MorphSplitDecoration( Outermost, hK),
            ks.flatMap{ k =>
              rightArcSpecHelper( k, hK )
            }
          )
        )
      } else {
        Seq(
          (
            MorphSplitDecoration( Outermost, hK ),
            ks.flatMap{ k =>
              rightArcSpecHelper( k, hK )
            }
          ),
          (
            MorphSplitDecoration( Inner, hK ),
            ks.flatMap{ k =>
              rightArcSpecHelper( k, hK )
            }
          )
        )
      }

    }


  }

  override def leftwardSplitSpecs( i:Int, j:Int ) = {
    val hObs = lexString( j )

    (1 to hObs.length).flatMap{ hK =>

      // val (hStem, hSuffix) = hObs.splitAt( hK )
      if( i == 0 ) {
        Seq(
          (
            MorphSplitDecoration( Outermost, hK),
            ( (i+1) to (j-2) by 2 ).flatMap{ k =>
              leftArcSpecHelper( k, hK)
            }
          )
        )
      } else {
        Seq(
          (
            MorphSplitDecoration( Outermost, hK ),
            ( (i+1) to (j-2) by 2 ).flatMap{ k =>
              leftArcSpecHelper( k, hK )
            }
          ),
          (
            MorphSplitDecoration( Inner, hK ),
            ( (i+1) to (j-2) by 2 ).flatMap{ k =>
              leftArcSpecHelper( k, hK )
            }
          )
        )
      }

    }

  }

  override def mSplitSpecs( i:Int, j:Int ) = {
    // mParents.map{ mDec => ( mDec, ks ) }
    val lObs = lexString( i )
    val rObs = lexString( j )
    val ks = ( (i+1) to (j-1) by 2 )

    (1 to lObs.length).flatMap{ lK =>
      (1 to rObs.length).flatMap{ rK =>
        Seq(
          (
            DecorationPair(
              MorphSplitDecoration( Outermost, lK ),
              MorphSplitDecoration( Inner, rK )
            ),
            ks
          ),
          (
            DecorationPair(
              MorphSplitDecoration( Inner, lK ),
              MorphSplitDecoration( Outermost, rK )
            ),
            ks
          )
        )
      }
    }
  }

  override def lexSpecs( index:Int ) = {
    val vs =
      if( index%2 == 0 ) {
        if( index > 0 )
          Seq( Inner, Outermost )
        else
          Seq( Outermost )
      } else {
        if( index < intString.length-1 )
          Seq( Inner, Outermost )
        else
          Seq( Outermost )
      }

    val w = lexString( index )
    (1 to w.length).flatMap{ wK =>
      // val (stem, suffix) = w.splitAt( wK )
      vs.map{ v =>
        MorphSplitDecoration( v, wK )
      }
    }

  }

  override def rootEventCounts( k:Int, rDec:Decoration, marginal:Double ):Seq[Tuple2[Event,Double]] = {
    val r = intString( k )
    // stem/suffix analysis will be the same for left and right children
    val DecorationPair( l , _  ) = rDec
    val MorphSplitDecoration( _, rK ) = l
    val ( rStem, rSuffix) = lexString( k ).splitAt( rK )
    Seq(
      ( RootEvent( -1, rStem ), marginal ),
      ( ChooseEvent( rStem, MorphAtt, rSuffix ), marginal )
    )
  }

  override def lexEventCounts( index:Int, pDec:Decoration, marginal:Double ) = {
    val w = intString( index )
    // val StemSuffixDecoration( _, _, pValence ) = pDec
    val MorphSplitDecoration( pValence, _ ) = pDec
    if( index%2 == 0 ) {
      Seq( ( StopEvent( w, LeftAtt, pValence, Stop ) , marginal ) )
    } else {
      Seq( ( StopEvent( w, RightAtt, pValence, Stop ) , marginal ) )
    }
  }

  override def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( i )
      // val dep = intString( k )
      // val StemSuffixDecoration( hStem, hSuffix, pValence ) = pDec
      // val StemSuffixDecoration( dStem, dSuffix, _ ) = cDec
    val MorphSplitDecoration( pValence, hK ) = pDec
    val MorphSplitDecoration( _, dK ) = cDec

    val ( hStem, hSuffix ) = lexString( i ).splitAt( hK )
    val ( dStem, dSuffix ) = lexString( k ).splitAt( dK )

    Seq(
      ( ChooseEvent( hStem, RightAtt, dStem ), marginal ),
      // ( ChooseEvent( hStem, MorphAtt, hSuffix ), marginal ),
      ( ChooseEvent( dStem, MorphAtt, dSuffix ), marginal ),
      ( StopEvent( head, RightAtt, pValence, NotStop ), marginal )
    )
  }

  override def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( j )
      // val dep = intString( k )
      // val StemSuffixDecoration( hStem, hSuffix, pValence ) = pDec
      // val StemSuffixDecoration( dStem, dSuffix, _ ) = cDec

    val MorphSplitDecoration( pValence, hK ) = pDec
    val MorphSplitDecoration( _, dK ) = cDec

    val ( hStem, hSuffix ) = lexString( j ).splitAt( hK )
    val ( dStem, dSuffix ) = lexString( k ).splitAt( dK )

    Seq(
      ( ChooseEvent( hStem, LeftAtt, dStem ), marginal ),
      // ( ChooseEvent( hStem, MorphAtt, hSuffix ), marginal ),
      ( ChooseEvent( dStem, MorphAtt, dSuffix ), marginal ),
      ( StopEvent( head, LeftAtt, pValence, NotStop ), marginal )
    )
  }





  override def lexCellFactor( index:Int, pDec:Decoration ) = {
    // println( intString.mkString("{"," ","}") )
    val head = intString( index )
    // val StemSuffixDecoration( hStem, hSuffix, pValence ) = pDec

    val MorphSplitDecoration( pValence, _ ) = pDec

    val score = 
      if( index%2 == 0 ) {
        theta( StopEvent( head, LeftAtt, pValence, Stop ) )
      } else {
        theta( StopEvent( head, RightAtt, pValence, Stop ) )
      }
    assert( ( score > myZero && score <= myOne + 0.00001 ) )
    score
  }


  override def cellMap( i:Int, j:Int ) = {
    if( ( i%2 == 1 || j%2 == 1 ) && i%2 != j%2 )
      MMap[Decoration,Double]().withDefaultValue( myZero )
    else if( i%2 == 1 && j%2 == 1 )
      MMap[Decoration,Double]().withDefaultValue( myZero )
      // MMap(
      //   DecorationPair(Outermost,Inner) -> myZero,
      //   DecorationPair(Inner,Outermost) -> myZero
      // )
    else
      MMap()
  }


}

