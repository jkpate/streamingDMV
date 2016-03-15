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

  override def rootSplitSpecs() = {
    ( 1 to (intString.length-1) by 2 ).flatMap{ k =>
      val rW = lexString( k )
      ( 1 to rW.length ).map{ l =>
        val ( stem, suffix ) = rW.splitAt( l )
        (
          k,
          DecorationPair(
            {
              StemSuffixDecoration( stem, suffix, Outermost )
            }, {
              StemSuffixDecoration( stem, suffix, Outermost )
            }
          )
        )
      }
    }
  }

  override def findLeftRootChild( k:Int, rDec:Decoration ) = headTrace( 0 )( k )( rDec )
  override def findRightRootChild( k:Int, rDec:Decoration ) = headTrace( k )( intString.length )( rDec )

  override def rootCellFactor( k:Int, rDec:DecorationPair ) = {
    val StemSuffixDecoration( stem, suffix, _ ) = rDec.evenLeft
    // theta( RootEvent( intString( k ) ) )
    myTimes(
      theta( RootEvent( -1, stem ) ),
      theta( ChooseEvent( stem, MorphAtt, suffix ) )
    )
  }
  override def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    // val dep = intString( k )
    val StemSuffixDecoration( hStem, hSuffix, _ ) = pDec
    val StemSuffixDecoration( dStem, dSuffix, _ ) = cDec

    myTimes(
      // theta( ChooseEvent( head, RightAtt, dep ) ),
      theta( ChooseEvent( hStem, RightAtt, dStem ) ),
      theta( ChooseEvent( hStem, MorphAtt, hSuffix ) ),
      theta( ChooseEvent( dStem, MorphAtt, dSuffix ) ),
      theta( StopEvent( head, RightAtt, pDec, NotStop ) )
    )
  }
  override def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )
    // val dep = intString( k )
    val StemSuffixDecoration( hStem, hSuffix, _ ) = pDec
    val StemSuffixDecoration( dStem, dSuffix, _ ) = cDec

    myTimes(
      // theta( ChooseEvent( head, LeftAtt, dep ) ),
      theta( ChooseEvent( hStem, LeftAtt, dStem ) ),
      theta( ChooseEvent( hStem, MorphAtt, hSuffix ) ),
      theta( ChooseEvent( dStem, MorphAtt, dSuffix ) ),
        theta( StopEvent( head, LeftAtt, pDec, NotStop ) )
    )
  }

  def rightArcSpecHelper( k:Int, hStem:String, hSuffix:String, dep:String ) = {
    ( 1 to dep.length ).map{ dK =>
      val (dStem, dSuffix) = dep.splitAt( dK )

      (
        k,
        DecorationPair(
          StemSuffixDecoration( hStem, hSuffix, Inner ),
          StemSuffixDecoration( dStem, dSuffix, Outermost )
        ),
        StemSuffixDecoration( dStem, dSuffix, Outermost )
        // Outermost
      )
    }.toSeq
  }
  def leftArcSpecHelper( k:Int, hStem:String, hSuffix:String, dep:String ) = {
    ( 1 to dep.length ).map{ dK =>
      val (dStem, dSuffix) = dep.splitAt( dK )

      (
        k,
        DecorationPair(
          StemSuffixDecoration( dStem, dSuffix, Outermost ),
          StemSuffixDecoration( hStem, hSuffix, Inner )
        ),
        StemSuffixDecoration( dStem, dSuffix, Outermost )
        // Outermost
      )
    }.toSeq
  }


  override def rightwardSplitSpecs( i:Int, j:Int ) = {
    val hObs = lexString( i )

    (1 to hObs.length).flatMap{ hK =>

      val (hStem, hSuffix) = hObs.splitAt( hK )

      if( j == intString.length ) {
        Seq(
          (
            StemSuffixDecoration( hStem, hSuffix, Outermost ),
            ( (i+2) to (j-1) by 2 ).flatMap{ k =>
              val dObs = lexString( k )

              rightArcSpecHelper( k, hStem, hSuffix, dObs )

              // ( k, DecorationPair( Inner, Outermost ), Outermost )
            }
          )
        )
      } else {
        Seq(
          (
            StemSuffixDecoration( hStem, hSuffix, Outermost ),
            ( (i+2) to (j-1) by 2 ).flatMap{ k =>
              val dObs = lexString( k )
              rightArcSpecHelper( k, hStem, hSuffix, dObs )
              // ( k, DecorationPair( Inner, Outermost ), Outermost )
            }
          ),
          (
            StemSuffixDecoration( hStem, hSuffix, Inner ),
            ( (i+2) to (j-1) by 2 ).flatMap{ k =>
              val dObs = lexString( k )
              rightArcSpecHelper( k, hStem, hSuffix, dObs )
              // ( k, DecorationPair( Inner, Outermost ), Outermost )
            }
          )
        )
      }

    }


  }

  override def leftwardSplitSpecs( i:Int, j:Int ) = {
    val hObs = lexString( j )

    (1 to hObs.length).flatMap{ hK =>

      val (hStem, hSuffix) = hObs.splitAt( hK )
      if( i == 0 ) {
        Seq(
          (
            StemSuffixDecoration( hStem, hSuffix, Outermost ),
            ( (i+1) to (j-2) by 2 ).flatMap{ k =>
              val dObs = lexString( k )

              leftArcSpecHelper( k, hStem, hSuffix, dObs )
              // ( k, DecorationPair( Outermost, Inner ), Outermost )
            }
          )
        )
      } else {
        Seq(
          (
            StemSuffixDecoration( hStem, hSuffix, Outermost ),
            ( (i+1) to (j-2) by 2 ).flatMap{ k =>
              val dObs = lexString( k )
              leftArcSpecHelper( k, hStem, hSuffix, dObs )
              // ( k, DecorationPair( Outermost, Inner), Outermost )
            }
          ),
          (
            StemSuffixDecoration( hStem, hSuffix, Inner ),
            ( (i+1) to (j-2) by 2 ).flatMap{ k =>
              val dObs = lexString( k )
              leftArcSpecHelper( k, hStem, hSuffix, dObs )
              // ( k, DecorationPair( Outermost, Inner ), Outermost )
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
      val (lStem, lSuffix ) = lObs.splitAt( lK )
      (1 to rObs.length).flatMap{ rK =>
        val (rStem, rSuffix ) = rObs.splitAt( rK )
        Seq(
          (
            DecorationPair(
              StemSuffixDecoration( lStem, lSuffix, Outermost ),
              StemSuffixDecoration( rStem, rSuffix, Inner )
            ),
            ks
          ),
          (
            DecorationPair(
              StemSuffixDecoration( lStem, lSuffix, Inner ),
              StemSuffixDecoration( rStem, rSuffix, Outermost )
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
      val (stem, suffix) = w.splitAt( wK )
      vs.map{ v =>
        StemSuffixDecoration( stem, suffix, v )
      }
    }

  }

  override def lexCellFactor( index:Int, pDec:Decoration ) = {
    // println( intString.mkString("{"," ","}") )
    val head = intString( index )
    val score = 
      if( index%2 == 0 ) {
        theta( StopEvent( head, LeftAtt, pDec, Stop ) )
      } else {
        theta( StopEvent( head, RightAtt, pDec, Stop ) )
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

