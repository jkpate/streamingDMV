package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.NOPOSArcFactoredParameters

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

trait InfiniteFirstOrderFoldUnfoldNOPOSParser[P<:NOPOSArcFactoredParameters] extends FirstOrderFoldUnfoldNOPOSParser[P] {


  override def rootCellFactor( k:Int, rDec:DecorationPair ) = {
    val rObs = lexString( k )
    theta( RootEvent( intString( k ), rObs ) )
  }

  override def rightwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( i )
    val dep = intString( k )
    val depLex = lexString( k )

    myTimes(
      theta( ChooseEvent( head, RightAtt, dep, depLex ) ),
        theta( StopEvent( head, RightAtt, pDec, NotStop ) )
    )
  }

  override def leftwardCellFactor( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration, cDec:Decoration ) = {
    val head = intString( j )
    val dep = intString( k )
    val depLex = lexString( k )

        // println( s"---" )
        // println( (head, dep, depLex ) )
        // println(
        //   theta( ChooseEvent( head, LeftAtt, dep, depLex ) )
        // )
        // println(
        //   theta( StopEvent( head, LeftAtt, pDec, NotStop ) )
        // )

    myTimes(
      theta( ChooseEvent( head, LeftAtt, dep, depLex ) ),
        theta( StopEvent( head, LeftAtt, pDec, NotStop ) )
    )
  }



  override def rootEventCounts( k:Int, marginal:Double ) = {
    val r = intString( k )
    val rObs = lexString( k )

    Seq(
      ( RootEvent( r, rObs ), marginal )
    )
  }
  override def rightwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( i )
    val dep = intString( k )
    val depLex = lexString( k )

    Seq(
      ( ChooseEvent( head, RightAtt, dep, depLex ), marginal ),
      ( StopEvent( head, RightAtt, pDec, NotStop ), marginal )
    )
  }

  override def leftwardEventCounts( i:Int, k:Int, j:Int, pDec:Decoration, mDec:MDecoration,
    cDec:Decoration, marginal:Double ) = {
    val head = intString( j )
    val dep = intString( k )
    val depLex = lexString( k )

    Seq(
      ( ChooseEvent( head, LeftAtt, dep, depLex ), marginal ),
      ( StopEvent( head, LeftAtt, pDec, NotStop ), marginal )
    )
  }

}

