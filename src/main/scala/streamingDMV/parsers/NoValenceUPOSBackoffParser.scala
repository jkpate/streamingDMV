// package streamingDMV.parsers
// 
// import streamingDMV.labels._
// import streamingDMV.parameters.NoValenceUPOSBackoffParameters
// 
// import breeze.linalg._
// import breeze.numerics._
// 
// import scala.collection.mutable.{Map=>MMap}
// 
// 
// class NoValenceUPOSBackoffParser(
//   maxLength:Int,
//   rootAlpha:Double = 1D,
//   stopAlpha:Double = 1D,
//   chooseAlpha:Double = 1D,
//   backoffAlpha:Double = 1D,
//   notBackoffAlpha:Double = 10D,
//   uposCount:Int,
//   randomSeed:Int = 15
// ) extends FirstOrderFoldUnfoldUPOSParser[NoValenceUPOSBackoffParameters](
//   maxLength, rootAlpha, stopAlpha, chooseAlpha, uposCount, randomSeed
// ) {
// 
//   val theta =
//     new NoValenceUPOSBackoffParameters(
//       rootAlpha,
//       stopAlpha,
//       chooseAlpha,
//       backoffAlpha,
//       notBackoffAlpha,
//       uposCount
//     )
// 
//   def leftArcParentVs( i:Int ) = Set[Decoration]( NoValence )
//   def rightArcParentVs( j:Int ) = Set[Decoration]( NoValence )
// 
//   def lexMarginals( index:Int ) = Seq()
// 
//   def mNodeParentVs( i:Int, j:Int ) = Set( PlainM )
// 
//   def zerosMatrix = DenseMatrix.zeros[Double]( uposCount, uposCount )
//   def zerosVector = DenseVector.zeros[Double]( uposCount )
// 
//   val insideHeads:Array[Array[MMap[Decoration,DenseVector[Double]]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
//     if( ( i%2 != j%2 ) ) {
//       MMap( NoValence -> zerosVector )
//     } else {
//       MMap[Decoration,DenseVector[Double]]()
//     }
//   )
//   // One of the m-node children is a head-child, so they can't both be outermost
//   val insideM:Array[Array[MMap[MDecoration,DenseMatrix[Double]]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
//     if( i%2 == 1 && j%2 == 1 ) {
//       MMap( PlainM -> zerosMatrix )
//     } else {
//       MMap[MDecoration,DenseMatrix[Double]]()
//     }
//   )
// 
//   val outsideHeads:Array[Array[MMap[Decoration,DenseVector[Double]]]] = Array.tabulate( 2*maxLength, (2*maxLength)+1 )( (i,j) =>
//     if( ( i%2 != j%2 ) ) {
//       MMap( NoValence -> zerosVector )
//     } else {
//       MMap[Decoration,DenseVector[Double]]()
//     }
//   )
//   // One of the m-node children is a head-child, so they can't both be outermost
//   val outsideM:Array[Array[MMap[MDecoration,DenseMatrix[Double]]]] = Array.tabulate( 2*maxLength, 2*maxLength )( (i,j) =>
//     if( i%2 == 1 && j%2 == 1 ) {
//       MMap( PlainM -> zerosMatrix )
//     } else {
//       MMap[MDecoration,DenseMatrix[Double]]()
//     }
//   )
// 
//   def lexFill( index:Int ) {
//     val head = intString( index )
//     insideHeads(index)(index+1)( NoValence ) = DenseVector.ones[Double](uposCount)
//   }
// 
//   def findLeftRootChild( k:Int, rootPos:Int ) = 
//     headTrace( 0 )( k )( NoValence, rootPos )
//   def findRightRootChild( k:Int, rootPos:Int ) = 
//     headTrace( k )( intString.length )( NoValence, rootPos )
// 
//   def findLeftLeftwardChild( i:Int, k:Int, dPos:Int ) =
//     headTrace( i )( k )( NoValence, dPos )
//   def findRightLeftwardChild( k:Int, j:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
//     mTrace( k )( j )( PlainM, dPos, hPos )
// 
//   def findLeftRightwardChild( i:Int, k:Int, hV:Decoration, mDV:Decoration, hPos:Int, dPos:Int ) =
//     mTrace( i )( k )( PlainM, hPos, dPos )
//   def findRightRightwardChild( k:Int, j:Int, dPos:Int ) =
//     headTrace( k )( j )( NoValence, dPos )
// 
//   def findLeftMChild( i:Int, k:Int, decoration:MDecoration, lPos:Int ) =
//     headTrace( i )( k )( NoValence, lPos )
//   def findRightMChild( k:Int, j:Int, decoration:MDecoration, rPos:Int ) =
//     headTrace( k )( j )( NoValence, rPos )
// 
//   def rootFactor( k:Int ) = {
//     val r = intString( k )
//     theta( RootEvent() ) :*
//     theta.rootLex( r ) :*
//       theta( StopEvent( LeftAtt, Stop ) ) :*
//       theta( StopEvent( RightAtt, Stop ) )
//   }
// 
//   def backoffChooseLexEvent( head:Int, dir:AttDir, dep:Int ) = {
//     theta.chooseLex( ChooseEvent( dir, dep ) ) :*
//       theta( LambdaChooseEvent( head, dir, Backoff ) )
//   }
//   def notBackoffChooseLexEvent( head:Int, dir:AttDir, dep:Int ) = {
//     theta.chooseLex( ChooseEvent( head, dir, dep ) ) :*
//       theta( LambdaChooseEvent( head, dir, NotBackoff ) )
//   }
//   def chooseLex( head:Int, dir:AttDir, dep:Int ) = {
//     backoffChooseLexEvent( head, dir, dep ) :+
//       notBackoffChooseLexEvent( head, dir, dep )
//   }
// 
//   def backoffChoosePosEvent( head:Int, dir:AttDir ) = {
//     theta( ChooseEvent( dir ) ) :*
//       theta( LambdaEvent( head, dir, Backoff ) )
//   }
//   def notBackoffChoosePosEvent( head:Int, dir:AttDir ) = {
//     theta( ChooseEvent( head, dir ) ) :*
//       theta( LambdaEvent( head, dir, NotBackoff ) )
//   }
//   def choosePos( head:Int, dir:AttDir ) = {
//     backoffChoosePosEvent( head, dir ) :+
//       notBackoffChoosePosEvent( head, dir )
//   }
// 
//   def chooseFactor( head:Int, dir:AttDir, dep:Int ) = {
//     choosePos( head:Int, dir:AttDir )(*,::) :+ chooseLex( head, dir, dep )
//   }
// 
//   def arcFactor( h:Int, dir:AttDir, dep:Int ) = {
//     val factor = theta( ChooseEvent( head, dir, dep ) )
// 
//     factor :*= (
//       (
//         theta( StopEvent( dep, dir.flip, NoValence, Stop ) ) :*
//         theta( StopEvent( dep, dir, NoValence, Stop ) )
//       ).t *
//       theta( StopEvent( head, dir, NoValence, NotStop ) )
//     )
// 
//     factor
//   }
// 
//   def populateRootCell( k:Int ) {
//     val r = intString( k )
// 
//     stringProb +=
//       sum( 
//         insideHeads( 0 )( k )( NoValence ) :*
//           insideHeads( k )( intString.length )( NoValence ) :*
//             rootFactor( k ).toDenseVector
//       )
//   }
// 
//   def populateRightwardCell( i:Int, k:Int, j:Int ) {
//     val head = intString( i )
//     val dep = intString( k )
//     insideHeads( i )( j )( NoValence ) :+= (
//           insideHeads( k )( j )( NoValence ).t * (
//         insideM( i )( k )( PlainM ).t :* arcFactor( head, RightAtt, dep )
//       )
//     ).t
//   }
// 
// 
// 
// 
// }
// 
// 
