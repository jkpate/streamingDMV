package streamingDMV.io

import streamingDMV.labels.{Utt,DirectedArc}

object readStrings {
  def apply( filePath:String ) = {
    io.Source.fromFile( filePath ).getLines.toList.map{ line =>
      line.split( "\\s+" )
    }
  }
}

object stringsToUtts {
  val dictionary = collection.mutable.HashMap[String,Int]()

  def apply( datasets:List[Array[String]]* ):Seq[List[Utt]] = {
    datasets.map{ strings =>
      strings.map{ str =>
        Utt(
          str.head,
          str.tail.map{ w =>
            dictionary.getOrElseUpdate( w, dictionary.size )
          }
        )
      }
    }
  }

}

object buildMiniBatches {
      // def apply(
      //   corpus:List[Utt],
      //   initialMiniBatchSize:Int,
      //   miniBatchSize:Int,
      //   batchVB:Boolean
      // ) = {

      //   val firstMB = corpus.take( initialMiniBatchSize )

      //   val subsMB =
      //     if( batchVB )
      //       Nil
      //     else
      //       corpus.drop( initialMiniBatchSize ).grouped( miniBatchSize ).toList

      //   firstMB :: subsMB
      // }

  def apply(
    corpus:List[Utt],
    largeMiniBatchSize:Int,
    largeMiniBatchEvery:Int,
    miniBatchSize:Int,
    batchVB:Boolean
  ) = {
    var remainingStrings = corpus

    var miniBatches = Seq[List[Utt]]()

    var buildLargeMB = true
    while( ! remainingStrings.isEmpty ) {
      if(
        buildLargeMB
        // (
        //   largeMiniBatchEvery == 0 &&
        //   miniBatches.size == 0
        // ) || (
        //   largeMiniBatchEvery > 0 &&
        //   miniBatches.size % largeMiniBatchEvery == 0
        // )
      ) {
        // println( "LARGE MINIBATCH" )
        // miniBatches = remainingStrings.take( largeMiniBatchSize ) :: miniBatches
        miniBatches :+= remainingStrings.take( largeMiniBatchSize )
        remainingStrings = remainingStrings.drop( largeMiniBatchSize )
        buildLargeMB = false
      } else {
        // println( "--" )
        // miniBatches = remainingStrings.take( miniBatchSize ) :: miniBatches
        val totalUtts = largeMiniBatchEvery * miniBatchSize
        // println( totalUtts )

        // miniBatches = miniBatches :+ remainingStrings.take( miniBatchSize )
        miniBatches ++= remainingStrings.slice(0,totalUtts).grouped( miniBatchSize ).toList
        // remainingStrings = remainingStrings.drop( miniBatchSize )
        remainingStrings = remainingStrings.drop( totalUtts )
        // println( remainingStrings.length )
        buildLargeMB = true
      }

      if( batchVB )
        remainingStrings = Nil
    }

    miniBatches.toList

  }

}

object printDependencyParse {
  def apply( arcs: Set[DirectedArc] ) = {
    arcs.toList.sortBy( _.dIdx ).map{_.hIdx}.mkString("[ ",", "," ]")
  }
}

