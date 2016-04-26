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

  def apply( lexString:Boolean, annotDouble:Boolean, datasets:List[Array[String]]* ):Seq[List[Utt]] = {
    datasets.map{ strings =>
      strings.map{ str =>
        if( annotDouble) {
          val tokens = str.tail.map{_.split("#")}
          Utt(
            str.head,
            tokens.map{ w =>
              dictionary.getOrElseUpdate( w(1), dictionary.size )
            },
            if( lexString )
              str.tail
            else
              Array(),
            tokens.map{ _(0).toDouble }
          )
        } else {
          Utt(
            str.head,
            str.tail.map{ w =>
              dictionary.getOrElseUpdate( w, dictionary.size )
            },
            if( lexString )
              str.tail
            else
              Array()
          )
        }
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
      if( buildLargeMB ) {
        // miniBatches = remainingStrings.take( largeMiniBatchSize ) :: miniBatches
        miniBatches :+= remainingStrings.take( largeMiniBatchSize )
        remainingStrings = remainingStrings.drop( largeMiniBatchSize )
        buildLargeMB = false
      } else {
        val totalUtts =
          if( largeMiniBatchEvery > 0 )
            largeMiniBatchEvery * miniBatchSize
          else
            remainingStrings.size

        miniBatches ++= remainingStrings.slice(0,totalUtts).grouped( miniBatchSize ).toList
        remainingStrings = remainingStrings.drop( totalUtts )
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

object printMorphs {
  def apply( morphs: Seq[Tuple3[Int,String,String]] ) = {
    morphs.sortBy( _._1 ).map{ m => s"${m._2}+${m._3}" }.mkString("< "," "," >" )
  }
}

