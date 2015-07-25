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

object printDependencyParse {
  def apply( arcs: Set[DirectedArc] ) = {
    arcs.toList.sortBy( _.dIdx ).map{_.hIdx}.mkString("[ ",", "," ]")
  }
}

