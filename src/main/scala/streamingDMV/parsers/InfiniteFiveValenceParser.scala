package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.{FiveValenceParameters,InfiniteParameters}

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class InfiniteFiveValenceParser(
  parserSpec:ParserSpec
) extends FiveValenceParser(
  parserSpec
) with InfiniteFirstOrderFoldUnfoldNOPOSParser[FiveValenceParameters] {

  override val theta = new FiveValenceParameters(
    parserSpec.toParameterSpec
  ) with InfiniteParameters
}


