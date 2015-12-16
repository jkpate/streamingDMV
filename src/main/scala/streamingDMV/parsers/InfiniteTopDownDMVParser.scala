package streamingDMV.parsers

import streamingDMV.labels._
import streamingDMV.parameters.{TopDownDMVParameters,InfiniteParameters}

import scala.collection.mutable.{Map=>MMap}
import scala.math.log

class InfiniteTopDownDMVParser(
  parserSpec:ParserSpec
) extends TopDownDMVParser(
  parserSpec
) with InfiniteFirstOrderFoldUnfoldNOPOSParser[TopDownDMVParameters] {

  override val theta = new TopDownDMVParameters(
    parserSpec.toParameterSpec
  ) with InfiniteParameters
}

