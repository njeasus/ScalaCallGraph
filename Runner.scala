package dataflow

import java.net.URLClassLoader

import dataflow.plugin.collect.AccessCollector

/**
  * Created by njeasus on 10/22/16.
  */
object Runner extends App {
  override def main(a: Array[String]) = {
//    println(Thread.currentThread().getContextClassLoader().asInstanceOf[URLClassLoader].getURLs().mkString(":").replace("file:", ""))
    scala.tools.nsc.Main.process(a)
    println(s"Result:\n${AccessCollector.toString}")
  }
}

// -Xprint: <phases>
// -Xshow-phases
//    phase name  id  description
//    ----------  --  -----------
//        parser   1  parse source into ASTs, perform simple desugaring
//         namer   2  resolve names, attach symbols to named trees
//packageobjects   3  load package objects
//         typer   4  the meat and potatoes: type the trees
//        patmat   5  translate match expressions
//superaccessors   6  add super accessors in traits and nested classes
//    extmethods   7  add extension methods for inline classes
//       pickler   8  serialize symbol tables
//     refchecks   9  reference/override checking, translate nested objects
// dataflowgraph  10
//       uncurry  11  uncurry, translate function values to anonymous classes
//     tailcalls  12  replace tail calls by jumps
//    specialize  13  @specialized-driven class and method specialization
// explicitouter  14  this refs to outer pointers
//       erasure  15  erase types, add interfaces for traits
//   posterasure  16  clean up erased inline classes
//      lazyvals  17  allocate bitmaps, translate lazy vals into lazified defs
//    lambdalift  18  move nested functions to top level
//  constructors  19  move field definitions into constructors
//       flatten  20  eliminate inner classes
//         mixin  21  mixin composition
//       cleanup  22  platform-specific cleanups, generate reflective calls
//    delambdafy  23  remove lambdas
//         icode  24  generate portable intermediate code
//           jvm  25  generate JVM bytecode
//      terminal  26  the last phase during a compilation run

class DumbLogger(prefix: String, doLog: Boolean = true) {
  def log(line: String) = if(doLog) println(s"$prefix $line")
}