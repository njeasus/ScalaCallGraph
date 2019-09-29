package dataflow.plugin

import dataflow.plugin.analyze.ClassAnalyzer

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class DataFlowGraph(val global: Global) extends Plugin {
  import global._

  val name = "dataflowgraph"
  val description = "Generates data-flow information"
  val components = List[PluginComponent](Component)

  private object Component extends PluginComponent {
    val global: DataFlowGraph.this.global.type = DataFlowGraph.this.global

    type Tree = this.global.Tree

    val runsAfter = List("delambdafy")

    val phaseName = DataFlowGraph.this.name
    def newPhase(_prev: Phase) = new DataFlowGraphPhase(_prev)

    class DataFlowGraphPhase(prev: Phase) extends StdPhase(prev) {
      override def name = DataFlowGraph.this.name

      def apply(unit: CompilationUnit) {
        unit.body.children.foreach(runOnClasses)
//        println(unit.body)
      }

      def runOnClasses(tree: Tree): Unit = tree match {
        case c: ClassDef => {
          val a = new ClassAnalyzer(c, global)
          a.analyze()
        }
        case Assign(Select(x, y), _) =>
        case x => x.children.foreach(runOnClasses)
      }

    }
  }

}
