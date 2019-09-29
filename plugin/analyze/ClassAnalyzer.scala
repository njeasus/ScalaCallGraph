package dataflow.plugin.analyze

import dataflow.DumbLogger
import dataflow.plugin.collect.{AccessCollector, Direct, Indirect}

import scala.reflect.internal.Trees
import dataflow.plugin.analyze.{MethodAnalyzer => MA}

import scala.tools.nsc.Global

class ClassAnalyzer[A <: Trees#Tree with Trees#ClassDef](val classTree: A, global: Global) {
  import global._

  val l = new DumbLogger("[CA]")
  private val name: String = classTree.symbol.fullName
  AccessCollector.addClass(name)

  l.log(s"Started analyzing class $name at ${classTree.pos}")

  object MethodAnalyzer extends MA {
    val global = ClassAnalyzer.this.global
    val thisName = ClassAnalyzer.this.name
    override def addDirectRead(name: String): Unit =  ClassAnalyzer.this.addDirectRead(methodName, name)
    override def addDirectWrite(name: String): Unit = ClassAnalyzer.this.addDirectWrite(methodName, name)
    override def addDirectCall(name: String): Unit = ClassAnalyzer.this.addDirectCall(methodName, name)
    override def addIndirectRead(name: String, via: String): Unit = ClassAnalyzer.this.addIndirectRead(methodName, name, via)
    override def addIndirectWrite(name: String, via: String): Unit = ClassAnalyzer.this.addIndirectWrite(methodName, name, via)
    override def addIndirectCall(name: String, via: String): Unit = ClassAnalyzer.this.addIndirectCall(methodName, name, via)
  }

  def addDirectRead(method: String, tpe: String) = {
    AccessCollector(name) { a => a.addRead(method, tpe, Direct)}
  }
  def addDirectWrite(method: String, tpe: String) = {
    AccessCollector(name) { a => a.addWrite(method, tpe, Direct)}
  }
  def addDirectCall(method: String, tpe: String) = {
    AccessCollector(name) { a => a.addCall(method, tpe, Direct)}
  }
  def addIndirectRead(method: String, tpe: String, via: String) = {
    AccessCollector(name) { a => a.addRead(method, tpe, Indirect(via))}
  }
  def addIndirectWrite(method: String, tpe: String, via: String) = {
    AccessCollector(name) { a => a.addWrite(method, tpe, Indirect(via))}
  }
  def addIndirectCall(method: String, tpe: String, via: String) = {
    AccessCollector(name) { a => a.addCall(method, tpe, Indirect(via))}
  }

  def analyze(): Unit = {
    classTree.impl.children.foreach {
      case d: DefDef if notAccessor(d)=> {
        MethodAnalyzer.methodName = d.localName.decoded
        MethodAnalyzer.analyze(d)
      }
      case x => //l.log("Unmatched")
    }
  }

  private def notAccessor(d: Trees#DefDef): Boolean = !d.mods.hasAccessorFlag

  private def typeNameTransformer(n: String): String = {
    n.replace(name, "")
  }
}
