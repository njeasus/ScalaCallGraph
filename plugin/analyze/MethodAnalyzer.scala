package dataflow.plugin.analyze

import dataflow.DumbLogger

import scala.reflect.api.Trees
import scala.tools.nsc.Global

trait MethodAnalyzer {
  val l = new DumbLogger("[MA]\t", true)
  val global: Global
  var methodName: String = _
  val thisName: String

  import global._

  def addDirectRead(name: String)
  def addDirectWrite(name: String)
  def addDirectCall(name: String)
  def addIndirectRead(name: String, via: String)
  def addIndirectWrite(name: String, via: String)
  def addIndirectCall(name: String, via: String)

  final def analyze(t: Trees#DefDef) = {
    l.log(s"Started analyzing method $methodName")
    t.rhs.foreach(analyzeTree)
  }

  private def analyzeTree(t: Trees#Tree): Unit = {
    t match {
      case Assign(Select(className, variable), rhs) =>
        l.log(s"\tFound direct variable assignment on line ${className.pos.line} to $className $variable from $rhs")
        addDirectWrite(fqn(className.symbol.fullName, variable))
        analyzeTree(rhs)
      case Apply(Select(obj, variableAccessor), rhs) if (variableAccessor.decoded.endsWith("_=")) =>
        l.log(s"\tFound direct variable assignment with _= on line ${obj.pos.line} to ${obj} with type ${obj.tpe} vs $thisName ${variableAccessor.decoded.replace("_=", "")} $rhs")
        val objTpe = obj.tpe.toString()
        val variable = variableAccessor.decoded.replace("_=", "")
        if(thisName != objTpe) {
          val objName = obj.toString
          addIndirectWrite(fqn(objTpe, variable), objName)
        } else {
          addDirectWrite(fqn(obj, variable))
        }
        rhs.foreach(analyzeTree)
      case Apply(s @ Select(obj, method), rhs) if(notAccessor(s)) =>
        l.log(s"\tFound method call on line ${obj.pos.line} to ${obj} ${method.decoded}")
        val objTpe = obj.tpe.toString()
        val methodName = method
        if(thisName != objTpe) {
          val objName = obj.toString.replace("()", "")
          addIndirectCall(fqn(objTpe, methodName), objName)
        } else {
          addDirectCall(fqn(obj, methodName))
        }
        rhs.foreach(analyzeTree)
      case Apply(s @ Select(obj, method), rhs) if(isAccessor(s)) =>
        l.log(s"\tFound accessor method call on line ${obj.pos.line} to ${obj} ${method.decoded}")
        addDirectRead(fqn(obj, method))
        rhs.foreach(analyzeTree)
      case i @ Ident(s) =>
        //l.log(s"\tFound simple ident access on line ${i.pos.line} to ${s}")
        // TODO: consider reading of incoming variable?
      case Function(params, body) =>
        l.log(s"\tFound function def on line ${body.pos.line} $body with params $params")
        analyzeTree(body)
      case a @ Assign(Ident(variable), rhs) =>
        l.log(s"\tFound assignment on line ${a.pos.line} from $variable to $rhs")
        addDirectWrite(variable.toString)
      case x => //l.log(s"\tUnmatched on line ${x.pos.line} with type ${x.getClass.getName} symbol: $x")
    }
  }

  private def notAccessor(t: Tree): Boolean = !t.symbol.hasAccessorFlag
  private def isAccessor(t: Tree): Boolean = t.symbol.hasAccessorFlag

  private def fqn(c: Any, n: Any) = s"${c}.$n"
}
