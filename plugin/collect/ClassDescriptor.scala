package dataflow.plugin.collect

import scala.collection.mutable

case class ClassDescriptor(val className: String) {
  case class Access(
                     val directReads: mutable.Set[String]= mutable.HashSet.empty,
                     val directWrites: mutable.Set[String] = mutable.HashSet.empty,
                     val directCalls: mutable.Set[String] = mutable.HashSet.empty,
                     val indirectReads: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty,
                     val indirectWrites: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty,
                     val indirectCalls: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty
                   ) {
    def addRead(tpe: String, t: AccessType): Unit = t match {
      case Direct => directReads += normalizeName(tpe)
      case Indirect(via) => addIndirect(indirectReads, via, normalizeName(tpe))
    }
    def addWrite(tpe: String, t: AccessType): Unit = t match {
      case Direct => directWrites += normalizeName(tpe)
      case Indirect(via) => addIndirect(indirectWrites, via, normalizeName(tpe))
    }
    def addCall(tpe: String, t: AccessType): Unit = t match {
      case Direct => directCalls += normalizeName(tpe)
      case Indirect(via) => addIndirect(indirectCalls, via, normalizeName(tpe))
    }

    private def addIndirect(set: mutable.Map[String, mutable.Set[String]], name: String, accessor: String) = {
      set.getOrElseUpdate(name, mutable.Set.empty) += accessor
    }

    override def toString() = {
      def indirectToString(m: mutable.Map[String, mutable.Set[String]]) = s"${m.map { case(k, v) => s"via [$k] -> [${v.mkString(", ")}]"}.mkString("\n\t\t\t", "\n\t\t\t", "")}"

      val b = new mutable.StringBuilder()
      if(directReads.nonEmpty) b ++= s"\n\t\t[Direct Reads]: ${directReads.mkString("\n\t\t\t", "\n\t\t\t", "")}"
      if(directWrites.nonEmpty) b ++= s"\n\t\t[Direct Writes]: ${directWrites.mkString("\n\t\t\t", "\n\t\t\t", "")}"
      if(directCalls.nonEmpty) b ++= s"\n\t\t[Direct Calls]: ${directCalls.mkString("\n\t\t\t", "\n\t\t\t", "")}"
      if(indirectReads.nonEmpty) b ++= s"\n\t\t[Indirect Reads]: ${indirectToString(indirectReads)}"
      if(indirectWrites.nonEmpty) b ++= s"\n\t\t[Indirect Writes]: ${indirectToString(indirectWrites)}"
      if(indirectCalls.nonEmpty) b ++= s"\n\t\t[Indirect Calls]: ${indirectToString(indirectCalls)}"

      b.toString()
    }
  }

  private def normalizeName(n: String) = {
    n.trim
  }

  val defAccesses: mutable.Map[String, Access] = mutable.Map.empty

  def addRead(function: String, tpe: String, access: AccessType) = synchronized {
    defAccesses.getOrElseUpdate(function, Access()).addRead(tpe, access)
  }

  def addWrite(function: String, tpe: String, access: AccessType) = synchronized {
    defAccesses.getOrElseUpdate(function, Access()).addWrite(tpe, access)
  }

  def addCall(function: String, tpe: String, access: AccessType) = synchronized {
    defAccesses.getOrElseUpdate(function, Access()).addCall(tpe, access)
  }

  override def toString() = s"Class [$className] \n\t${defAccesses.mkString(s"\n\t")}"
}

abstract class AccessType
case object Direct extends AccessType
case class Indirect(val via: String) extends AccessType