package dataflow.plugin.collect

import scala.collection.mutable

object AccessCollector {
  val accesses: mutable.Map[String, ClassDescriptor] = mutable.Map.empty

  def addClass(name: String) = synchronized {
    accesses += name -> ClassDescriptor(name)
  }

  def apply[A](className: String)(f: (ClassDescriptor) => A) = synchronized {
    f(accesses(className))
  }

  override def toString() = accesses.values.mkString("\n")
}
