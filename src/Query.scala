package queries

import ch.epfl.data.autolifter.annotations._
import scala.collection.mutable.ArrayBuffer

object Query {
  def apply[T](in: ArrayBuffer[T]): Query[T] = new Query(in)
}
@deep
class Query[T](val table: ArrayBuffer[T]) {

  def scan(pred: T => Boolean): Query[T] = {
    val builder: ArrayBuffer[T] = ArrayBuffer()
    table foreach { value =>
      if (pred(value)) builder += value
    }
    Query(builder)
  }

  def map[U](mapF: T => U): Query[U] = {
    val builder: ArrayBuffer[U] = ArrayBuffer()
    table foreach { value =>
      builder += mapF(value)
    }
    Query(builder)
  }

  override def toString: String =
    table.mkString("Query(", ", ", ")")
}