import ch.epfl.yinyang._
import ch.epfl.yinyang.typetransformers._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

package object queries {

  def query[T](block: => T): T = macro impl.query[T]

  object impl {
    def query[T](c: Context)(block: c.Expr[T]): c.Expr[T] = {
      YYTransformer[c.type, T](c)(
        "queries.QueryDSL",
        new RepTransformer[c.type](c),
        None,
        None,
        Map())(block)
    }
  }
}