package queries
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

trait QueryOps extends Base with OverloadHack { this: QueryComponent =>
  object Query {
    def apply[T](in: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]): Rep[Query[T]] = apply_obj[T](in)(manifestT)
  }
  implicit class QueryRep[T](self: Rep[Query[T]])(implicit manifestT: Manifest[T]) {
    def scan(pred: (Rep[T] => Rep[Boolean])): Rep[Query[T]] = queryScan[T](self, pred)(manifestT)
    def map[U](mapF: (Rep[T] => Rep[U]))(implicit manifestU: Manifest[U]): Rep[Query[U]] = queryMap[T, U](self, mapF)(manifestT, manifestU)
    def table: Rep[ArrayBuffer[T]] = query_Field_Table[T](self)(manifestT)
  }
  def queryNew[T](table: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]): Rep[Query[T]]
  def queryScan[T](self: Rep[Query[T]], pred: ((Rep[T]) => Rep[Boolean]))(implicit manifestT: Manifest[T]): Rep[Query[T]]
  def queryMap[T, U](self: Rep[Query[T]], mapF: ((Rep[T]) => Rep[U]))(implicit manifestT: Manifest[T], manifestU: Manifest[U]): Rep[Query[U]]
  def query_Field_Table[T](self: Rep[Query[T]])(implicit manifestT: Manifest[T]): Rep[ArrayBuffer[T]]
  def apply_obj[T](in: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]): Rep[Query[T]]
  type Query[T]

}
trait QueryExp extends QueryOps with BaseExp with EffectExp with VariablesExp with LoweringTransformer with ArrayBufferOps with ArrayBufferOpsExp { this: QueryComponent =>
  // case classes
  case class QueryNew[T](table: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]) extends Def[Query[T]] {
    val m0 = manifest[T]
  }

  case class QueryScan[T](self: Rep[Query[T]], pred: ((Rep[T]) => Rep[Boolean]))(implicit manifestT: Manifest[T]) extends Def[Query[T]] {
    val m0 = manifest[T]
  }

  case class QueryMap[T, U](self: Rep[Query[T]], mapF: ((Rep[T]) => Rep[U]))(implicit manifestT: Manifest[T], manifestU: Manifest[U]) extends Def[Query[U]] {
    val m0 = manifest[T]
    val m1 = manifest[U]
  }

  case class Apply[T](in: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]) extends Def[Query[T]] {
    val m0 = manifest[T]
  }

  case class Query_Field_Table[T](self: Rep[Query[T]])(implicit manifestT: Manifest[T]) extends Def[ArrayBuffer[T]] {
    val m0 = manifest[T]
  }

  // method definitions
  // lowered
  def queryScan[T](self: Rep[Query[T]], pred: ((Rep[T]) => Rep[Boolean]))(implicit manifestT: Manifest[T]): Rep[Query[T]] = QueryScan[T](self, pred).atPhase(lowering) {
    implicit def sc: SourceContext = new SourceContext {}; {
      val builder: this.Rep[ArrayBuffer[T]] = ArrayBuffer.apply();

      lowering(self).table.foreach(((value: this.Rep[T]) => __ifThenElse(pred.apply(value), builder.+=(value), unit(unit(())))));
      Query.apply(builder)
    }
  }
  def queryMap[T, U](self: Rep[Query[T]], mapF: ((Rep[T]) => Rep[U]))(implicit manifestT: Manifest[T], manifestU: Manifest[U]): Rep[Query[U]] = QueryMap[T, U](self, mapF).atPhase(lowering) {
    implicit def sc: SourceContext = new SourceContext {}; {
      val builder: this.Rep[ArrayBuffer[U]] = ArrayBuffer.apply();
      lowering(self).table.foreach(((value: this.Rep[T]) => builder.+=(mapF.apply(value))));
      Query.apply(builder)
    }
  }
  def apply_obj[T](in: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]): Rep[Query[T]] = Apply[T](in)
  def query_Field_Table[T](self: Rep[Query[T]])(implicit manifestT: Manifest[T]): Rep[ArrayBuffer[T]] = Query_Field_Table[T](self)
  // def
  def queryNew[T](table: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]): Rep[Query[T]] = QueryNew[T](table)
  override def mirror[A: Manifest](e: Def[A], f$: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case QueryScan(self, pred) => QueryScan(f$(self), pred)
    case Reflect(e @ QueryScan(self, pred), u, es) => reflectMirrored(Reflect(QueryScan(f$(self), pred)(e.m0), mapOver(f$, u), f$(es)))(mtype(manifest[A]))
    case QueryMap(self, mapF) => QueryMap(f$(self), mapF)
    case Reflect(e @ QueryMap(self, mapF), u, es) => reflectMirrored(Reflect(QueryMap(f$(self), mapF)(e.m0, e.m1), mapOver(f$, u), f$(es)))(mtype(manifest[A]))
    case Apply(in) => Apply(f$(in))
    case Reflect(e @ Apply(in), u, es) => reflectMirrored(Reflect(Apply(f$(in))(e.m0), mapOver(f$, u), f$(es)))(mtype(manifest[A]))
    case Query_Field_Table(self) => Query_Field_Table(f$(self))
    case Reflect(e @ Query_Field_Table(self), u, es) => reflectMirrored(Reflect(Query_Field_Table(f$(self))(e.m0), mapOver(f$, u), f$(es)))(mtype(manifest[A]))
    case _ =>
      super.mirrorDef(e, f$)
  })
  override type Query[T] = queries.Query[T]
}
trait QueryScalaGen extends ScalaGenEffect {
  val IR: QueryComponent
  import IR._

  override def emitNode(sym: Sym[Any], node: Def[Any]): Unit = node match {
    case QueryNew(table) => {
      stream.print("val " + quote(sym) + " = new Query")
      stream.print("(")
      stream.print(quote(table))
      stream.print(")")
      stream.println("")
    }
    case Apply(in) => {
      stream.print("val " + quote(sym) + " = " + "queries.Query.apply")
      stream.print("(")
      stream.print(quote(in))
      stream.print(")")
      stream.println("")
    }
    case Query_Field_Table(self) => {
      stream.print("val " + quote(sym) + " = " + quote(self) + ".table")
      stream.println("")
    }
    case _ => super.emitNode(sym, node)
  }
}
trait QueryImplicits { this: QueryComponent =>
  // Add implicit conversions here!
}
trait QueryComponent extends QueryExpOpt with QueryImplicits
trait QueryDSL extends QueryComponent with LMSCore { self =>
  val codegen = new QueryScalaGen with LMSCoreGen { val IR: self.type = self }
  val optimizer = new QueryTransformer { val IR: self.type = self }
}

trait QueryTransformer extends ForwardTransformer {
  val IR: QueryComponent with LMSCore
  import IR._

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(sym, qm @ QueryMap(Def(QueryScan(query, pred)), mapF)) =>
      val ab = IR.ArrayBuffer[Any]()(qm.m1)
      query.table.foreach { value =>
        __ifThenElse(pred(value), ab += mapF(value), lift(()))
      }
      IR.Query(ab)
    case _ => super.transformStm(stm)
  }
}
trait QueryExpOpt extends QueryExp with LMSCore { this: QueryComponent =>
  override def queryNew[T](table: Rep[ArrayBuffer[T]])(implicit manifestT: Manifest[T]): Rep[Query[T]] = {
    /* add local optimizations here */
    super.queryNew[T](table)(manifestT)
  }
  override def queryScan[T](self: Rep[Query[T]], pred: ((Rep[T]) => Rep[Boolean]))(implicit manifestT: Manifest[T]): Rep[Query[T]] = {
    self match {
      case Def(QueryScan(slf, pred1)) =>
        queryScan(slf, (x: Rep[T]) => pred1(x) && pred(x))

      case _ => super.queryScan[T](self, pred)(manifestT)
    }
  }
  override def queryMap[T, U](self: Rep[Query[T]], mapF: ((Rep[T]) => Rep[U]))(implicit manifestT: Manifest[T], manifestU: Manifest[U]): Rep[Query[U]] = {
    /* add local optimizations here */
    super.queryMap[T, U](self, mapF)(manifestT, manifestU)
  }
  override def query_Field_Table[T](self: Rep[Query[T]])(implicit manifestT: Manifest[T]): Rep[ArrayBuffer[T]] = {
    /* add local optimizations here */
    super.query_Field_Table[T](self)(manifestT)
  }

}
