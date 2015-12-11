package scala.virtualization.lms
package common

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util.OverloadHack
import ch.epfl.yinyang.api._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import java.io._

trait LMSYinYang extends BaseYinYangManifest with BaseExp { self =>
  case class Hole[T: TypeRep](symId: Long) extends Def[T] {
    val tpe: TypeRep[T] = manifest[T]
  }

  override def mirrorDef[A: Manifest](e: Def[A], f$: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case h @ Hole(id) => Hole(id)(h.tpe)
    case _ =>
      super.mirrorDef(e, f$)
  }

  override def mirror[A: Manifest](e: Def[A], f$: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case h @ Hole(id) => toAtom(Hole(id)(h.tpe))(h.tpe)
    case _ =>
      super.mirror(e, f$)
  }

  implicit def liftAny[T: TypeRep]: LiftEvidence[T, Rep[T]] =
    new LiftEvidence[T, Rep[T]] {
      def hole(tpe: TypeRep[T], symbolId: Int): Rep[T] = Hole(symbolId)(tpe)
      def lift(v: T): Rep[T] = Const(v)
    }

  def requiredHoles = Nil
}

trait LMSCoreGen extends ScalaGenLoops with ScalaGenEqual with ScalaGenPrimitiveOps with ScalaGenArrayBufferOps with ScalaGenIfThenElse with ScalaGenTupleOps with ScalaGenBooleanOps {
  val IR: LMSCore
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Hole(x) =>
    case _ => super.emitNode(sym, rhs)
  }
  override def quote(x: Exp[Any]): String = x match {
    case Const(x: collection.mutable.ArrayBuffer[_]) => s"""collection.mutable.ArrayBuffer(${x.mkString(", ")})"""
    case _ => super.quote(x)
  }

  def emitSourceYinYang[T: Manifest](f: Exp[T], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val body = reifyBlock(f)

    val syms: List[Sym[_]] = focusBlock(body) {
      innerScope flatMap {
        case TP(sym, rhs) =>
          rhs match {
            case Hole(x) => scala.List(sym)
            case _ => Nil
          }
        case _ => Nil
      }
    }
    println("=========================   Applying Optimizations   =========================")
    val optimizedBody = optimizer.transformBlock(body)
    println("=========================      Applying Lowering     =========================")
    val loweredBody = lowering.run(optimizedBody)
    val res = emitSource(syms, loweredBody, className, stream)

    res
  }

}

trait LMSCore extends LMSYinYang with FullyStaged with CodeGenerator
    with IfThenElse with TupleOpsExp with TupleOps with IfThenElseExpOpt
    with ScalaCompile with IfThenElseExp with Equal
    with BooleanOpsExp with BooleanOps with EqualExpBridge with EqualExp with LoopsExp
    with LoweringTransformer with PrimitiveOpsExp with ArrayBufferOpsExp { self =>
  def main(): Any
  val codegen: LMSCoreGen { val IR: self.type }
  val optimizer: ForwardTransformer { val IR: self.type }

  def generateCode(className: String, unstableHoleIds: scala.Predef.Set[Int]): String = {
    val source = new StringWriter()
    codegen.emitSourceYinYang(main.asInstanceOf[Exp[Any]], className, new PrintWriter(source))
    source.toString
  }
  /*
   * Ret must be Nothing* => T. If I was only smarter to make this work without a convention :/
   */
  def compile[T: TypeRep, Ret](holeIds: scala.Predef.Set[Int]) = {

    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1
    val source = new StringWriter()
    codegen.emitSourceYinYang(main.asInstanceOf[Exp[T]], className, new PrintWriter(source))

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    Predef.println(s"$source")
    run.compileSources(scala.List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    reporter.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    cls.getConstructor().newInstance().asInstanceOf[Ret]
  }
}
