package a8.codegen

import java.io.{PrintWriter, StringWriter}
import java.util.regex.Pattern

import scala.reflect.ClassTag

object CommonOpsCopy
  extends CommonOpsCopy


trait CommonOpsCopy {

  implicit class IterableOps[A, B <: Iterable[A]](xs: B) {
    def toNonEmpty: Option[B] =
      if ( xs.nonEmpty ) Some(xs)
      else None
  }

  def classTag[A](implicit ctag: ClassTag[A]): ClassTag[A] = ctag

  implicit class StringOps(private val s: String) {

    def indent(indent: String) = {
      val result =
        s.linesIterator
          .map(indent + _)
          .mkString("\n")
      result
    }

    def quoted = '\"'.toString + s + '\"'.toString

    def asBlankOpt: Option[String] =
      if ( s.trim.length == 0 ) None
      else Some(s)

    def splitList(regex: String, limit: Int = Integer.MAX_VALUE, trim: Boolean = true, dropEmpty: Boolean = true) = {

      val pattern = Pattern.compile(regex)

      def trimmer(s: String): String = {
        if ( trim ) s.trim
        else s
      }

      def trimDrop(s: String): Option[String] = {
        val t0 = trimmer(s)
        if ( !dropEmpty || t0.length > 0 ) Some(t0)
        else None
      }

      def splitter(input: String, limit: Int): List[String] = {
        if ( limit == 0 ) Nil
        else {
          val splitLimit = math.min(limit,2)
          pattern.split(input, splitLimit) match {
            case Array() => Nil
            case Array(p0) => trimDrop(p0).toList
            case Array(p0, p1) => {
              trimDrop(p0) match {
                case None => splitter(p1, limit)
                case Some(i)  => i :: splitter(p1, limit - 1)
              }
            }
          }
        }
      }

      splitter(s, limit)

    }

  }

  implicit val globalExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  implicit class PimpedThrowable(private val _value: Throwable) {

    def rootCause: Throwable = {
      _value.getCause match {
        case null => _value
        case c if _value == c => _value
        case c => c.rootCause
      }
    }

    def toStringX = getStackTraceAsString

    def getStackTraceAsString = {
      val sw = new StringWriter()
      val pw = new PrintWriter(sw)
      _value.printStackTrace(pw)
      pw.flush()
      pw.close()
      sw.toString
    }

  }

  implicit class OptionOps[A](private val option: Option[A]) {

    def getOrError(msg: String) =
      option match {
        case Some(a) => a
        case None => sys.error(msg)
      }

  }


  implicit class IteratorOps[A](val iter: Iterator[A]) {

    def nextOpt: Option[A] =
      if ( iter.hasNext ) Some(iter.next())
      else None

  }

  def dbgexpr[V](value: sourcecode.Text[V])(implicit enclosing: sourcecode.Enclosing, file: sourcecode.File, location: sourcecode.Line): String = {
    val filename = file.value.lastIndexOf("/") match {
      case -1 => file.value
      case i => file.value.substring(i+1)
    }
    enclosing.value + "@" + filename + ":" + location.value + " (" + value.source + ") = " + value.value
  }

  def throwError(msg: String) = sys.error(msg)

  implicit class StringInterpolations(sc: StringContext) {

    class CaseInsenitiveUnApply {
      def unapply(other: String) =
        sc.parts match {
          case Seq(a) => a.equalsIgnoreCase(other)
          case s => s.mkString.equalsIgnoreCase(other)
        }
    }
    def ci = new CaseInsenitiveUnApply

  }

  implicit class BooleanOps(b: Boolean) {
    def option[A](fn: =>A): Option[A] =
      if ( b ) Some(fn) else None
  }

  def none[A]: Option[A] = None
  def some[A](a: A): Option[A] = Some(a)


}
