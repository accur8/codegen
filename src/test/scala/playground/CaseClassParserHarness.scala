package playground

import a8.codegen.{CaseClassParser, CompanionGen, FastParseTools}
import a8.codegen.FastParseTools.{ParserConfig, Source}

import java.io.File

object CaseClassParserHarness extends App {

  val sourceCode =
    """package a8.remoteapi.api

import fs2.{Chunk, Pipe}
import fs2.text.utf8Encode
import java.util.UUID

import scala.concurrent.duration.FiniteDuration
import a8.common.ApiImports._
import a8.remoteapi.api.Mxprotocol.{MxCreateMailboxRequest, MxCreateMailboxResponse, MxInitialMessage, MxMessage, MxPubPrivPair, MxSendMessageRequest, MxSendMessageResponse, MxSentMessage}
import a8.shared.CompanionGen
import a8.shared.json.JsonCodec
import a8.shared.json.ast.{JsDoc, JsNull, JsVal}
//import a8.wsjdbc.core.MessagePackAssist
import a8.shared.{StringValue}
import a8.shared.StringValue.StringValueCompanion
import org.typelevel.ci.CIString
import wvlet.airframe.msgpack.spi.{Packer, Unpacker}

object protocol {

  object impl {
    def randomId(segmentCount: Int): String =
      (1 to segmentCount)
        .map { _ =>
          UUID.randomUUID.toString.replace("-", "")
        }
        .mkString("")
  }

  object PublicMailboxId extends StringValueCompanion[PublicMailboxId] {
    val empty = PublicMailboxId("")
    def create() = new PublicMailboxId(impl.randomId(1))
  }

  case class PublicMailboxId(value: String) extends StringValue {
//    def toProto = proto.PublicMailboxId(value)
  }

  object PrivateMailboxId extends StringValueCompanion[PrivateMailboxId] {
    def create() = new PrivateMailboxId(impl.randomId(2))
  }

  case class PrivateMailboxId(value: String) extends StringValue {
//    def toProto = proto.PrivateMailboxId(value)
  }

  object PubPrivPair extends MxPubPrivPair {
  }

  @CompanionGen
  case class PubPrivPair(
    publicId: PublicMailboxId,
    privateId: PrivateMailboxId,
  )

  object CreateMailboxRequest extends MxCreateMailboxRequest {
  }

  @CompanionGen
  case class CreateMailboxRequest(
    inactivityTimeout: Option[FiniteDuration] = None
  )

  object CreateMailboxResponse extends MxCreateMailboxResponse {
  }

  @CompanionGen
  case class CreateMailboxResponse(
    publicId: PublicMailboxId,
    privateId: PrivateMailboxId,
  ) {
    def asPubPriv = PubPrivPair(publicId, privateId)
  }


  object SendMessageRequest extends MxSendMessageRequest {
  }

  @CompanionGen
  case class SendMessageRequest(
    from: PrivateMailboxId,
    to: Vector[PublicMailboxId] = Vector.empty,
    bcc: Vector[PublicMailboxId] = Vector.empty,
    headers: Vector[(String, String)] = Vector.empty,
    body: JsDoc = JsDoc.empty,
    schema: Option[String] = None,
    encoding: Option[String] = None,
    correlation: Option[UUID] = None,
  )

  object SendMessageResponse extends MxSendMessageResponse {
    val empty = SendMessageResponse(Vector.empty, Vector.empty, Vector.empty)

    def apply(th: Throwable): SendMessageResponse =
      SendMessageResponse(
        errors = Vector(th.stackTraceAsString)
      )

  }

  @CompanionGen
  case class SendMessageResponse(
    successes: Vector[PublicMailboxId] = Vector.empty,
    failures: Vector[PublicMailboxId] = Vector.empty,
    errors: Vector[String] = Vector.empty,
    messageSize: Int = 0,
    correlation: Option[UUID] = None,
  ) {
    def +(right: SendMessageResponse) =
      copy(
        successes = successes ++ right.successes,
        failures = failures ++ right.failures,
        errors = errors ++ right.errors,
        messageSize = messageSize + right.messageSize,
      )
  }

  case class CancelMethodRequest(
    privateId: PrivateMailboxId,
    handle: JsVal
  )

  case class CancelMethodResponse(
    privateId: PrivateMailboxId,
    handle: JsVal
  )


  object Message extends MxMessage {
  }

  @CompanionGen
  case class Message(
    from: PublicMailboxId,
    to: Vector[PublicMailboxId],
    headers: Vector[(String, String)] = Vector.empty,
    body: JsDoc = JsDoc.empty,
    schema: Option[String] = None,
    encoding: Option[String] = None,
    timestamp: Option[Long] = None,
//    topicOffset: Option[TopicOffset] = None,
    correlation: Option[UUID] = None,
  ) {
    lazy val headersMap = headers.toMap
  }


  object HttpRequest {
    case class Header(name: String, value: String)
  }

  case class HttpHeader(name: String, value: String)

  case class HttpRequest(
    url: String,
    method: String,
    protocol: String,
    headers: Iterable[HttpHeader],
    body: String,
  )

  object SentMessage extends MxSentMessage {
  }
  @CompanionGen
  case class SentMessage(
    start: Long,
    finished: Long,
    request: SendMessageRequest,
    response: SendMessageResponse
  )

  object websocket {

    object InitialMessage extends MxInitialMessage {
    }

    @CompanionGen
    case class InitialMessage(
      publicId: PublicMailboxId,
      privateId: PrivateMailboxId,
      readOffset: Option[ReadOffset] = None,
      heartbeatPeriod: Option[FiniteDuration] = None,
    ) {
      def resolvedReadOffset = readOffset.getOrElse(ReadOffset.First)
    }
  }

  sealed trait ReadOffset

  object TopicOffset {
    val Empty = TopicOffset(0)
    val First = TopicOffset(1)
    val Last = TopicOffset(Long.MaxValue)

    implicit val jsonCodec = JsonCodec.long.dimap[TopicOffset](TopicOffset.apply, _.value)

  }
  case class TopicOffset(value: Long) extends ReadOffset with Ordered[TopicOffset] {

    def isEmpty = value == 0

    def nextOffset = TopicOffset(value+1)

    override def compare(that: TopicOffset): Int = value.compareTo(that.value)

  }

  case object ReadOffset {

    implicit val jsonCodec =
      JsonCodec.string
          .dimap[ReadOffset](
            s => parse(s) match {
              case Left(msg) =>
                sys.error(msg)
              case Right(ro) =>
                ro
            },
            _ match {
              case First => "first"
              case Last => "last"
              case Tail => "tail"
              case TopicOffset(v) => v.toString
            }
          )

    case object First extends ReadOffset
    case object Last extends ReadOffset
    case object Tail extends ReadOffset

    def parse(readOffsetStr: String): Either[String, ReadOffset] = {
      val readOffsetStrL = readOffsetStr.toLowerCase.trim
      readOffsetStrL match {
        case "first" =>
          Right(First)
        case "last" =>
          Right(Last)
        case "tail" =>
          Right(Tail)
        case _ =>
          readOffsetStrL.toLongOption match {
            case Some(l) =>
              Right(TopicOffset(l))
            case None =>
              Left(s"unable to parse ${readOffsetStr} into a ReadOffset expected (first | last | tail | <whole_number>)")
          }
      }
    }

  }

}

      """.trim

  val file = new File(".")
  lazy val parser = new CaseClassParser(file, (_,_) => CompanionGen.empty)(ParserConfig(true))

  lazy val sourceFile = FastParseTools.parse(Source(sourceCode, file.getPath), parser.SourceFile)

  sourceFile.toString

}
