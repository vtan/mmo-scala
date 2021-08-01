package mmo.server

import akka.actor.typed.ActorRef
import akka.stream.scaladsl.{Flow, Framing, Source}
import akka.util.ByteString
import akka.NotUsed
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.typed.scaladsl.ActorSink
import com.sksamuel.avro4s.{AvroInputStream, AvroOutputStream}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.UUID
import mmo.common.api.{PlayerCommand, PlayerEvent}

import scala.util.{Failure, Success, Try}

object SessionFlow {
  private val maxMessageBytes = 512

  def create(gameActor: ActorRef[GameActor.Message])(implicit mat: Materializer): Flow[ByteString, ByteString, NotUsed] = {
    val id = UUID.randomUUID()

    val (queue, outgoingSource) = Source.queue[PlayerEvent](
      bufferSize = 32,
      overflowStrategy = OverflowStrategy.fail
    ).preMaterialize()

    val incomingSink = ActorSink.actorRef[GameActor.Message](
      ref = gameActor,
      onCompleteMessage = GameActor.Disconnected(id),
      onFailureMessage = _ => GameActor.Disconnected(id)
    ).contramap(command => GameActor.PlayerCommandReceived(id, command))

    gameActor.tell(GameActor.Connected(id, queue))

    Flow[ByteString]
      .via(Framing.simpleFramingProtocolDecoder(maxMessageBytes))
      .map { bytes =>
        deserializeCommand(bytes) match {
          case Success(command) => command
          case Failure(ex) =>
            mat.system.log.error(ex, "Failed to deserialized player command")
            throw ex
        }
      }
      .via(Flow.fromSinkAndSourceCoupled(incomingSink, outgoingSource))
      .map(serializeEvent)
      .via(Framing.simpleFramingProtocolEncoder(maxMessageBytes))
  }

  private def deserializeCommand(byteString: ByteString): Try[PlayerCommand] = {
    val bytes = new ByteArrayInputStream(byteString.toArray)
    val avro = AvroInputStream.binary[PlayerCommand].from(bytes).build(PlayerCommand.avroSchema)
    val result = avro.tryIterator.toSeq match {
      case result +: _ => result
      case _ => Failure(new RuntimeException("Empty command message"))
    }
    avro.close()
    result
  }

  private def serializeEvent(event: PlayerEvent): ByteString = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val avro = AvroOutputStream.binary[PlayerEvent].to(byteArrayOutputStream).build()
    avro.write(event)
    avro.close()
    ByteString(byteArrayOutputStream.toByteArray)
  }
}
