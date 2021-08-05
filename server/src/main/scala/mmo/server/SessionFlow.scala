package mmo.server

import mmo.common.api.{Constants, PlayerCommand, PlayerEvent}

import akka.actor.typed.ActorRef
import akka.stream.scaladsl.{Flow, Framing, Source}
import akka.util.ByteString
import akka.NotUsed
import akka.stream.{Materializer, OverflowStrategy}
import akka.stream.typed.scaladsl.ActorSink
import com.sksamuel.avro4s.{AvroInputStream, AvroOutputStream}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.UUID
import scala.concurrent.duration._

object SessionFlow {

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

    val eventSerializer = new EventSerializer
    val commandDeserializer = new CommandDeserializer

    Flow[ByteString]
      .via(Framing.simpleFramingProtocolDecoder(Constants.maxMessageBytes))
      .map(commandDeserializer.deserialize)
      .initialTimeout(timeout = 5.seconds)
      .statefulMapConcat(() => {
        var handshakeDone = false
        command => {
          if (handshakeDone) {
            List(command)
          } else {
            command match {
              case PlayerCommand.InitiateSession(magic) if magic == PlayerCommand.InitiateSession.magic =>
                handshakeDone = true
                gameActor.tell(GameActor.Connected(id, queue))
                Nil
              case _ => throw new RuntimeException("Handshake failed")
            }
          }
        }
      })
      .via(Flow.fromSinkAndSourceCoupled(incomingSink, outgoingSource))
      .map(eventSerializer.serialize)
      .via(Framing.simpleFramingProtocolEncoder(Constants.maxMessageBytes))
  }

  private class EventSerializer {
    private val byteArrayOutputStream = new ByteArrayOutputStream()
    private val avroOutputStream = AvroOutputStream.binary[PlayerEvent].to(byteArrayOutputStream).build()

    def serialize(event: PlayerEvent): ByteString = {
      byteArrayOutputStream.reset()
      avroOutputStream.write(event)
      avroOutputStream.flush()
      ByteString(byteArrayOutputStream.toByteArray)
    }
  }

  private class CommandDeserializer {
    private val payloadBuffer: Array[Byte] = Array.fill(Constants.maxMessageBytes)(0.toByte)
    private val payloadInputStream = new ByteArrayInputStream(payloadBuffer)
    private val avroInputStream = AvroInputStream.binary[PlayerCommand].from(payloadInputStream).build(PlayerCommand.avroSchema)

    def deserialize(byteString: ByteString): PlayerCommand = {
      val start = payloadBuffer.length - byteString.size
      byteString.copyToArray(payloadBuffer, start)
      payloadInputStream.reset()
      payloadInputStream.skip(start.toLong)
      avroInputStream.iterator.next()
    }
  }
}
