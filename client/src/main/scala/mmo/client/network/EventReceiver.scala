package mmo.client.network

import mmo.common.PlayerEvent

import com.sksamuel.avro4s.AvroInputStream
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import scala.util.{Failure, Success, Try}

trait EventReceiver {
  def poll(): PlayerEvent
}

class EventReceiverRunnable(
  inputStream: InputStream
) extends Runnable with EventReceiver {

  private val eventQueue: BlockingQueue[PlayerEvent] = new ArrayBlockingQueue[PlayerEvent](256)

  override def poll(): PlayerEvent =
    eventQueue.poll()

  override def run(): Unit =
    try {
      while (true) {
        deserializeEvent(inputStream) match {
          case Success(event) => eventQueue.add(event)
          case Failure(exception) => exception.printStackTrace()
        }
      }
    } catch {
      case _: Throwable => () // TODO handle disconnect if not quitting
    }

  private def deserializeEvent(inputStream: InputStream): Try[PlayerEvent] = {
    val messageSize = ByteBuffer.wrap(inputStream.readNBytes(4)).getInt
    val bytes = new ByteArrayInputStream(inputStream.readNBytes(messageSize))
    val avro = AvroInputStream.binary[PlayerEvent].from(bytes).build(PlayerEvent.avroSchema)
    val result = avro.tryIterator.toSeq match {
      case result +: _ => result
      case _ => Failure(new RuntimeException("Empty event message"))
    }
    avro.close()
    result
  }
}
