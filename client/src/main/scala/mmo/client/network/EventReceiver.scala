package mmo.client.network

import mmo.common.api.{PlayerEvent, Pong}

import com.sksamuel.avro4s.AvroInputStream
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import java.util.concurrent.atomic.AtomicLong
import scala.util.{Failure, Success, Try}

trait EventReceiver {
  def poll(): PlayerEvent
  def take(): PlayerEvent
  val lastPingNanos: AtomicLong
}

class EventReceiverRunnable(
  inputStream: InputStream
) extends Runnable with EventReceiver {

  private val eventQueue: BlockingQueue[PlayerEvent] = new ArrayBlockingQueue[PlayerEvent](256)

  override val lastPingNanos: AtomicLong = new AtomicLong(0L)

  override def poll(): PlayerEvent =
    eventQueue.poll()

  override def take(): PlayerEvent =
    eventQueue.take()

  override def run(): Unit =
    try {
      while (true) {
        deserializeEvent(inputStream) match {
          case Success(Pong(clientTimeNanos)) => lastPingNanos.set(System.nanoTime() - clientTimeNanos)
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
