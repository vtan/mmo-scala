package mmo.client.network

import mmo.common.api.{Constants, PlayerEvent, Pong}

import com.sksamuel.avro4s.AvroInputStream
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable.ArrayBuffer

final case class EventReceiverStats(count: Int, totalSize: Int)

trait EventReceiver {
  def poll(): PlayerEvent
  val lastPingNanos: AtomicLong
  def clearStats(): EventReceiverStats
}

private[network] class EventReceiverRunnable(
  inputStream: InputStream
) extends Runnable with EventReceiver {

  private val eventQueue: BlockingQueue[PlayerEvent] = new ArrayBlockingQueue[PlayerEvent](256)
  private val eventReader: EventReader = new EventReader
  private val eventSizeBuffer = ArrayBuffer.empty[Int]

  override val lastPingNanos: AtomicLong = new AtomicLong(0L)

  override def poll(): PlayerEvent = eventQueue.poll()

  override def clearStats(): EventReceiverStats = {
    val stats = EventReceiverStats(eventSizeBuffer.size, eventSizeBuffer.sum)
    eventSizeBuffer.clear()
    stats
  }

  override def run(): Unit =
    try {
      while (true) {
        val (event, size) = eventReader.read(inputStream)
        event match {
          case Pong(clientTimeNanos) => lastPingNanos.set(System.nanoTime() - clientTimeNanos)
          case event => eventQueue.add(event)
        }
        if (eventSizeBuffer.size < 100) {
          eventSizeBuffer += size
        }
      }
    } catch {
      case _: Throwable => ()
    }
}

private class EventReader {

  private val payloadBuffer: Array[Byte] = Array.fill(Constants.maxMessageBytes)(0.toByte)
  private val payloadInputStream = new ByteArrayInputStream(payloadBuffer)
  private val avroInputStream = AvroInputStream.binary[PlayerEvent].from(payloadInputStream).build(PlayerEvent.avroSchema)

  def read(inputStream: InputStream): (PlayerEvent, Int) = {
    val size = ByteBuffer.wrap(inputStream.readNBytes(4)).getInt
    if (size > payloadBuffer.length) {
      throw new RuntimeException(s"Received too large command: $size bytes")
    }
    val start = payloadBuffer.length - size
    val _ = inputStream.readNBytes(payloadBuffer, start, size)
    payloadInputStream.reset()
    payloadInputStream.skip(start.toLong)
    avroInputStream.iterator.next() -> size
  }
}
