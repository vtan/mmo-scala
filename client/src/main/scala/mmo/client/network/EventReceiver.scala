package mmo.client.network

import mmo.common.api.{Constants, PlayerEvent, Pong}

import com.sksamuel.avro4s.AvroInputStream
import java.io.{ByteArrayInputStream, InputStream}
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}
import java.util.concurrent.atomic.AtomicLong

trait EventReceiver {
  def poll(): PlayerEvent
  val lastPingNanos: AtomicLong
}

private[network] class EventReceiverRunnable(
  inputStream: InputStream
) extends Runnable with EventReceiver {

  private val eventQueue: BlockingQueue[PlayerEvent] = new ArrayBlockingQueue[PlayerEvent](256)
  private val eventReader: EventReader = new EventReader

  override val lastPingNanos: AtomicLong = new AtomicLong(0L)

  override def poll(): PlayerEvent = eventQueue.poll()

  override def run(): Unit =
    try {
      while (true) {
        eventReader.read(inputStream) match {
          case Pong(clientTimeNanos) => lastPingNanos.set(System.nanoTime() - clientTimeNanos)
          case event => eventQueue.add(event)
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

  def read(inputStream: InputStream): PlayerEvent = {
    val size = ByteBuffer.wrap(inputStream.readNBytes(4)).getInt
    if (size > payloadBuffer.length) {
      throw new RuntimeException(s"Received too large command: $size bytes")
    }
    val start = payloadBuffer.length - size
    val _ = inputStream.readNBytes(payloadBuffer, start, size)
    payloadInputStream.reset()
    payloadInputStream.skip(start.toLong)
    avroInputStream.iterator.next()
  }
}
