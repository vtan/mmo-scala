package mmo.client.network

import mmo.common.PlayerCommand

import com.sksamuel.avro4s.AvroOutputStream
import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

trait CommandSender {
  def offer(command: PlayerCommand): Unit
}

class CommandSenderRunnable(
  outputStream: OutputStream
) extends Runnable with CommandSender {

  private val commandQueue: BlockingQueue[PlayerCommand] = new ArrayBlockingQueue[PlayerCommand](256)

  override def offer(command: PlayerCommand): Unit = {
    val _ = commandQueue.offer(command)
    ()
  }

  override def run(): Unit =
    try {
      while (true) {
        val command = commandQueue.take()
        writeCommand(command)
      }
    } catch {
      case _: Throwable => () // TODO handle disconnect if not quitting
    }

  private def writeCommand(command: PlayerCommand): Unit = {
    val payloadStream = new ByteArrayOutputStream()
    val avro = AvroOutputStream.binary[PlayerCommand].to(payloadStream).build()
    avro.write(command)
    avro.close()
    val sizeBuffer = ByteBuffer.allocate(4)
    sizeBuffer.putInt(payloadStream.size)
    outputStream.write(sizeBuffer.array())
    payloadStream.writeTo(outputStream)
  }
}
