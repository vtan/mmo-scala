package mmo.client.network

import mmo.common.api.PlayerCommand

import com.sksamuel.avro4s.AvroOutputStream
import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue}

trait CommandSender {
  def offer(command: PlayerCommand): Unit
}

private[network] class CommandSenderRunnable(
  outputStream: OutputStream
) extends Runnable with CommandSender {

  private val commandQueue: BlockingQueue[PlayerCommand] = new ArrayBlockingQueue[PlayerCommand](256)
  private val commandWriter: CommandWriter = new CommandWriter

  override def offer(command: PlayerCommand): Unit = {
    val _ = commandQueue.offer(command)
    ()
  }

  override def run(): Unit =
    try {
      while (true) {
        val command = commandQueue.take()
        commandWriter.write(command, outputStream)
      }
    } catch {
      case _: Throwable => ()
    }
}

private class CommandWriter {

  private val byteArrayOutputStream = new ByteArrayOutputStream()
  private val sizeBuffer = ByteBuffer.allocate(4)
  private val avroOutputStream = AvroOutputStream.binary[PlayerCommand].to(byteArrayOutputStream).build()

  def write(command: PlayerCommand, outputStream: OutputStream): Unit = {
    byteArrayOutputStream.reset()
    val _ = sizeBuffer.clear()
    avroOutputStream.write(command)
    avroOutputStream.flush()
    sizeBuffer.putInt(byteArrayOutputStream.size())
    outputStream.write(sizeBuffer.array())
    byteArrayOutputStream.writeTo(outputStream)
  }
}
