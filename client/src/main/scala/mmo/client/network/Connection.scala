package mmo.client.network

import java.net.Socket
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Future, Promise}

class Connection(
  val eventReceiver: EventReceiver,
  val commandSender: CommandSender
) {
  private val connected: AtomicBoolean = new AtomicBoolean(true)

  def isConnected: Boolean = connected.get
}

object Connection {

  def apply(host: String, port: Int): Future[Connection] = {
    val promise = Promise[Connection]()
    val thread = new Thread(
      () => {
        try {
          val socket = new Socket(host, port)
          socket.setTcpNoDelay(true)
          val inputStream = socket.getInputStream
          val outputStream = socket.getOutputStream

          val eventReceiver = new EventReceiverRunnable(inputStream)
          val eventReceiverThread = new Thread(eventReceiver, "connection-event-receiver")
          eventReceiverThread.setDaemon(true)
          eventReceiverThread.start()
          val commandSender = new CommandSenderRunnable(outputStream)
          val commandSenderThread = new Thread(commandSender, "connection-command-sender")
          commandSenderThread.setDaemon(true)
          commandSenderThread.start()

          val connection = new Connection(eventReceiver, commandSender)
          promise.success(connection)

          eventReceiverThread.join()
          commandSenderThread.join()
          connection.connected.set(false)
        } catch {
          case ex: Throwable => promise.failure(ex)
        }
        ()
      },
      "connection"
    )
    thread.setDaemon(true)
    thread.start()
    promise.future
  }
}
