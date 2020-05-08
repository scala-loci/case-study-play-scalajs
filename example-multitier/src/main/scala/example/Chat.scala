package example

import loci._
import loci.transmitter.rescala._
import loci.serializer.upickle._
import loci.communicator.ws.akka._
import rescala.default._
import scalatags.JsDom.all._
import org.scalajs.dom
import org.scalajs.jquery.{ jQuery => $ }
import scala.collection.mutable
import java.net.URI
import com.google.common.html.HtmlEscapers
import models.User
import models.ChatRoom
import models.ChatMessage
import models.ChatUserStore

@multitier object Chat {
  @peer type Server <: { type Tie <: Multiple[Client] }
  @peer type Client <: { type Tie <: Single[Server] }

  val assetsDir: Local[String] on Client
  val store: Local[ChatUserStore] on Server
  val room: Local[ChatRoom] on Server

  val maxMessages = 20

  val userMessage: Evt[String] on Client = Evt[String]

  val systemMessage: Local[Evt[ChatMessage]] on Server = Evt[ChatMessage]

  val errorMessage: Local[Evt[(Remote[Client], String)]] on Server = Evt[(Remote[Client], String)]

  val message = on[Server] sbj { remote: Remote[Client] =>
    room.robotMessage ||
    systemMessage ||
    errorMessage
      .collect { case (client, message) if client == remote =>
        ChatMessage(None, message)
      } ||
    userMessage.asLocalFromAllSeq
      .map { case (client, message) =>
        store.get(username(client)).map { user =>
          ChatMessage(Some(user), HtmlEscapers.htmlEscaper.escape(message).replace("\n", "<br/>"))
        }
      }
      .collect { case Some(message) => message }
  }

  def createMessage(message: String, name: String, avatar: String) = on[Client] local {
    val ownname = username(remote[Server].connected)
    div(`class`:=s"row message-box${ if(name == ownname) "-me" else "" }")(
      div(`class`:="col-md-2")(
        div(`class`:="message-icon")(
          img(src:=s"$assetsDir/images/avatars/$avatar", `class`:="img-rounded"),
          div(name)
        )
      ),
      div(`class`:="col-md-10")(raw(message))
    )
  }

  on[Client] {
    $("#message").keypress((e: dom.KeyboardEvent) => {
      if(!e.shiftKey && e.keyCode == 13) {
        e.preventDefault()
        userMessage.fire($("#message").value().toString)
        $("#message").value("")
      }
    })

    message.asLocal observe {
      case ChatMessage(Some(User(name, avatar)), message) =>
        val messages = dom.document.getElementById("messages")
        messages.appendChild(createMessage(message, name, avatar).render)
        if(messages.childNodes.length >= maxMessages){
          messages.removeChild(messages.firstChild)
        }
        messages.scrollTop = messages.scrollHeight

      case ChatMessage(None, message) =>
        dom.window.alert(message)
    }
  }

  def username(remote: Remote[_]) =
    remote.protocol match {
      case WS(url, _, _) => new URI(url).getPath.substring("/chat/ws/".size)
      case _ => ""
    }

  on[Server] {
    remote[Client].joined observe { remote =>
      room.setupRobot()

      val name = username(remote)
      val user = store.get(name)

      if (name.nonEmpty && user.isEmpty) {
        val user = User(name, room.randomAvatar)
        store.save(user, remote)
        systemMessage.fire(ChatMessage(Some(user), "has entered the room"))
      }
      else {
        errorMessage.fire((remote, "This username is already used"))
        remote.disconnect()
      }
    }

    remote[Client].left observe { remote =>
      store.get(username(remote), remote).map { user =>
        systemMessage.fire(ChatMessage(Some(user), "has left the room"))
        store.remove(user.name)
      }
    }
  }
}

object ChatInitialization {
  private val servers = mutable.Map.empty[ChatUserStore, WebSocketHandler]

  def server(chatUserStore: ChatUserStore, chatRoom: ChatRoom) = servers getOrElseUpdate (chatUserStore, {
    val webSocket = WebSocketHandler()
    multitier start new Instance[Chat.Server](listen[Chat.Client] { webSocket }) {
      val store = chatUserStore
      val room = chatRoom
    }
    webSocket
  })

  def client(url: String, assetsDirectory: String) = {
    multitier start new Instance[Chat.Client](connect[Chat.Server] { WS(url) }) {
      val assetsDir = assetsDirectory
    }
  }
}
