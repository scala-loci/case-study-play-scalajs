package example

import loci._
import loci.rescalaTransmitter._
import loci.serializable.upickle._
import loci.ws.akka._
import rescala._
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

@multitier
class Chat(assetsDir: => String, store: => ChatUserStore, room: => ChatRoom) {
  trait Server extends Peer { type Tie <: Multiple[Client] }
  trait Client extends Peer { type Tie <: Single[Server] }

  val maxMessages = 20

  val userMessage: Evt[String] on Client = Evt[String]

  val systemMessage: Evt[ChatMessage] localOn Server = Evt[ChatMessage]

  val errorMessage: Evt[(Remote[Client], String)] localOn Server = Evt[(Remote[Client], String)]

  val message = placed[Server].sbj { implicit! => remote: Remote[Client] =>
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

  def createMessage(message: String, name: String, avatar: String) = placed[Client].local { implicit! =>
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

  placed[Client] { implicit! =>
    $("#message").keypress((e: dom.KeyboardEvent) => {
      if(!e.shiftKey && e.keyCode == 13) {
        e.preventDefault()
        userMessage fire $("#message").value().toString
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

  placed[Client].terminating { implicit! =>
    $("#message").off()
  }

  def username(remote: Remote[_]) =
    remote.protocol match {
      case WS(url, _, _) => new URI(url).getPath.substring("/chat/ws/".size)
      case _ => ""
    }

  placed[Server] { implicit! =>
    remote[Client].joined observe { remote =>
      room.setupRobot

      val name = username(remote)
      val user = store.get(name)

      if (name.nonEmpty && user.isEmpty) {
        val user = User(name, room.randomAvatar)
        store.save(user, remote)
        systemMessage fire ChatMessage(Some(user), "has entered the room")
      }
      else {
        errorMessage fire ((remote, "This username is already used"))
        remote.disconnect()
      }
    }

    remote[Client].left observe { remote =>
      store.get(username(remote), remote).map { user =>
        systemMessage fire ChatMessage(Some(user), "has left the room")
        store.remove(user.name)
      }
    }
  }
}

object Chat {
  private val servers = mutable.Map.empty[ChatUserStore, WebSocketHandler]

  def server(store: ChatUserStore, room: ChatRoom) = servers getOrElseUpdate (store, {
    val webSocket = WebSocketHandler()
    val chat = new Chat(???, store, room)
    multitier setup new chat.Server { def connect = listen[chat.Client] { webSocket } }
    webSocket
  })

  def client(url: String, assetsDir: String) = {
    val chat = new Chat(assetsDir, ???, ???)
    multitier setup new chat.Client { def connect = request[chat.Server] { WS(url) } }
  }
}
