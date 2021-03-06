package example

import scala.scalajs.js
import scala.scalajs.js.URIUtils
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.jquery.{ jQuery => $ }
import scalatags.JsDom.all._
import scala.concurrent.ExecutionContext.Implicits.global

@JSExport
object ChatJS {
  var assetsDir: String = ""
  var wsBaseUrl: String = ""

  var client: Option[loci.Runtime[Chat.Client]] = None

  def signInPanel = div(id:="signInPanel"){
    form(`class`:="form-inline", role:="form")(
      div(id:="usernameForm", `class`:="form-group")(
        div(`class`:="input-group")(
          div(`class`:="input-group-addon", raw("&#9786;")),
          input(id:="username", `class`:="form-control", `type`:="text", placeholder:="Enter username")
        )
      ),
//      span(style:="margin:0px 5px"),
//      select(id:="channel", `class`:="form-control")(
//        option(value:="0", "WebSocket"), option(value:="1", "Server-Sent Events")),
      span(style:="margin:0px 5px"),
      button(`class`:="btn btn-default", onclick:={ () =>
        val input = $("#username").value().toString.trim
        if(input == "") {
          $("#usernameForm").addClass("has-error")
          dom.window.alert("Invalid username")
        }else{
          $("#usernameForm").removeClass("has-error")
          $("#loginAs").text(s"Login as: $input")
          $("#username").value("")
          $("#signInPanel").addClass("hide")
          $("#chatPanel").removeClass("hide")

          client = Some(connect(input))

          client.map(_.terminated.foreach { _ =>
            $("#signInPanel").removeClass("hide")
            $("#chatPanel").addClass("hide")
            $("#messages").html("")
          })
        }
        false
      })("Sign in")
    )
  }

  def chatPanel = div(id:="chatPanel", `class`:="hide")(
    div(`class`:="row", style:="margin-bottom: 10px;")(
      div(`class`:="col-md-12", style:="text-align: right;")(
        span(id:="loginAs", style := "padding: 0px 10px;"),
        button(`class`:="btn btn-default", onclick:={ () =>
          client.map(_.terminate())
        }, "Sign out")
      )
    ),
    div(`class` := "panel panel-default")(
      div(`class` := "panel-heading")(
        h3(`class` := "panel-title")("Chat Room")
      ),
      div(`class` := "panel-body")(
        div(id := "messages")
      ),
      div(`class` := "panel-footer")(
        textarea(id:="message", `class` := "form-control message", placeholder := "Say something")
      )
    )
  )

  def connect(username: String) =
    ChatInitialization.client(wsBaseUrl + URIUtils.encodeURIComponent(username), assetsDir)

  @JSExport
  def main(settings: js.Dynamic) = {
    assetsDir = settings.assetsDir.toString
    wsBaseUrl = settings.wsBaseUrl.toString

    val content = dom.document.getElementById("content")
    content.appendChild(signInPanel.render)
    content.appendChild(chatPanel.render)
  }
}


//package example
//
//import config.Routes
//import org.scalajs.dom.ext.Ajax
//import org.scalajs.dom.raw.EventSource
//import scala.scalajs.js
//import scala.scalajs.js.annotation.JSExport
//import js.Dynamic.{ global => g }
//import org.scalajs.dom
//import scalatags.JsDom._
//import all._
//import org.scalajs.jquery.{jQuery=>$}
//
//@JSExport
//object ChatJS {
//
//  val maxMessages = 20
//
//  var assetsDir: String = ""
//  var wsBaseUrl: String = ""
//
//  var client: Option[ChatClient] = None
//
//  def signInPanel = div(id:="signInPanel"){
//    form(`class`:="form-inline", role:="form")(
//      div(id:="usernameForm", `class`:="form-group")(
//        div(`class`:="input-group")(
//          div(`class`:="input-group-addon", raw("&#9786;")),
//          input(id:="username", `class`:="form-control", `type`:="text", placeholder:="Enter username")
//        )
//      ),
//      span(style:="margin:0px 5px"),
//      select(id:="channel", `class`:="form-control")(
//        option(value:="0", "WebSocket"), option(value:="1", "Server-Sent Events")),
//      span(style:="margin:0px 5px"),
//      button(`class`:="btn btn-default", onclick:={ () =>
//        val input = $("#username").value().toString.trim
//        if(input == "") {
//          $("#usernameForm").addClass("has-error")
//          dom.window.alert("Invalid username")
//        }else{
//          $("#usernameForm").removeClass("has-error")
//          client = ChatClient.connect(wsBaseUrl, input).map{ c =>
//            $("#loginAs").text(s"Login as: ${c.username}")
//            $("#username").value("")
//            $("#signInPanel").addClass("hide")
//            $("#chatPanel").removeClass("hide")
//            c
//          }
//        }
//        false
//      })("Sign in")
//    )
//  }
//
//  def chatPanel = div(id:="chatPanel", `class`:="hide")(
//    div(`class`:="row", style:="margin-bottom: 10px;")(
//      div(`class`:="col-md-12", style:="text-align: right;")(
//        span(id:="loginAs", style := "padding: 0px 10px;"),
//        button(`class`:="btn btn-default", onclick:={ () =>
//          singOut
//        }, "Sign out")
//      )
//    ),
//    div(`class` := "panel panel-default")(
//      div(`class` := "panel-heading")(
//        h3(`class` := "panel-title")("Chat Room")
//      ),
//      div(`class` := "panel-body")(
//        div(id := "messages")
//      ),
//      div(`class` := "panel-footer")(
//        textarea(id:="message", `class` := "form-control message", placeholder := "Say something")
//      )
//    )
//  )
//
//  def createMessage(msg: String, username: String, avatar: String) = {
//    div(`class`:=s"row message-box${if(username == client.map(_.username).getOrElse(""))"-me" else ""}")(
//      div(`class`:="col-md-2")(
//        div(`class`:="message-icon")(
//          img(src:=s"$assetsDir/images/avatars/${avatar}", `class`:="img-rounded"),
//          div(username)
//        )
//      ),
//      div(`class`:="col-md-10")(raw(msg))
//    )
//  }
//
//  def singOut = {
//    client.map(_.close())
//    $("#signInPanel").removeClass("hide")
//    $("#chatPanel").addClass("hide")
//    $("#messages").html("")
//  }
//
//  trait ChatClient {
//
//    val username: String
//
//    def send(msg: String)
//
//    def close()
//  }
//
//  object ChatClient {
//
//    def connect(url: String, username: String): Option[ChatClient] = {
//      try {
//        if ($("#channel").value().toString == "0") {
//          if (g.window.WebSocket.toString != "undefined") {
//            Some(new WSChatClient(url, username))
//          } else None
//        } else {
//          if (g.window.EventSource.toString != "undefined") {
//            Some(new SSEChatClient(username))
//          } else None
//        }
//      }catch{
//        case e: Throwable => {
//          dom.window.alert("Unable to connect because "+e.toString)
//          None
//        }
//      }
//    }
//
//    def receive(e: dom.MessageEvent) = {
//      val msgElem = dom.document.getElementById("messages")
//      val data = js.JSON.parse(e.data.toString)
//      dom.console.log(data)
//      if(data.error.toString != "undefined"){
//        dom.window.alert(data.error.toString)
//        singOut
//      }else{
//        val user = data.user.name.toString
//        val avatar = data.user.avatar.toString
//        val message = data.message.toString
//        msgElem.appendChild(createMessage(message, user, avatar).render)
//        if(msgElem.childNodes.length >= maxMessages){
//          msgElem.removeChild(msgElem.firstChild)
//        }
//        msgElem.scrollTop = msgElem.scrollHeight
//      }
//    }
//  }
//
//  class WSChatClient(url: String, val username: String) extends ChatClient {
//
//    val socket = new dom.WebSocket(url + username)
//    socket.onmessage = ChatClient.receive _
//
//    override def send(msg: String): Unit = {
//      val json = js.JSON.stringify(js.Dynamic.literal(text=$("#message").value()))
//      socket.send(json)
//    }
//
//    override def close() = socket.close()
//
//  }
//
//  class SSEChatClient(val username: String) extends ChatClient {
//    import common.ExtAjax._
//    val sse = new EventSource(Routes.Chat.connectSSE(username))
//    sse.onmessage = ChatClient.receive _
//
//    def encode(value: String) = js.URIUtils.encodeURIComponent(value)
//
//    override def send(msg: String): Unit = {
//      Ajax.postAsForm(Routes.Chat.talk, s"username=${encode(username)}&msg=${encode(msg)}")
//    }
//
//    override def close() = sse.close()
//
//  }
//
//  def ready = {
//    $("#message").keypress((e: dom.KeyboardEvent) => {
////      dom.console.log(e)
//      if(!e.shiftKey && e.keyCode == 13) {
//        e.preventDefault()
//        client.map{_.send($("#message").value().toString)}
//        $("#message").value("")
//      }
//    })
//  }
//
//  @JSExport
//  def main(settings: js.Dynamic) = {
//    this.assetsDir = settings.assetsDir.toString
//    this.wsBaseUrl = settings.wsBaseUrl.toString
//
//    val content = dom.document.getElementById("content")
//    content.appendChild(signInPanel.render)
//    content.appendChild(chatPanel.render)
//    ready
//  }
//
//}
