package example

import loci._
import loci.rescalaTransmitter._
import loci.serializable.upickle._
import loci.ws.akka._
import rescala._
import org.scalajs.dom
import scalatags.JsDom.all._
import scala.util.Random
import scala.io.Source
import play.api.mvc.Session
import play.api.mvc.RequestHeader
import common.Framework._
import upickle.default._


case class Hangman(
    level: Int = 0,
    word: String = "",
    guess: List[Char] = Nil,
    misses: Int = 0) {

  def guessWord = {
    for (c <- word.toCharArray) yield {
      if (guess.contains(c)) c
      else '_'
    }
  }

  def gameOver: Boolean = {
    (misses >= level) || won
  }

  def won = {
    (for (c <- word.toCharArray) yield {
      guess.contains(c)
    }).find(i => i == false) == None
  }
}


@multitier
class HangmanGame {
  trait Server extends Peer { type Tie <: Single[Client] }
  trait Client extends Peer { type Tie <: Single[Server] }

  val sessionName: String on Server = "hangman"

  val words: Array[String] localOn Server =
    Source.fromInputStream(getClass.getResourceAsStream("/public/text/words.txt"))
      .mkString.split("[\\s,]+").filter(word => (word.length > 5 && word.forall(Character.isLetter)))

  val rand: Random localOn Server = new Random

  val session = placed[Server].local { implicit! =>
    val request = remote[Client].connected.protocol.identification collect {
      case (request: RequestHeader, _) => request
    }
    request.map(_.session).getOrElse(Session())
  }

  val game = placed[Server] { implicit! =>
    Var(session.get(sessionName).map(read[Hangman](_)))
  }

  def start(level: Int): Unit on Server = placed { implicit! =>
    val word = words(rand.nextInt(words.length)).toUpperCase
    game set Some(Hangman(level, word))
  }

  def guess(g: Char): Unit on Server = placed { implicit! =>
    game.now foreach { hangman =>
      val misses = if (hangman.word.contains(g)) hangman.misses else hangman.misses + 1
      game set Some(hangman.copy(guess = hangman.guess :+ g, misses = misses))
    }
  }

  def giveup(): Unit on Server = placed { implicit! =>
    game set None
  }

  placed[Server] { implicit! =>
    game observe { game =>
      val newSession = game.map { game =>
        session + (sessionName -> write(game))
      }.getOrElse { session - sessionName }

      val cookie = Session.encodeAsCookie(newSession)
      val cookieString = cookie.name + '=' + cookie.value

      remote[Client].capture(cookieString) { implicit! =>
        dom.document.cookie = cookieString
      }
    }
  }

  placed[Client] { implicit! =>
    val content = Signal {
      game.asLocal().map { game =>
        if(game.gameOver) pageResult(game) else pageGuess(game)
      }.getOrElse {
        pagePlay
      }
    }
    dom.document.getElementById("content").appendChild(content.render)
  }

  def pagePlay = placed[Client].local { implicit! =>
    div {
      var currentLevel = 0
      val levels = Array(
        (10, "Easy game; you are allowed 10 misses."),
        (5, "Medium game; you are allowed 5 misses."),
        (3, "Hard game; you are allowed 3 misses.")
      )
      div(
        p("Inspired from ")(a(href:="http://www.yiiframework.com/demos/hangman/", target:="_blank","Yii's demo")),
        p("This is the game of Hangman. You must guess a word, a letter at a time.\n" +
          "If you make too many mistakes, you lose the game!"),
        form(id := "playForm")(
          for ((level, text) <- levels) yield {
            val levelId = s"level_${level}"
            div(`class`:="radio")(
              input(id:=levelId, `type`:="radio", name:="level", onclick:={ () =>
                currentLevel = level
              }, { if (level == currentLevel) checked:="checked" }),
              label(`for`:=levelId, style:="padding-left: 5px")(text)
            )
          }, br,
          input(`type`:="button", value:="Play!", `class`:="btn btn-primary", onclick:={ () =>
            if (currentLevel > 0) {
              remote call start(currentLevel)
            } else {
              dom.window.alert("Please select level!")
            }
          })
        )
      )
    }
  }

  def pageGuess(game: Hangman) = placed[Client].local { implicit! =>
    div(
      h2("Please make a guess"),
      h3(style := "letter-spacing: 4px;")(game.guessWord.mkString),
      p(s"You have made ${game.misses} bad guesses out of a maximum of ${game.level}"),
      p("Guess:")(
        for (c <- 'A' until 'Z' if !game.guess.contains(c)) yield {
          a(c.toString)(style:="padding-left:10px;", href:="javascript:void(0);", onclick:={ () =>
            remote call guess(c)
          })
        }
      ), br,
      a("Give up?", href := "javascript:void(0);", onclick := { () =>
        remote call giveup()
      })
    )
  }

  def pageResult(game: Hangman) = placed[Client].local { implicit! =>
    div {
      val result = if (game.won) "You Win!" else "You Lose!"
      div(
        h2(result),
        p(s"The word was: ${game.word}"), br,
        input(`type`:="button", value:="Start Again!", `class`:="btn btn-primary", onclick:={ () =>
          remote call giveup()
        })
      )
    }
  }
}

object HangmanGame {
  lazy val server = {
    val webSocket = WebSocketHandler()
    val hangman = new HangmanGame
    multitier setup new hangman.Server { def connect = listen[hangman.Client] { webSocket } }
    webSocket
  }

  def client(url: String) = {
    val hangman = new HangmanGame
    multitier setup new hangman.Client { def connect = request[hangman.Server] { WS(url) } }
  }
}