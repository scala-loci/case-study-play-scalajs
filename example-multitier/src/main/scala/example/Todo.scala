package example

import loci._
import loci.rescalaTransmitter._
import loci.serializable.upickle._
import loci.ws.akka._
import rescala._
import org.scalajs.dom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import common.Framework._
import models.Task
import models.TaskStore

@multitier
class Todo(store: => TaskStore) {
  trait Server extends Peer { type Tie <: Multiple[Client] }
  trait Client extends Peer { type Tie <: Single[Server] }

  val tasks: Var[Seq[Task]] on Server = Var(Seq.empty[Task])

  val filters = placed[Client].local { implicit! =>
    Map[String, Task => Boolean](
      ("All", t => true),
      ("Active", !_.done),
      ("Completed", _.done))
  }

  val filter: Var[String] localOn Client = Var("All")

  def create(txt: String, done: Boolean = false) = placed[Server] { implicit! =>
    store.create(Task(None, txt, done)) foreach { _ => updateTaskList }
  }

  def update(task: Task) = placed[Server] { implicit! =>
    task.id foreach { _ => store.update(task) foreach { _ => updateTaskList } }
  }

  def delete(id: Option[Long]) = placed[Server] { implicit! =>
    id foreach { store.delete(_) foreach { _ => updateTaskList } }
  }

  def clearCompletedTasks() = placed[Server] { implicit! =>
    store.clearCompletedTasks foreach { _ => updateTaskList }
  }

  def updateTaskList() = placed[Server].local { implicit! =>
    store.all foreach tasks.set
  }

  placed[Server] { implicit! => updateTaskList }

  def templateHeader = placed[Client].local { implicit! =>
    val inputBox = input(
      id:="new-todo",
      placeholder:="What needs to be done?",
      autofocus:=true
    ).render

    header(id:="header")(
      form(
        inputBox,
        onsubmit := { () =>
          remote call create(inputBox.value)
          inputBox.value = ""
          false
        }
      )
    )
  }

  def templateBody = placed[Client].local { implicit! =>
    section(id:="main")(
      input(
        id:="toggle-all",
        `type`:="checkbox",
        cursor:="pointer",
        onclick := { () =>
//          val target = tasks().exists(_.done == false)
//          Var.set(tasks().map(_.done -> target): _*)
        }
      ),
      label(`for`:="toggle-all", "Mark all as complete"),
      partList,
      partControls
    )
  }

  def templateFooter = placed[Client].local { implicit! =>
    footer(id:="info")(
      p("Double-click to edit a todo"),
      p("Original version created by ", a(href:="https://github.com/lihaoyi/workbench-example-app/blob/todomvc/src/main/scala/example/ScalaJSExample.scala")("Li Haoyi")),
      p("Modified version with database backend can be found ", a(href:="https://github.com/hussachai/play-scalajs-showcase")("here"))
    )
  }

  def partList = placed[Client].local { implicit! =>
    val editing = Var(Option.empty[Task])

    Signal {
      ul(id := "todo-list")(
        for (task <- tasks.asLocal() if filters(filter())(task)) yield {
          val inputRef = input(`class` := "edit", value := task.txt).render

          li(
            `class` := Signal {
              if (task.done) "completed"
              else if (editing() == Some(task)) "editing"
              else ""
            },
            div(`class` := "view")(
              ondblclick := { () =>
                editing set Some(task)
              },
              input(`class`:= "toggle", `type`:= "checkbox", cursor:= "pointer", onchange:= { () =>
                  remote call update(task.copy(done = !task.done))
                }, if (task.done) checked := true else ""
              ),
              label(task.txt),
              button(
                `class` := "destroy",
                cursor := "pointer",
                onclick := { () => remote call delete(task.id) }
              )
            ),
            form(
              onsubmit := { () =>
                remote call update(task.copy(txt = inputRef.value))
                editing set None
                false
              },
              inputRef
            )
          )
        }
      )
    }
  }

  def partControls = placed[Client].local { implicit! =>
    val done = Signal { tasks.asLocal().count(_.done) }
    val notDone = Signal { tasks.asLocal().length - done() }

    footer(id:="footer")(
      span(id:="todo-count")(strong(notDone), " item left"),
      ul(id:="filters")(
        for ((name, pred) <- filters.toSeq) yield {
          li(a(
            `class`:= Signal {
              if (name == filter()) "selected"
              else ""
            },
            name,
            href:="#",
            onclick := { () => filter set name }
          ))
        }
      ),
      button(
        id:="clear-completed",
        onclick := { () => remote call clearCompletedTasks },
        "Clear completed (", done, ")"
      )
    )
  }

  placed[Client] { implicit! =>
    dom.document.getElementById("content").appendChild(
      section(id:="todoapp")(
        templateHeader,
        templateBody,
        templateFooter
      ).render
    )
  }
}

object Todo {
  private val servers = mutable.Map.empty[TaskStore, WebSocketHandler]

  def server(store: TaskStore) = servers getOrElseUpdate (store, {
    val webSocket = WebSocketHandler()
    val todo = new Todo(store)
    multitier setup new todo.Server { def connect = listen[todo.Client] { webSocket } }
    webSocket
  })

  def client(url: String) = {
    val todo = new Todo(???)
    multitier setup new todo.Client { def connect = request[todo.Server] { WS(url) } }
  }
}
