package example

import loci._
import loci.transmitter.rescala._
import loci.serializer.upickle._
import loci.communicator.ws.akka._
import rescala.default._
import org.scalajs.dom
import scalatags.JsDom.all._
import scalatags.JsDom.tags2.section
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import common.Framework._
import models.Task
import models.TaskStore

@multitier object Todo {
  @peer type Server <: { type Tie <: Multiple[Client] }
  @peer type Client <: { type Tie <: Single[Server] }

  val store: Local[TaskStore] on Server

  val tasks: Var[Seq[Task]] on Server = Var(Seq.empty[Task])

  val filters = on[Client] local {
    Map[String, Task => Boolean](
      ("All", t => true),
      ("Active", !_.done),
      ("Completed", _.done))
  }

  val filter: Local[Var[String]] on Client = Var("All")

  def create(txt: String, done: Boolean = false) = on[Server] {
    store.create(Task(None, txt, done)) foreach { _ => updateTaskList() }
  }

  def update(task: Task) = on[Server] {
    task.id foreach { _ => store.update(task) foreach { _ => updateTaskList() } }
  }

  def delete(id: Option[Long]) = on[Server] {
    id foreach { store.delete(_) foreach { _ => updateTaskList() } }
  }

  def clearCompletedTasks() = on[Server] {
    store.clearCompletedTasks foreach { _ => updateTaskList() }
  }

  def updateTaskList() = on[Server].local {
    store.all foreach tasks.set
  }

  on[Server] { updateTaskList() }

  def templateHeader = on[Client] local {
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

  def templateBody = on[Client] local {
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

  def templateFooter = on[Client] local {
    footer(id:="info")(
      p("Double-click to edit a todo"),
      p("Original version created by ", a(href:="https://github.com/lihaoyi/workbench-example-app/blob/todomvc/src/main/scala/example/ScalaJSExample.scala")("Li Haoyi")),
      p("Modified version with database backend can be found ", a(href:="https://github.com/hussachai/play-scalajs-showcase")("here"))
    )
  }

  def partList = on[Client] local {
    val editing = Var(Option.empty[Task])

    Signal.dynamic {
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
                editing.set(Some(task))
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
                editing.set(None)
                false
              },
              inputRef
            )
          )
        }
      )
    }
  }

  def partControls = on[Client] local {
    val done = Signal { tasks.asLocal().count(_.done) }
    val notDone = Signal { tasks.asLocal().length - done() }

    footer(id:="footer")(
      span(id:="todo-count")(strong(notDone), " item left"),
      ul(id:="filters")(
        for ((name, pred) <- filters.toSeq) yield {
          li(a(
            `class`:= Signal.dynamic {
              if (name == filter()) "selected"
              else ""
            },
            name,
            href:="#",
            onclick := { () => filter.set(name) }
          ))
        }
      ),
      button(
        id:="clear-completed",
        onclick := { () => remote call clearCompletedTasks() },
        "Clear completed (", done, ")"
      )
    )
  }

  on[Client] {
    dom.document.getElementById("content").appendChild(
      section(id:="todoapp")(
        templateHeader,
        templateBody,
        templateFooter
      ).render
    )
  }
}

object TodoInitialization {
  private val servers = mutable.Map.empty[TaskStore, WebSocketHandler]

  def server(taskStore: TaskStore) = servers getOrElseUpdate (taskStore, {
    val webSocket = WebSocketHandler()
    multitier start new Instance[Todo.Server](listen[Todo.Client] { webSocket }) {
      val store = taskStore
    }
    webSocket
  })

  def client(url: String) = {
    multitier start new Instance[Todo.Client](connect[Todo.Server] { WS(url) })
  }
}
