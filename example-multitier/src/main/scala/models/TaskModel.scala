package models

import scala.concurrent.Future

case class Task(id: Option[Long] = None, txt: String, done: Boolean)

object Task { implicit val pickler = upickle.default.macroRW[Task] }

trait TaskStore {
  def all: Future[Seq[Task]]

  def create(taskWithoutId: Task): Future[Task]

  def update(task: Task): Future[Boolean]

  def delete(ids: Long*): Future[Boolean]

  def clearCompletedTasks: Future[Int]
}
