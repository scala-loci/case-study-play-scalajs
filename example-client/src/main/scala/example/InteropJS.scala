package example

import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.ScalaJSDefined
import org.scalajs.dom
import scalatags.JsDom._
import all._
import shared.foo._

@JSExport
object ScalaJSInterop {

  def template = div{
    ul(
      li(a("hello"))
    )
  }

  def runScript = {
//    val foo = new Foo
//    foo.test //Doesn't work because there's no method named test in javascript Foo class
//
//    new Alert("Hello this is native javascript alert")

    val bob = new Person("Bob")
//    dom.window.alert(bob.firstName)
//    bob.walk()
//    bob.sayHello()
    val student = new Student("Alice", "Biology")
//    student.sayHello()
//    student.sayGoodBye()
//    student.notExistInJs() //Throws error "undefined is not a function"
  }


  @JSExport
  def main(foo: js.Dynamic) = {
//    foo.listInt.asInstanceOf[js.Array[Int]](2).toString
    dom.window.alert(JSON.stringify(foo))
    dom.document.body.appendChild(template.render)
    runScript
  }

}


/**
 * This represents Foo class in javascript
 * You must not declare it as nested class
 */
@ScalaJSDefined
class Foo extends js.Object {
  def test: Unit = {
    dom.window.alert("Test add-hoc method") /* deprecated */
  }
}

@js.native
class Alert(msg: String) extends js.Object {}

@js.native
class Person(val firstName: String) extends js.Object{
  /**
   * The method must have () and Unit as return type in case it returns nothing
   * If you leave out the return type, scala will put Nothing as the return type
   * and Scala.js doesn't work with Nothing type yet.
   */
  def walk(): Unit = js.native

  def sayHello(): Unit = js.native
}

@js.native
class Student(firstName: String, val subject: String) extends Person(firstName) {
  def sayGoodBye(): Unit = js.native

  def notExistInJs(): Unit = js.native
}

import org.scalajs.dom._
import org.scalajs.dom.raw._

/**
 * A MessageEvent is sent to clients using WebSockets when data is received from the
 * server. This is delivered to the listener indicated by the WebSocket object's
 * onmessage attribute.
 *
 * MDN
 */
@js.native
class MessageEvent(typeArg: String, init: js.UndefOr[MessageEventInit]) extends Event(typeArg, init) {
  def source: Window = js.native

  def origin: String = js.native

  /**
   * The data from the server (`String`, [[Blob]], or `ArrayBuffer`)
   *
   * MDN
   */
  def data: Any = js.native

  def ports: js.Any = js.native
}

trait MessageEventInit extends EventInit {
  var canBubbleArg: js.UndefOr[Boolean] = js.undefined
  var cancelableArg: js.UndefOr[Boolean] = js.undefined
  var dataArg: js.UndefOr[js.Any] = js.undefined
  var originArg: js.UndefOr[String] = js.undefined
  var lastEventIdArg: js.UndefOr[String] = js.undefined
  var sourceArg: js.UndefOr[Window] = js.undefined
}
