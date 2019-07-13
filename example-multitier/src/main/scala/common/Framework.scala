package common

import rx._
import rescala.default._
import scala.util.Success
import scala.util.Failure
import scalatags.JsDom.all._
import org.scalajs.dom.Element

/**
 * A minimal binding between ReScala and Scalatags and Scala-Js-Dom
 */
object Framework {

  /**
   * Wraps reactive strings in spans, so they can be referenced/replaced
   * when the Signal changes.
   */
  implicit def rescalaStr[T](signal: Signal[T])(implicit f: T => Frag): Frag = {
    rescalaMod(Signal { span(signal()) })
  }

  /**
   * Sticks some Signal into a Scalatags fragment, which means hooking up an
   * changed event handler to propagate changes into the DOM via the element's
   * ID. Monkey-patches the handler onto the element itself so we have a
   * reference to kill it when the element leaves the DOM (e.g. gets deleted).
   */
  implicit def rescalaMod(signal: Signal[HtmlTag]): Frag = {
    val rendered = signal map { _.render } withDefault "".render
    var lastTag = rendered.now
    rendered.changed observe { newTag =>
      lastTag.parentNode.replaceChild(newTag, lastTag)
      lastTag = newTag
    }
    bindNode(lastTag)
  }
  implicit def rescalaAttrValue[T: AttrValue, Sig[T] <: Signal[T]] = new AttrValue[Sig[T]]{
    def apply(t: Element, a: Attr, signal: Sig[T]): Unit = {
      signal.changed += { value => implicitly[AttrValue[T]].apply(t, a, value) }
    }
  }
  implicit def rescalaStyleValue[T: StyleValue, Sig[T] <: Signal[T]] = new StyleValue[Sig[T]]{
    def apply(t: Element, s: Style, signal: Sig[T]): Unit = {
      signal.changed += { value => implicitly[StyleValue[T]].apply(t, s, value) }
    }
  }
}


//package common
//import scala.collection.{SortedMap, mutable}
//import scalatags.JsDom.all._
//import scala.util.{Failure, Success, Random}
//import rx._
//import rx.core.{Propagator, Obs}
//import org.scalajs.dom
//import org.scalajs.dom.{Element, DOMParser}
//import scala.scalajs.js
//
//
///**
// * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom
// */
//object Framework {
//
//  /**
//   * Wraps reactive strings in spans, so they can be referenced/replaced
//   * when the Rx changes.
//   */
//  implicit def RxStr[T](r: Rx[T])(implicit f: T => Frag): Frag = {
//    rxMod(Rx(span(r())))
//  }
//
//  /**
//   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
//   * to propagate changes into the DOM via the element's ID. Monkey-patches
//   * the Obs onto the element itself so we have a reference to kill it when
//   * the element leaves the DOM (e.g. it gets deleted).
//   */
//  implicit def rxMod[T <: dom.raw.HTMLElement](r: Rx[HtmlTag]): Frag = {
//    def rSafe = r.toTry match {
//      case Success(v) => v.render
//      case Failure(e) => span(e.toString, backgroundColor := "red").render
//    }
//    var last = rSafe
//    Obs(r, skipInitial = true){
//      val newLast = rSafe
//      last.parentElement.replaceChild(newLast, last)
//      last = newLast
//    }
//    bindNode(last)
//  }
//  implicit def RxAttrValue[T: AttrValue] = new AttrValue[Rx[T]]{
//    def apply(t: Element, a: Attr, r: Rx[T]): Unit = {
//      Obs(r){ implicitly[AttrValue[T]].apply(t, a, r())}
//    }
//  }
//  implicit def RxStyleValue[T: StyleValue] = new StyleValue[Rx[T]]{
//    def apply(t: Element, s: Style, r: Rx[T]): Unit = {
//      Obs(r){ implicitly[StyleValue[T]].apply(t, s, r())}
//    }
//  }
//}
