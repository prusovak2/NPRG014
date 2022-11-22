// Uncomment this to finalize the code

package h3

import scala.collection.mutable.ListBuffer
import scala.compiletime.ops.boolean

abstract class Event

case class Command(cmdName: String) extends Event

case class Succeed(cmdName: String) extends Event // command with name cmdName succeeded, two commands with the same name won't exists at the same time

case class Fail(cmdName: String) extends Event

class Property(val name: String, val func: () => Boolean)

// base class for classes checking the rules
class Monitor[T]:
  val properties = ListBuffer.empty[Property]
  var is_satisfied = true

  def property(propName: String)(formula: => Boolean) = /* Add declaration here */
    properties += Property(propName, () => formula)

  var eventsToBeProcessed = List[T]()

  def check(events: List[T]) =
    for prop <- properties do
      eventsToBeProcessed = events

      val result = prop.func()

      println("Property \"" + prop.name + "\" ... " + (if (result) "OK" else "FAILED"))

  //def simplifyUsing(func: PartialFunction[Expr, Expr]) =
	//	if func.isDefinedAt(this) then func(this) else this

  def require(func: PartialFunction[T, Boolean]): Boolean =
    val definedEvents = eventsToBeProcessed.dropWhile(event => !func.isDefinedAt(event))
    if(!definedEvents.isEmpty) 
      eventsToBeProcessed = definedEvents.tail 
      func(definedEvents.head)
    else true

  /* Add body here
   *
   * to know whether a partial function is defined for a given event,
   * use func.isDefinedAt(event).
   */


class MyMonitor extends Monitor[Event] :

  // writing dsl here
  property("The first command should succeed or fail before it is received again") {
    require {
      case Command(c) => // if I see a  command c
        require { // at some time in the future I wanna see the command c (`c` so that same c is used -> new var c is not created when pattern matching) to succeed or yo fail
          case Succeed(`c`) => true
          case Fail(`c`) => true
          case Command(`c`) => false // I do not wanna see the same command again - command should complete before it is issued again
        }
    }
  }

  property("The first command should not get two results") {
    require {
      case Succeed(c) =>
        require {
          case Succeed(`c`) => false
          case Fail(`c`) => false
          case Command(`c`) => true
        }
      case Fail(c) =>
        require {
          case Succeed(`c`) => false
          case Fail(`c`) => false
          case Command(`c`) => true
        }
    }
  }

  property("The first command should succeed") {
    /* Add a property definition here which requires that the first command does not fail.
     * It should yield OK with the events listed in the main method.
     */
    require {
      case Command(c) => // if I see a  command c
        require { 
          case Succeed(`c`) => true
          case Fail(`c`) => false
        }
    }
  }


object Checker:

  def main(args: Array[String]): Unit =
    val events = List(
      Command("take_picture"),
      Command("get_position"),
      Succeed("take_picture"),
      Fail("take_picture")
    )

    val monitor = new MyMonitor
    monitor.check(events)

    /*val events2 = List(
      Command("take_picture"),
      Command("get_position"),
      Command("take_picture"),
      Succeed("get_position"),
      Fail("take_picture")
    )
    monitor.check(events2)

    val events3 = List(
      Command("take_picture"),
      Command("get_position"),
      Fail("get_position")
    )

    monitor.check(events3)

    val events4 = List(
      Command("take_picture"),
      Command("get_position"),
      Fail("take_picture")
    )*/