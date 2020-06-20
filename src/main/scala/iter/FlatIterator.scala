package iter

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object FlatIterator {
  def flatten(i: Iterator[Any]): Iterator[String] = {
    @tailrec
    def flattenRec(i: Any, acc: Iterator[String] = Iterator.empty[String], iters: Queue[Iterator[Any]] = Queue.empty): Iterator[String] = {
      if (iters.nonEmpty) {
        i match {
          case x if x == null => flattenRec(iters.dequeue._1, acc ++ Iterator(null), iters.dequeue._2)
          case it: Iterator[Any] if it.hasNext => flattenRec(it.next, acc, iters.prepended(it))
          case _: Iterator[Any] => flattenRec(iters.dequeue._1, acc, iters.dequeue._2)
          case x => flattenRec(iters.dequeue._1, acc ++ Iterator(x.toString), iters.dequeue._2)
        }
      } else {
        i match {
          case x if x == null => acc ++ Iterator(null)
          case it: Iterator[Any] if it.hasNext => flattenRec(it.next, acc, iters.prepended(it))
          case _: Iterator[Any] => acc
          case x => acc ++ Iterator(x.toString)
        }
      }
    }

    flattenRec(Option(i).fold(Iterator.single[Any](null))(identity))
  }
}
