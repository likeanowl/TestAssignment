package iter

import iter.FlatIterator.flatten
import org.scalatest.flatspec.AnyFlatSpec

class FlatIteratorSpec extends AnyFlatSpec {
  it should "work correctly in basic cases" in {
    val expected = Seq(
      Seq(
        "1",
        "1.1",
        "1.2",
        "2",
        "2.1.1",
        "2.2"
      ),
      Seq(
        "hey",
        "ho",
        "hoho",
        "heyho"
      )
    )

    val result = Seq(
      flatten(Iterator(
        "1",
        Iterator(
          "1.1",
          "1.2"
        ),
        "2",
        Iterator(
          Iterator(
            "2.1.1"
          ),
          "2.2"
        )
      )),
      flatten(Iterator(
        "hey",
        Iterator("ho", Iterator("hoho")),
        "heyho"
      ))
    ).map(_.toSeq)

    expected.zip(result).foreach(pair => assertResult(pair._1)(pair._2))
  }

  it should "work correctly with empty iterators" in {
    val expected = Seq.empty[String]
    val result = Seq(
      flatten(Iterator.empty[Any]),
      flatten(Iterator(Iterator.empty[Any])),
      flatten(Iterator(Iterator(Iterator.empty[Any])))
    ).map(_.toSeq)
    result.foreach(x => assertResult(expected)(x))
  }

  it should "work correctly with nulls" in {
    val expected = Seq(Seq(null), Seq(null), Seq(null, null, "hey"))
    val result = Seq(flatten(null), flatten(Iterator(null)), flatten(Iterator(null, Iterator(null), "hey"))).map(_.toSeq)
    expected.zip(result).foreach(pair => assertResult(pair._1)(pair._2))
  }
}
