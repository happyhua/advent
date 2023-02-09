package advent
package util

import org.scalatest.funsuite.AnyFunSuite

class UtilTest extends AnyFunSuite:
  test("util repeated") {
    var input = Seq(7, 8, 9, 0, 7, 8, 9)
    assert(repeatedContains(input) == Seq(7, 8, 9))
    assert(repeatedExact(input) == Seq())

    input = Seq(7, 8, 9, 7, 8, 9)
    assert(repeatedContains(input) == Seq(7, 8, 9))
    assert(repeatedExact(input) == Seq(7, 8, 9))

    input = Seq(1, 2, 3, 7, 8, 9, 0, 7, 8, 9)
    assert(repeatedContains(input) == Seq())
    assert(repeatedExact(input) == Seq())
    assert(repeatedExactWithOffset(input) == Seq())

    input = Seq(1, 2, 3, 7, 8, 9, 7, 8, 9)
    assert(repeatedContains(input) == Seq())
    assert(repeatedExact(input) == Seq())
    assert(repeatedExactWithOffset(input) == Seq(7, 8, 9))

    input = Seq(1, 2, 3, 0, 7, 8, 9, 0, 7, 8, 9, 0, 7, 8)
    assert(repeatedExactWithOffset(input) == Seq(9, 0, 7, 8))
    assert(repeatedExactWithOffsetNormalized(input) == Seq(0, 7, 8, 9))

    input = Seq(1, 2, 3, 0, 7, 8, 9, 0, 7, 8, 9, 0, 7)
    assert(repeatedExactWithOffset(input) == Seq(8, 9, 0, 7))
    assert(repeatedExactWithOffsetNormalized(input) == Seq(0, 7, 8, 9))

    input = Seq(1, 2, 3, 0, 7, 8, 9, 0, 7, 8, 9, 0)
    assert(repeatedExactWithOffset(input) == Seq(7, 8, 9, 0))
    assert(repeatedExactWithOffsetNormalized(input) == Seq(0, 7, 8, 9))

    input = Seq(1, 2, 3, 0, 7, 8, 9, 0, 7, 8, 9)
    assert(repeatedExactWithOffset(input) == Seq(0, 7, 8, 9))
    assert(repeatedExactWithOffsetNormalized(input) == Seq(0, 7, 8, 9))

    input = Seq(1, 2, 3, 0, 7, 8, 9, 0, 7, 8)
    assert(repeatedExactWithOffset(input) == Seq())
    assert(repeatedExactWithOffsetNormalized(input) == Seq())
  }
