package barneshut

import java.util.concurrent._
import scala.collection._
import scala.math._
import scala.collection.parallel._
import barneshut.conctrees.ConcBuffer
import org.junit._
import org.junit.Assert.{assertEquals, fail}

class BarnesHutSuite {
  // test cases for quad tree

import FloatOps._
  @Test def `Empty: center of mass should be the center of the cell`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  @Test def `Empty: mass should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  @Test def `Empty: total should be 0`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  @Test def `Leaf with 1 body`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  @Test def `Fork with 3 empty quadrants and 1 leaf (nw)`: Unit = {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }

  @Test def `Fork with 4 empty quadrants`: Unit = {
    val nw = Empty(17.5f, 27.5f, 5f)
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
    assert(quad.massX == 20f, s"${quad.massX} should be 20f")
    assert(quad.massY == 30f, s"${quad.massY} should be 30f")
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  @Test def `Empty.insert(b) should return a Leaf with only that body (2pts)`: Unit = {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

  @Test def `'insert' should work correctly on a leaf with center (1,1) and size 2`: Unit = {
    val b1 = new Body(10f, 0.5f, 0.5f, 0f, 0f)
    val b2 = new Body(10f, 1.5f, 0.5f, 0f, 0f)
    val nw = Leaf(0.5f, 0.5f, 1f, Seq(b1))
    val ne = Empty(1.5f, 0.5f, 1f)
    val sw = Empty(0.5f, 1.5f, 1f)
    val se = Empty(1.5f, 1.5f, 1f)
    val quad = Fork(nw, ne, sw, se)
    val inserted = quad.insert(b2)
    val insertedProper = Fork(nw, Leaf(1.5f, 0.5f, 1f, Seq(b2)), sw, se)

    assertEquals(inserted, insertedProper)
  }

  // test cases for Body

  @Test def `Body.updated should do nothing for Empty quad trees`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assertEquals(0f, body.xspeed, precisionThreshold)
    assertEquals(0f, body.yspeed, precisionThreshold)
  }

  @Test def `Body.updated should take bodies in a Leaf into account (2pts)`: Unit = {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  @Test def `'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96 (2pts)`: Unit = {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }

  @Test def `'SectorMatrix.combine' should correctly combine two sector matrices of size 96 (2pts)`: Unit = {
    val b1 = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries1 = new Boundaries()
    boundaries1.minX = 1
    boundaries1.minY = 1
    boundaries1.maxX = 97
    boundaries1.maxY = 97
    val sm1 = new SectorMatrix(boundaries1, SECTOR_PRECISION)
    sm1 += b1
    val res1 = sm1(2, 3).size == 1 && sm1(2, 3).exists(_ == b1)
    assert(res1, s"Body 1 not found in the right sector")

    val b2 = new Body(15, 25, 47, 0.1f, 0.1f)
    val boundaries2 = new Boundaries()
    boundaries2.minX = 1
    boundaries2.minY = 1
    boundaries2.maxX = 97
    boundaries2.maxY = 97
    val sm2 = new SectorMatrix(boundaries1, SECTOR_PRECISION)
    sm2 += b2
    val res2 = sm2(2, 3).size == 1 && sm2(2, 3).exists(_ == b2)
    assert(res2, s"Body 2 not found in the right sector")

    val sm3 = sm1.combine(sm2)
    val res3 = sm3(2, 3).size == 2 && sm3(2, 3).exists(_ == b1) && sm3(2, 3).exists(_ == b2)
    assert(res3, s"Bodies 1, 2 not found in the right sector")
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}

object FloatOps {
  val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

