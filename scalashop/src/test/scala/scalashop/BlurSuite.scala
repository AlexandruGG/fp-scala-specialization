package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {
  @Test def `boxBlurKernel should handle a radius of 0`(): Unit = {
    val size = 3
    val src = new Img(size, size)

    for (x <- 0 until size; y <- 0 until size) {
      val colour = rgba(x, y, x * y, x ^ y)
      src(x, y) = colour

      assertEquals(colour, boxBlurKernel(src, x, y, 0))
    }
  }

  @Test def `boxBlurKernel should handle a radius of 1`(): Unit = {
    val src = new Img(3, 3)
    src(0, 0) = 0; src(0, 1) = 1; src(0, 2) = 2
    src(1, 0) = 3; src(1, 1) = 4; src(1, 2) = 5
    src(2, 0) = 6; src(2, 1) = 7; src(2, 2) = 8

    assertEquals(4, boxBlurKernel(src , 1, 1, 1))
  }

  @Test def `VerticalBoxBlur blur updates dest image`(): Unit = {
    val src = new Img(2, 2)
    src(0, 0) = 0; src(0, 1) = 1
    src(1, 0) = 2; src(1, 1) = 3

    val dest = new Img (2, 2)
    dest(0, 0) = 0; src(0, 1) = 0
    dest(1, 0) = 0; src(1, 1) = 0

    VerticalBoxBlur.blur(src, dest, 0, 1, 1)

    for (x <- 0 until 1; y <- 0 until 1) {
      assertEquals(src(x, y), dest(x, y))
    }
  }

  @Test def `HorizontalBoxBlur blur updates dest image`(): Unit = {
    val src = new Img(2, 2)
    src(0, 0) = 0; src(0, 1) = 1
    src(1, 0) = 2; src(1, 1) = 3

    val dest = new Img (2, 2)
    dest(0, 0) = 0; src(0, 1) = 0
    dest(1, 0) = 0; src(1, 1) = 0

    HorizontalBoxBlur.blur(src, dest, 0, 1, 1)

    for (x <- 0 until 1; y <- 0 until 1) {
      assertEquals(src(x, y), dest(x, y))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
