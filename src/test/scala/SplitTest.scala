import hw03._

import org.scalatest.Matchers

class SplitTest extends org.scalatest.FlatSpec with Matchers {

    it should "correctly splits empty list" in {
        HNil.split(Zero) should be((HNil, HNil))
    }

    it should "correctly splits not empty list" in {
        {
            val (lfound, rfound) = ("123" :: true :: 42 :: HNil).split(Zero)
            val lexpected = HNil
            val rexpected = "123" :: true :: 42 :: HNil
            lfound should be(lexpected)
            rfound should be(rexpected)
        }
        {
            val (lfound, rfound) = ("123" :: true :: 42 :: HNil).split(Positive(Zero))
            val lexpected = "123" :: HNil
            val rexpected = true :: 42 :: HNil
            lfound should be(lexpected)
            rfound should be(rexpected)
        }
        {
            val (lfound, rfound) = ("123" :: true :: 42 :: HNil).split(Positive(Positive(Zero)))
            val lexpected = "123" :: true :: HNil
            val rexpected = 42 :: HNil
            lfound should be(lexpected)
            rfound should be(rexpected)
        }
        {
            val (lfound, rfound) = ("123" :: true :: 42 :: HNil).split(Positive(Positive(Positive(Zero))))
            val lexpected = "123" :: true :: 42 :: HNil
            val rexpected = HNil
            lfound should be(lexpected)
            rfound should be(rexpected)
        }
    }

    it should "not compile if split index is too large" in {
        assertDoesNotCompile(
            """
              |val (lfound, rfound) = ("123" :: true :: 42 :: HNil).split(Positive(Positive(Positive(Positive(Zero)))))
              |        val lexpected = "123" :: true :: 42 :: HNil
              |        val rexpected = HNil
              |        lfound should be(lexpected)
              |        rfound should be(rexpected)
            """.stripMargin
        )
    }
}
