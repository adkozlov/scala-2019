import hw03._
import org.scalatest.Matchers

class ZipTest extends org.scalatest.FlatSpec with Matchers {

    it should "zip empty lists correctly" in {
        (HNil zip HNil) should be(HNil)
    }

    it should "zip not empty lists correctly" in {
        val left = "hello" :: 42 :: false :: HNil
        val right = true :: "world" :: 53 :: HNil
        val found = left zip right
        val expected = ("hello", true) :: (42, "world") :: (false, 53) :: HNil
        found should be(expected)
    }

    it should "not compile if lists sizes are not equal" in {
        assertDoesNotCompile(
            """
              |val left = "hello" :: 42 :: false :: HNil
              |val right = true :: "world" :: 53 :: "one more" :: HNil
              |val result = left zip right
            """.stripMargin
        )
    }
}
