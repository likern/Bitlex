package com.bitflex.bencoding

import org.scalatest.FunSpec

import scala.util.Random

/**
  * Created by victor on 11.10.16.
  */
class BencodeTest extends FunSpec {
  describe("Parser result of bencode integer") {
    describe("without \'i\' prefix") {
      it("should be None") {
        val res = Bencode.parse("3e")
        assert(res.isEmpty)
      }
    }

    describe("without \'e\' postfix") {
      it("should be None") {
        val res = Bencode.parse("i3")
        assert(res.isEmpty)
      }
    }

    describe("containing \"minus zero\" -0") {
      it("should be None") {
        val res = Bencode.parse("i-0e")
        assert(res.isEmpty)
      }
    }

    describe("with leading zero") {
      it("should be None") {
        val res1 = Bencode.parse("i03e")

        // Check positive random integer with leading zero
        val strInt1 = "i0" + (Random.nextDouble() * Int.MaxValue).toInt + "e"
        val res2 = Bencode.parse(strInt1)

        // Check negative random integer with leading zero
        val strInt2 = "i-0" + (Random.nextDouble() * Int.MaxValue).toInt + "e"
        val res3 = Bencode.parse(strInt2)

        assert(res1.isEmpty)
        assert(res2.isEmpty)
        assert(res3.isEmpty)
      }

    }

    describe("containing \"zero\" 0") {
      it("should be Some(BnInt(0)") {
        val res = Bencode.parse("i0e")
        assert(res.nonEmpty)

        val value = res match {
          case Some(BnInt(x)) => x
          case _ => fail
        }
        assertResult(0)(value)
      }
    }

    describe("which not in base 10") {
      it("should be None") {
        val res1 = Bencode.parse("iFFe")
        val res2 = Bencode.parse("i2Ae")

        assert(res1.isEmpty)
        assert(res2.isEmpty)
      }

    }
  }

  describe("Parser result of bencode string") {
    describe("without length-prefix") {
      it("should be None") {
        val res = Bencode.parse("spam")
        assert(res.isEmpty)
      }
    }

    describe("with incorrect length-prefix") {
      it("should be None") {
        val res = Bencode.parse("2:spam")
        assert(res.isEmpty)
      }
    }

    describe("with zero length-prefix") {
      it("should be None") {
        val res = Bencode.parse("0:")
        assert(res.isEmpty)
      }
    }

    describe("without colon delimeter") {
      it("should be None") {
        val res = Bencode.parse("4spam")
        assert(res.isEmpty)
      }
    }

    describe("containing colon in valid string") {
      it("should be Some(x") {
        val res = Bencode.parse("9:delim:ter")
        val value = res match {
          case Some(BnString(x)) => x
          case _ => fail
        }
        assertResult("delim:ter")(value)
      }
    }

    describe("with valid string") {
      it("should be Some(x)") {
        val res = Bencode.parse("4:test")
        val value = res match {
          case Some(BnString(x)) => x
          case _ => fail
        }
        assertResult("test")(value)
      }
    }
  }

  describe("Parser result of bencode list") {
    describe("without \'l\' prefix") {
      it("should be None") {
        val res = Bencode.parse("4:spam4:eggse")
        assert(res.isEmpty)
      }
    }

    describe("without \'e\' prefix") {
      it("should be None") {
        val res = Bencode.parse("l4:spam4:eggs")
        assert(res.isEmpty)
      }
    }

    describe("containing zero elements") {
      it("should be Nil (empty list)") {
        val res = Bencode.parse("le")
        val value = res match {
          case Some(BnList(list)) => list
          case _ => fail
        }
        assertResult(Nil)(value)
      }
    }

    describe("containing one zero integer") {
      it("should contain exactly this integer") {
        Bencode.parse("li0ee") match {
          case Some(BnList(List(BnInt(value)))) if value == 0 => Unit
          case _ => fail
        }
      }
    }

    describe("containing one empty list as element") {
      it("should contain only this list") {
        Bencode.parse("llee") match {
          case Some(BnList(List(BnList(Nil)))) => Unit
          case _ => fail
        }
      }
    }

    describe("containing two string elements") {
      it("should contain exactly these strings") {
        val res = Bencode.parse("l4:spam4:eggse")
        res match {
          case Some(BnList(List(BnString("spam"), BnString("eggs")))) => Unit
          case _ => fail
        }
      }
    }
  }
}