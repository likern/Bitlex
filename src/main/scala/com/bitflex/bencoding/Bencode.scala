package com.bitflex.bencoding

import internal.BencodeParser

/**
  * Helper functions to handle BnValues.
  */
object Bencode {

  /**
    * Parse a String representing a bencode, and return it as a BnValue.
    *
    * @param input a String to parse
    * @return the BnValue representing the string
    */
  def parse(input: String): Option[BnValue] = {
    BencodeParser.parseAll(BencodeParser.value, input) match {
      case BencodeParser.Success(value, tail) if tail.atEnd => Some(value)
      case _ => None
    }
  }
}
