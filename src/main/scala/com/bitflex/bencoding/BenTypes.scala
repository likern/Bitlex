package com.bitflex.bencoding

sealed trait BnValue

/**
  * Represent a Bencode string value.
  */
case class BnString(value: String) extends BnValue

/**
  * Represent a Bencode num value.
  */
case class BnInt(value: BigInt) extends BnValue

/**
  * Represent a Bencode list value.
  */
case class BnList(elems: List[BnValue]) extends BnValue

/**
  * Represent a Bencode dictionary value.
  */
case class BnDict(bindins: Map[BnString, BnValue]) extends BnValue
