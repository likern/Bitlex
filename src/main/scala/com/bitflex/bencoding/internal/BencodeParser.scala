package com.bitflex.bencoding.internal

import com.bitflex.bencoding._

import scala.util.parsing.combinator.RegexParsers

object BencodeParser extends RegexParsers {

  def value: Parser[BnValue] = dictLiteral | listLiteral | stringLiteral | intLiteral
  def intLiteral: Parser[BnInt] = "i"~> intValue <~"e"
  def intValue: Parser[BnInt] = """(-?[1-9]\d*)|(0)""".r ^^ (x => BnInt(BigInt(x)))

  def stringLiteral: Parser[BnString] = lengthWithPrefix >> { length => """.{%d}""".format(length.toInt).r} ^^ BnString
  def lengthWithPrefix: Parser[BigInt] = lengthPrefix <~ ":"
  def lengthPrefix: Parser[BigInt] = """[1-9]\d*""".r ^^ (BigInt(_))

  def listLiteral: Parser[BnList] = "l"~> listValue <~"e"
  def listValue = rep(value) ^^ BnList

  def dictLiteral: Parser[BnDict] = "d"~> dictValue <~"e"
  def dictValue = rep(dictMember) ^^ (Map() ++ _) ^^ BnDict
  def dictMember: Parser[(BnString, BnValue)] = stringLiteral~value ^^ { case stringLiteral~value => (stringLiteral, value) }
}