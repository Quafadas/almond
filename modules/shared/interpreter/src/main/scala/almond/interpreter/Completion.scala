package almond.interpreter

import argonaut.JsonObject

/**
  * Completion result
  *
  * @param from: position from which one of the completion can be substituted
  * @param until: position up to which one the completion can be substituted (not including the character at position `until`)
  * @param completions: possible replacements between indices `from` until `to`
  */
final case class Completion(
  from: Int,
  until: Int,
  completions: Seq[String],
  metadata: JsonObject
)

object Completion {
  def apply(from: Int, until: Int, completions: Seq[String]): Completion =
    Completion(from, until, completions, JsonObject.empty)
  def empty(pos: Int): Completion =
    Completion(pos, pos, Nil, JsonObject.empty)
}
