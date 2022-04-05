package compiler.parser.utils

import parsley.Parsley
import parsley.Parsley.{pos, pure}

trait ParserBuilder[T] {
  val parser: Parsley[T]
  final def <#(p: Parsley[_]): Parsley[T] = parser <* p
}

object ParserBuilder {
  trait ParserBuilderPos1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1)(pos: (Int, Int)): R

    val parser: Parsley[T1 => R] = pos.map(p => apply(_)(p))
  }

  trait ParserBuilder1[T1, R] extends ParserBuilder[T1 => R] {
    def apply(x: T1): R

    val parser: Parsley[T1 => R] = pure(apply)
  }

  trait ParserBuilderPos2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2)(pos: (Int, Int)): R

    val parser: Parsley[(T1, T2) => R] = pos.map(p => apply(_, _)(p))
  }

  trait ParserBuilder2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
    def apply(x: T1, y: T2): R

    val parser: Parsley[(T1, T2) => R] = pure(apply)
  }
}
