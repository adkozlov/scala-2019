package hw03

trait NonNegative

final case class Positive[Rest <: NonNegative](rest: Rest) extends NonNegative

case object Zero extends NonNegative
