package ru.spbau.jvm.scala.heterogeneous

trait Zippable[
First <: HList,
Second <: HList,
Result <: HList
] {
  def apply(fst: First, snd: Second): Result
}

object Zippable {
  implicit def nilNilZippable: Zippable[Nil, Nil, Nil] =
    (fst: Nil, snd: Nil) => fst

  implicit def fNilZippable[
    Second <: HList
  ]: Zippable[Nil, Second, Nil] =
    (fst: Nil, snd: Second) => fst

  implicit def sNilZippable[
  First <: HList
  ]: Zippable[First, Nil, Nil] =
    (fst: First, snd: Nil) => snd

  implicit def zippable[
  FHead,
  FTail <: HList,
  SHead,
  STail <: HList,
  ResultTail <: HList
  ](implicit zippable: Zippable[FTail, STail, ResultTail]): Zippable[FHead :: FTail, SHead :: STail, (FHead, SHead) :: ResultTail] =
    (fst: FHead :: FTail, snd: SHead :: STail) => {
      val HCons(fHead, fTail) = fst
      val HCons(sHead, sTail) = snd
      HCons((fHead, sHead), zippable(fTail, sTail))
    }
}
