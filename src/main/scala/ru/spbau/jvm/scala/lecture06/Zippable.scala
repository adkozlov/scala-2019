package ru.spbau.jvm.scala.lecture06

trait Zippable[
  Prefix <: HList,
  List <: HList,
  Result <: HList
] {
  def apply(prefix: Prefix, list: List): Result
}

object Zippable {

  //Two lists with different size can not be zipped

  // Nil zip Nil = Nil
  implicit def nilZippable: Zippable[Nil, Nil, Nil] =
    (_: Nil, _: Nil) => HNil

  // Prefix zip List = Result
  // (PrefixHead :: Prefix) zip (ListHead :: List) = (PrefixHead, ListHead) :: Result
  implicit def zippable[
    PrefixHead,
    Prefix <: HList,
    ListHead,
    List <: HList,
    Result <: HList
  ](implicit zippable: Zippable[Prefix, List, Result]): Zippable[PrefixHead :: Prefix, ListHead :: List, (PrefixHead, ListHead) :: Result] =
    (p: PrefixHead :: Prefix, l: ListHead :: List) => {
      val HCons(prefixHead, prefix) = p
      val HCons(listHead, list) = l
      HCons((prefixHead, listHead), zippable(prefix, list))
    }
}
