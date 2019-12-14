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
  implicit def nilZippable[List <: Nil, Suffix <: Nil]: Zippable[List, Suffix, Nil] =
    (_: List, _: Suffix) => HNil

  // Prefix zip List = Result
  // (PrefixHead :: Prefix) zip (ListHead :: List) = (PrefixHead, ListHead) :: Result
  implicit def zippable[
    ListHead,
    List <: HList,
    SuffixHead,
    Suffix <: HList,
    Result <: HList
  ](implicit zippable: Zippable[List, Suffix, Result])
  : Zippable[ListHead :: List, SuffixHead :: Suffix, (ListHead, SuffixHead) :: Result] =
    (list: ListHead :: List, suffix: SuffixHead :: Suffix) => {
      val HCons(listHead, listTail) = list
      val HCons(suffixHead, suffixTail) = suffix
      HCons((listHead, suffixHead), zippable(listTail, suffixTail))
    }
}
