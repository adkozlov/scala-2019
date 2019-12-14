package hw03

trait Appendable[
Prefix <: HList,
List <: HList,
Result <: HList
] {
    def apply(prefix: Prefix, list: List): Result
}

object Appendable {

    // Nil ::: List = List
    implicit def nilAppendable[
    List <: HList
    ]: Appendable[Nil, List, List] =
        (_: Nil, list: List) => list

    // Prefix ::: List = Result
    // (Head :: Prefix) ::: List = Head :: Result
    implicit def appendable[
    Head,
    Prefix <: HList,
    List <: HList,
    Result <: HList
    ](implicit appendable: Appendable[Prefix, List, Result]): Appendable[Head :: Prefix, List, Head :: Result] =
        (cons: Head :: Prefix, list: List) => {
            val HCons(head, prefix) = cons
            HCons(head, appendable(prefix, list))
        }
}
