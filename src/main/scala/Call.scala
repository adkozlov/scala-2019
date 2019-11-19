import java.time.LocalDate

class Call(
              val idFrom: Int,
              val idTo: Int,
              val duration: Double,
              val cost: Double,
              val date: LocalDate
          ) {
    def isBetween(from: LocalDate, to: LocalDate): Boolean =
        (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to) || date.isEqual(to))

    override def toString: String =
        s"$idFrom --> $idTo duration: $duration, cost: $cost, date: $date"

    override def equals(obj: Any): Boolean =
        obj match {
            case obj: Call =>
                idFrom.equals(obj.idFrom) &&
                idTo.equals(obj.idTo) &&
                duration.equals(obj.duration) &&
                cost.equals(obj.cost) &&
                date.equals(obj.date)
            case _ => false
        }
}
