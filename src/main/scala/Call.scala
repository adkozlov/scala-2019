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
}
