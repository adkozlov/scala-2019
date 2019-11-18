import org.joda.time.DateTime
import org.scalatest.FunSuite
import ru.spbau.jvm.scala.{Call, Database, Employee, EmployeeCall, EmployeeTotal}
import ru.spbau.jvm.scala.Main.{CALLS_FILE_NAME, EMPLOYEES_FILE_NAME, toDate}

class DatabaseSuite extends FunSuite {
  val database = new Database(CALLS_FILE_NAME, EMPLOYEES_FILE_NAME)

  test("Full month call's list should consist of 8 calls.") {
    val list = database.calls(toDate("01.09.2013"), toDate("30.09.2013"))
    assert(list.size == 8)
    assert(list.contains(EmployeeCall(Employee("Tom", "Addison"), "212-85-06", 42, 4.2f)))
    assert(list.contains(EmployeeCall(Employee("Tom", "Addison"), "+7(921)777-56-77", 2, 0.2f)))
    assert(list.contains(EmployeeCall(Employee("Max", "Baker"), "+7(926)766-66-67", 15, 1.5f)))
    assert(list.contains(EmployeeCall(Employee("Tom", "Addison"), "+7(931)733-56-67", 32, 3.2f)))
    assert(list.contains(EmployeeCall(Employee("Ted", "Banks"), "+7(921)786-56-99", 12, 1.2f)))
    assert(list.contains(EmployeeCall(Employee("Tom", "Addison"), "+7(921)786-56-67", 1, 0.1f)))
    assert(list.contains(EmployeeCall(Employee("Max", "Baker"), "212-85-06", 3, 0.3f)))
    assert(list.contains(EmployeeCall(Employee("Ken", "Anderson"), "+7(911)456-45-45", 10, 1f)))
  }

  test("Half month call's list should consist of 2 calls.") {
    val list = database.calls(toDate("01.09.2013"), toDate("15.09.2013"))
    assert(list.size == 2)
    assert(list.contains(EmployeeCall(Employee("Tom", "Addison"), "+7(921)777-56-77", 2, 0.2f)))
    assert(list.contains(EmployeeCall(Employee("Tom", "Addison"), "+7(931)733-56-67", 32, 3.2f)))
  }

  test("Average call duration should be 14s.") {
    val avg = database.averageCallDuration()
    assert(avg == 14f)
  }

  test("Total month cost should be 11.7.") {
    val cost = database.totalCost()
    assert(Math.abs(cost - 11.7) < 0.1)
  }

  test("Half month cost should be 8.3.") {
    val cost = database.totalCost(toDate("15.09.2013"))
    assert(Math.abs(cost - 8.3) < 0.1)
  }

  test("Quarter month cost should be 3.4.") {
    val cost = database.totalCost(toDate("07.09.2013"), toDate("15.09.2013"))
    assert(Math.abs(cost - 3.4) < 0.1)
  }

  test("Existed employee should have number.") {
    val number = database.getPhone(Employee("Ted", "Banks")).get
    assert(number == "+7(921)687-65-76")
  }

  test("Nonexistent employee should not have number.") {
    val number = database.getPhone(Employee("Tom", "Black"))
    assert(number.isEmpty)
  }

  test("There should be 2 calls from Max Baker.") {
    val calls = database.callsFromEmployee(Employee("Max", "Baker")).get
    assert(calls.size == 2)
    assert(calls.contains(Call("+7(921)876-65-76", "+7(926)766-66-67", DateTime.parse("2013-09-19T09:09:00.000Z"), 15, 1.5f)))
    assert(calls.contains(Call("+7(921)876-65-76", "212-85-06", DateTime.parse("2013-09-19T15:09:00.000Z"), 3, 0.3f)))
  }

  test("There should be zero calls from Leo Blackman") {
    val calls = database.callsFromEmployee(Employee("Leo", "Blackman")).get
    assert(calls.isEmpty)
  }

  test("There no calls from nonexistent employee.") {
    val calls = database.callsFromEmployee(Employee("Tom", "Black"))
    assert(calls.isEmpty)
  }

  test("There should be 4 groups.") {
    val groups = database.groupCallsByEmployee(toDate("01.09.2013"), toDate("30.09.2013"))
    assert(groups.size == 4)
    assert(groups.contains(EmployeeTotal(Employee("Ted", "Banks"), 12, 1.2f)))
    assert(groups.contains(EmployeeTotal(Employee("Max", "Baker"), 18, 1.8f)))
    assert(groups.contains(EmployeeTotal(Employee("Ken", "Anderson"), 10, 1.0f)))
    assert(groups.contains(EmployeeTotal(Employee("Tom", "Addison"), 77, 7.6999993f)))
  }

  test("There are 5 employees.") {
    val employees = database.getEmployees
    assert(employees.size == 5)
    assert(employees.contains((Employee("Ted", "Banks"), "+7(921)687-65-76")))
    assert(employees.contains((Employee("Leo", "Blackman"), "+7(921)786-56-67")))
    assert(employees.contains((Employee("Ken", "Anderson"), "+7(921)768-56-67")))
    assert(employees.contains((Employee("Tom", "Addison"), "+7(921)678-56-67")))
    assert(employees.contains((Employee("Max", "Baker"), "+7(921)876-65-76")))
  }
}
