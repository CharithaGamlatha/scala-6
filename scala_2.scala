import scala.io.StdIn.readLine

object StudentRecords {
  type StudentRecord = (String, Int, Int, Double, Char)

  // a. Function getStudentInfo
  def getStudentInfo(name: String, marks: Int, totalMarks: Int): StudentRecord = {
    val percentage = (marks.toDouble / totalMarks) * 100
    val grade = percentage match {
      case p if p >= 90 => 'A'
      case p if p >= 75 => 'B'
      case p if p >= 50 => 'C'
      case _ => 'D'
    }
    (name, marks, totalMarks, percentage, grade)
  }

  // b. Function printStudentRecord
  def printStudentRecord(record: StudentRecord): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Name: $name")
    println(s"Marks: $marks / $totalMarks")
    println(s"Percentage: ${percentage}%.2f%%".format(percentage))
    println(s"Grade: $grade")
  }

  // c. Function validateInput
  def validateInput(name: String, marks: String, totalMarks: String): (Boolean, Option[String]) = {
    if (name.isEmpty) return (false, Some("Name cannot be empty"))
    val marksInt = try { Some(marks.toInt) } catch { case _: NumberFormatException => None }
    val totalMarksInt = try { Some(totalMarks.toInt) } catch { case _: NumberFormatException => None }
    (marksInt, totalMarksInt) match {
      case (Some(m), Some(t)) if m >= 0 && m <= t => (true, None)
      case (Some(m), Some(t)) if m < 0 => (false, Some("Marks cannot be negative"))
      case (Some(m), Some(t)) if m > t => (false, Some("Marks cannot exceed total marks"))
      case _ => (false, Some("Marks and total marks must be valid integers"))
    }
  }

  // d. Function getStudentInfoWithRetry
  def getStudentInfoWithRetry(): StudentRecord = {
    var valid = false
    var name, marks, totalMarks = ""
    var errorMessage: Option[String] = None

    while (!valid) {
      println("Enter student's name:")
      name = readLine()
      println("Enter marks obtained:")
      marks = readLine()
      println("Enter total possible marks:")
      totalMarks = readLine()

      val validationResult = validateInput(name, marks, totalMarks)
      valid = validationResult._1
      errorMessage = validationResult._2

      if (!valid) {
        println(s"Invalid input: ${errorMessage.getOrElse("Unknown error")}")
      }
    }

    getStudentInfo(name, marks.toInt, totalMarks.toInt)
  }

  def main(args: Array[String]): Unit = {
    val studentRecord = getStudentInfoWithRetry()
    printStudentRecord(studentRecord)
  }
}