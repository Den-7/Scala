
import scala.util.{Try, Failure, Success}
object LR41 {

  // a) Дан List[Int], верните элемент с индексом n
  def task1(list: List[Int],n: Int):Option[Int]={
    list match {
      case head :: tail => Some(list(n)) // если не пустой то выводим
      case Nil   => None
    }
  }
  // примените функцию из пункта (a) здесь, не изменяйте сигнатуру 
  def testGetNth(list: List[Int], n: Int): Option[Int] = task1(list,n)



  // b) Напишите функцию, увеличивающую число в два раза.
  def task2(n: Option[Int]):Option[Int]={
    n match {
      case Some(a) => Some(2*a)
      case None => None
    }
  }
  // примените функцию из пункта (b) здесь, не изменяйте сигнатуру
  def testDouble(n: Option[Int]): Option[Int] = task2(n)



  // c) Напишите функцию, проверяющую является ли число типа Int четным. Если так, верните Right. В противном случае, верните Left("Нечетное число.").
  //исп конструкцию Either.cond() если да - выводим число, если нет - строку
  def task3(n: Int):Either[String, Int]= {
    Either.cond(n % 2 == 0, n, "Нечетное число") match {
      case Left(i) => Left(i)
      case Right(s) => Right(s)
    }
  }

  // примените функцию из пункта (c) здесь, не изменяйте сигнатуру
  def testIsEven(n: Int): Either[String, Int] = task3(n)


  // d) Напишите функцию, реализующую безопасное деление целых чисел. Верните Right с результатом или Left("Вы не можете делить на ноль.").
  def task4(a: Int, b: Int): Either[String, Int] = {
    Either.cond(b != 0, a / b, "Нельзя делить на ноль") match {
      case Left(i) => Left(i)
      case Right(s) => Right(s)
    }
  }
  // примените функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testSafeDivide(a: Int, b: Int): Either[String, Int] = task4(a, b)


  // e) Обработайте исключения функции с побочным эффектом вернув 0.
  def task5(impure: String => Int, str: String): Try[Int] = {
    Try(impure(str)) match {
      case Success(a) => Success(a)
      case Failure(error) => Failure(error)
    }
  }
  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testGoodOldJava(impure: String => Int, str: String): Try[Int] = task5(impure, str)

  def main(args: Array[String]) = {
    println
    println(testGetNth(List(5,10,15,20,25),2))
    println
    println(testDouble(Some(50)))
    println
    println(testIsEven(5))
    println
    println(testSafeDivide(100,5))
    println(testSafeDivide(100,0))
    def func1(str:String):Int=str(0)/0
    println
    println(testGoodOldJava(func1,"String"))
  }
}