import scala.annotation.tailrec
/** Реализуйте функции для решения следующих задач.
 * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
 * рекурсия будет хвостовой если:
 *   1. рекурсия реализуется в одном направлении
 *   2. вызов рекурсивной функции будет последней операцией перед возвратом
 */
object L13 {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(rem: List[A], agg: Int): Int = rem match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(origin: List[A], reverse: List[A]): List[A] = origin match {
      case Cons(n, t) => loop(t, Cons(n,  reverse))
      case Nil() => reverse
    }
    loop(list, new Nil[A])
  }
  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse(list)


  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   *
   *
   */
  def map[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], changed: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), changed))
      case Nil() => reverse(changed)
    }
    loop(list, Nil())
  }
  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)


  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   *
   * рекурсивно берем каждый элемент левого листа и добавляем в начало правого листа
   * предварительно к левому листу применяется реверс
   */
  def append[A](l: List[A], r: List[A]): List[A] = {
    @tailrec
    def loop(left: List[A], right: List[A] ): List[A] = left match {
      case Cons(n, t) => loop(t, Cons(n, right))
      case Nil() => right
    }
    loop(reverse(l), r)
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l,r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   *
   *
   * рекурсивно берем элемент, применяем функцию, выводим. -> лист листов
   */
  def flatMap[A, B](list: List[A])(f: A => B): List[B] = {
    @tailrec
    def loop(origin: List[A], result: List[B]): List[B] = origin match {
      case Cons(n, t) => loop(t, Cons(f(n), result))
      case Nil() => reverse(result)
    }
    loop(list,new Nil[B])
  }
  // используйте функцию из пункта (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = Nil()

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */
    /** при работе с деревом написать ф-цию с хвостовой рекурсией нельзя изза возникновения различных проблем */

  def main(args: Array[String]): Unit = {
    println
    println(testReverse(Cons(5, Cons(10, Cons(15, Nil())))))
    println
    println(testMap(Cons(5, Cons(10, Cons(15, Nil()))), (x: Int) => x + 5))
    println
    println(testAppend(Cons[Int](1, Cons(2, Cons(3, Nil()))), Cons[Int](4, Cons(5, Cons(6, Nil())))))
    println
    println(flatMap(Cons[Int](5, Cons(10, Cons(15, Nil()))))((x: Int) => Cons(x, Nil())))
  }
}
