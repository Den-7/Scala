
sealed trait List[A]
//2 класса наследуются от трейта
case class Cons[A](head: A, tail: List[A]) extends List[A] //непустой лист. первй элемент отдельно
case class Nil[A]() extends List[A] //класс Nil представляет пустой лист

/** Напишите свои решения в виде функций. */
object L12 {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.
  def listIntEmpty(list: List[Int]): Boolean = list == Nil()
  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = listIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
  def listIntHead(list: List[Int]): Int = list match {
    case list: Cons[Int] => list.head
    case _ => -1
  }
  // используйте функцию из пункта (б) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = listIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?

  /** Да. Переопределить класс Nill */

  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */
  sealed trait Tree[A]
  case class Node[A](leftBranch: Tree[A], rightBranch: Tree[A]) extends Tree[A]
  case class Leaf[A](head: A) extends Tree[A]

  def main(args: Array[String]): Unit = {

    var list: List[Int] = Cons(1, Cons(2, Cons(3, Nil())))

    println
    print("Пустой лист: ")
    println(testListIntEmpty(list))
    print("Заполненный лист: ")
    println(testListIntEmpty(Nil()))

    println
    print("Пустой лист: ")
    println(testListIntHead(Nil()))
    print("Заполненный лист: ")
    println(testListIntHead(list))

    println
    println(
      Node(
        Node(Leaf(1),
          Leaf(2)),
        Leaf(3))
    )
  }
}
