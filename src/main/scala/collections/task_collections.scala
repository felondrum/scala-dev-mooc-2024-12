package collections

object task_collections {

  def isASCIIString(str: String): Boolean = str.matches("[A-Za-z]+")

  /**
   * Реализуйте метод который первый элемент списка не изменяет, а для последующих алгоритм следующий:
   * если isASCIIString is TRUE тогда пусть каждый элемент строки будет в ВЕРХНЕМ регистре
   * если isASCIIString is FALSE тогда пусть каждый элемент строки будет в нижнем регистре
   * Пример:
   * capitalizeIgnoringASCII(List("Lorem", "ipsum" ,"dolor", "sit", "amet")) -> List("Lorem", "IPSUM", "DOLOR", "SIT", "AMET")
   * capitalizeIgnoringASCII(List("Оказывается", "," "ЗвУк", "КЛАВИШЬ", "печатной", "Машинки", "не", "СТАЛ", "ограничивающим", "фактором")) ->
   * List("Оказывается", "," "звук", "КЛАВИШЬ", "печатной", "машинки", "не", "стал", "ограничивающим", "фактором")
   * HINT: Тут удобно использовать collect и zipWithIndex
   *
   * **/
  def capitalizeIgnoringASCII(text: List[String]): List[String] = {
    def capitalizeIgnoringASCIIPart: PartialFunction[(String, Int), String] = {
      case head: (String, Int) if head._2 == 0 => head._1
      case tailIsASCII: (String, Int) if tailIsASCII._2 > 0 && isASCIIString(tailIsASCII._1) => tailIsASCII._1.toUpperCase
      case tailIsNotASCII: (String, Int) => tailIsNotASCII._1.toLowerCase
    }

    text.zipWithIndex.collect(capitalizeIgnoringASCIIPart)
  }

  /**
   *
   * Компьютер сгенерировал текст используя вместо прописных чисел, числа в виде цифр, помогите компьютеру заменить цифры на числа
   * В тексте встречаются числа от 0 до 9
   *
   * Реализуйте метод который цифровые значения в строке заменяет на числа: 1 -> one, 2 -> two
   *
   * HINT: Для всех возможных комбинаций чисел стоит использовать Map
   * **/
  def numbersToNumericString(text: String): String = {
    val map: Map[String, String] = Map apply
      ("0" -> "zero",
        "1" -> "one",
        "2" -> "two",
        "3" -> "three",
        "4" -> "four",
        "5" -> "five",
        "6" -> "six",
        "7" -> "seven",
        "8" -> "eight",
        "9" -> "nine")
    val Space = " "
    text.split(Space)
      .map(word => map.getOrElse(word, word))
      .mkString(Space)
  }

  /**
   *
   * У нас есть два дилера со списками машин которые они обслуживают и продают (case class Auto(mark: String, model: String)).
   * Базы данных дилеров содержат тысячи и больше записей. Нет гарантии что записи уникальные и не имеют повторений
   * HINT: Set
   * HINT2: Iterable стоит изменить
   * **/

  case class Auto(mark: String, model: String)

  /**
   * Хотим узнать какие машины можно обслужить учитывая этих двух дилеров
   * Реализуйте метод который примет две коллекции (два источника) и вернёт объединенный список уникальный значений
   **/
  def intersectionAuto(dealerOne: Vector[Auto], dealerTwo: Seq[Auto]): Set[Auto] = {
    (dealerOne ++ dealerTwo).toSet
  }

  /**
   * Хотим узнать какие машины обслуживается в первом дилеромском центре, но не обслуживаются во втором
   * Реализуйте метод который примет две коллекции (два источника)
   * и вернёт уникальный список машин обслуживающихся в первом дилерском центре и не обслуживающимся во втором
   * */
  def filterAllLeftDealerAutoWithoutRight(dealerOne: Vector[Auto], dealerTwo: Seq[Auto]): Iterable[Auto] = {
    dealerOne
      .filter(fromFirst => !dealerTwo.contains(fromFirst)).toSet
  }
}