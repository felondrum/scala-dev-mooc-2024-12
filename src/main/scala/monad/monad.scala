package object monad {

  /**
   * Реализуйте методы map / flatMap / withFilter чтобы работал код и законы монад соблюдались
   * HINT: для проверки на пустой элемент можно использовать eq
   */

  trait Wrap[+A] {

    def get: A

    def pure[R](x: R): Wrap[R] = {
      if (x == null) EmptyWrap else NonEmptyWrap(x)
    }

    def flatMap[R](f: A => Wrap[R]): Wrap[R] = {
      f(get)
    }

    // HINT: map можно реализовать через pure и flatMap
    def map[R](f: A => R): Wrap[R] = {
      flatMap(_ => pure(f(get)))
      // Следовал HINT, но как будто flatMap излишний, pure(f(get)) достаточно
    }

    def withFilter(f: A => Boolean): Wrap[A] = {
      if (f(get)) pure(get) else EmptyWrap
      // Не понял как реализовать без выбрасывания исключения не переписывая EmptyWrap
    }

  }

  object Wrap {
    def empty[R]: Wrap[R] = EmptyWrap
  }

  case class NonEmptyWrap[A](result: A) extends Wrap[A] {
    override def get: A = result
  } // pure

  case object EmptyWrap extends Wrap[Nothing] {
    override def get: Nothing = throw new NoSuchElementException("Wrap.get")
  } // bottom, null element

}