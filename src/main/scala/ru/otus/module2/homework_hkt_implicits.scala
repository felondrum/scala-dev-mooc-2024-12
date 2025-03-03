package ru.otus.module2

object homework_hkt_implicits{

  trait Bindable[F[_], A]{
    def map[B](fa: F[A])(f: A => B): F[B]
    def flatMap[B](fa: F[A])(f: A => F[B]): F[B]
  }

  def tuplef[F[_], A, B](fa: F[A], fb: F[B])(implicit b1: Bindable[F, A], b2: Bindable[F, B]): F[(A, B)] =
    b1.flatMap(fa)(a => b2.map(fb)(b => (a, b)))

}