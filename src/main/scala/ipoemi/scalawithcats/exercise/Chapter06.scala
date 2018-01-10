package ipoemi.scalawithcats.exercise

object `6.3.1.1` {
  import cats.Monad

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
    Monad[M].flatMap(x)(a => Monad[M].map(y)(b => (a, b)))
}

