package ipoemi.scalawithcats.exercise

object `9.2` {

  import cats.Monoid
  import cats.syntax.monoid._

  def foldMap[A, B: Monoid](v: Vector[A])(f: A => B) =
  //v.foldLeft(Monoid[B].empty)((acc, x) => Monoid[B].combine(acc, f(x)))
    v.foldLeft(Monoid[B].empty)(_ |+| f(_))

}

object `9.3.3` {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  import cats.Monoid
  import cats.instances.future._
  import cats.syntax.monoid._
  import cats.syntax.applicative._

  import `9.2`._

  /*
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cpuCnt = Runtime.getRuntime.availableProcessors()
    val sizePerCpu = (1.0 * values.size / cpuCnt).ceil.toInt
    values.grouped(sizePerCpu).foldLeft(Monoid[B].empty.pure[Future]) { (fur, g) =>
      for {
        x <- fur
        y <- foldMap(g)(func).pure[Future]
      } yield x |+| y
    }
  }
  */

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cpuCnt = Runtime.getRuntime.availableProcessors()
    val sizePerCpu = (1.0 * values.size / cpuCnt).ceil.toInt
    val futures = values.grouped(sizePerCpu).map(g => Future(foldMap(g)(func)))
    Future.sequence(futures).map(Monoid[B].combineAll(_))
  }
}

object `9.3.4` {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  import cats.Foldable
  import cats.Traverse
  import cats.Monoid

  import cats.instances.future._
  import cats.instances.vector._

  import cats.syntax.monoid._
  import cats.syntax.applicative._
  import cats.syntax.foldable._
  import cats.syntax.traverse._

  def foldMap[F[_] : Foldable, A, B: Monoid](fa: F[A])(fn: A => B): B =
    Foldable[F].foldLeft(fa, Monoid[B].empty)(_ |+| fn(_))

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val cpuCnt = Runtime.getRuntime.availableProcessors()
    val sizePerCpu = (1.0 * values.size / cpuCnt).ceil.toInt
    val groups = values.grouped(sizePerCpu)
    groups
      .toVector
      .traverse(g => g.foldMap(func).pure[Future])
      .map(_.combineAll)
  }
}
