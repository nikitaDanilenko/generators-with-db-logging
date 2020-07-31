import scala.concurrent.Future

case class DB[A](value: () => A) {

  def flatMap[B](f: A => DB[B]): DB[B] =
    f(value())

  def map[B](f: A => B): DB[B] =
    DB(() => f(value()))

}

object DB {

  def successful[A](a: => A): DB[A] = DB(() => a)

  def failed[A](message: String): DB[A] = throw new Throwable(message)

  def sequence[A](actions: Vector[DB[A]]): DB[Vector[A]] =
    DB(() => actions.map(_.value()))

  def seq(actions: Vector[DB[_]]): DB[Unit] = {
    DB {
      () =>
        actions.foldLeft(())((_, action) => action.value())
    }

  }

  def run[A](action: DB[A]): Future[A] = Future.successful(action.value())
}
