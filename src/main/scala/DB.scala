case class DB[A](value: A) {

  def flatMap[B](f: A => DB[B]): DB[B] =
    f(value)

  def map[B](f: A => B): DB[B] =
    DB(f(value))

}

object DB {
  def successful[A](a: A): DB[A] = DB(a)

  def sequence[A](actions: Vector[DB[A]]): DB[Vector[A]] =
    DB(actions.map(_.value))

}
