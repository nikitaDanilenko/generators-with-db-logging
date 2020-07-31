import cats.{ Monad, StackSafeMonad }
import cats.data.WriterT
import org.scalacheck.Gen
import cats.instances.vector._

object GenWithDB {

  implicit val genMonad: Monad[Gen] = new StackSafeMonad[Gen] {
    override def pure[A](x: A): Gen[A] = Gen.const(x)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }

  // A cleaner solution would use HLists.
  type DBVector = Vector[DB[_]]

  type GenWithDB[A] = WriterT[Gen, DBVector, A]

  def tell[A](actions: DBVector): GenWithDB[Unit] =
    WriterT.tell[Gen, DBVector](actions)

  def tellSingle[A](action: DB[A]): GenWithDB[Unit] =
    tell(Vector(action))

  def liftF[A](gen: Gen[A]): GenWithDB[A] =
    WriterT.liftF[Gen, DBVector, A](gen)

  def nonEmptyCollectionOf[A](gen: GenWithDB[A]): GenWithDB[Vector[A]] = for {
    list <- liftF(Gen.nonEmptyListOf(Gen.const(())))
    xs <- liftF(Gen.sequence[Vector[(DBVector, A)], (DBVector, A)](Vector.fill(list.length)(gen).map(_.run)))
    actionsResults = xs.unzip
    _ <- tell(actionsResults._1.flatten)
  } yield actionsResults._2

}
