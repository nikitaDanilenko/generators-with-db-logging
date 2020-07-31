import java.util.UUID

import GenWithDB.{ GenWithDB, _ }
import cats.instances.vector._
import org.scalacheck.Gen

object ChildGen {

  def creationGen: Gen[Child.Creation] =
    Gen.choose(0, 1000).map(Child.Creation.apply)

  def childGenDB(parentId: UUID): GenWithDB[Child] =
    for {
      childCreation <- GenWithDB.liftF(creationGen)
      child = Child.Creation.create(id = UUID.randomUUID(), parentId = parentId, creation = childCreation)
      _ <- GenWithDB.tellSingle(ChildDAO.insert(child))
    } yield child

}
