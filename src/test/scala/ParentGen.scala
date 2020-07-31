import java.util.UUID

import GenWithDB.{GenWithDB, _}
import cats.instances.vector._
import org.scalacheck.Gen

object ParentGen {

  def creationGen: Gen[Parent.Creation] =
    Gen.alphaNumStr.map(Parent.Creation.apply)

  def parentGenDB: GenWithDB[Parent] = for {
    parentCreation <- GenWithDB.liftF(creationGen)
    parent = Parent.Creation.create(id = UUID.randomUUID() ,parentCreation)
    _ <- GenWithDB.tellSingle(ParentDAO.insert(parent))
  } yield parent

}
