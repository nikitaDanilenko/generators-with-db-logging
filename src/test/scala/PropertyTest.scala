import GenWithDB.{GenWithDB, _}
import cats.instances.vector._
import org.scalacheck.{Prop, Properties}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PropertyTest extends Properties("PropertyTest") {

  private case class ParentWithChildren(parent: Parent, children: Vector[Child])

  private val parentChildrenGen: GenWithDB[ParentWithChildren] = for {
    parent <- ParentGen.parentGenDB
    children <- GenWithDB.nonEmptyCollectionOf(ChildGen.childGenDB(parent.id))
  } yield ParentWithChildren(parent, children)

  property("Children for parent inserted correctly") = Prop.forAll(parentChildrenGen.run) {
    case (actions, parentWithChildren) =>
      //Without the following line, no actions are executed, and the test fails.
      DB.run(DB.seq(actions))
      val childrenInDB = Await.result(DB.run(ChildDAO.findByParentId(parentWithChildren.parent.id)), Duration.Inf)
      val parentInDB = Await.result(DB.run(ParentDAO.read(parentWithChildren.parent.id)), Duration.Inf)
      val parentProperty = parentInDB.contains(parentWithChildren.parent)
      val childrenProperty = childrenInDB.toSet == parentWithChildren.children.toSet
      Prop.all(parentProperty, childrenProperty)
  }

}
