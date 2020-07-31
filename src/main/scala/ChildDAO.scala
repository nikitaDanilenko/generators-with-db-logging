import java.util.UUID

import scala.collection.mutable

object ChildDAO {
  private val childTable: mutable.Map[UUID, Child] = mutable.Map.empty

  def insert(child: Child): DB[Child] = {
    for {
      result <- ParentDAO.read(child.parentId)
      insertion <- result match {
        case Some(_) =>
          childTable += (child.id -> child)
          DB.successful(child)
        case _ => DB.failed(s"Parent constraint violated for child with id ${child.id}")
      }
    } yield insertion
  }

  def read(childId: UUID): DB[Option[Child]] =
    DB.successful(childTable.get(childId))

  def findByParentId(parentId: UUID): DB[Vector[Child]] =
    DB.successful(childTable.values.filter(_.parentId == parentId).toVector)

  def delete(childId: UUID): DB[Unit] = {
    childTable.remove(childId)
    DB.successful(())
  }
}
