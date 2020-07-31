import java.util.UUID

import scala.collection.mutable

object ParentDAO {
  private val parentTable: mutable.Map[UUID, Parent] = mutable.Map.empty

  def insert(parent: Parent): DB[Parent] = {
    parentTable += (parent.id -> parent)
    DB.successful(parent)
  }

  def read(parentId: UUID): DB[Option[Parent]] =
    DB.successful(parentTable.get(parentId))

  def delete(parentId: UUID): DB[Unit] = {
    for {
      children <- ChildDAO.findByParentId(parentId)
      _ <- DB.sequence(children.map(child => ChildDAO.delete(child.id)))
      _ = parentTable.remove(parentId)
    } yield ()
  }


}
