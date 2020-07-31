import java.util.UUID
case class Child(id: UUID, parentId: UUID, number: Int)

object Child {
  case class Creation(number: Int)
  object Creation {
    def create(id: UUID, parentId: UUID, creation: Creation): Child =
      Child(id = id, parentId = parentId, number = creation.number)
  }
}
