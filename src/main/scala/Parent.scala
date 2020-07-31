import java.util.UUID

case class Parent(id: UUID, name: String)

object Parent {
  case class Creation(name: String)

  object Creation {

    def create(id: UUID, creation: Creation): Parent =
      Parent(id = id, name = creation.name)

  }

}
