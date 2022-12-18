package o1.adventure
import scala.collection.mutable.Map

/* The class `Area` represents locations in a text adventure game world. A game world
   consists of areas. In general, an “area” can be pretty much anything: a room, a building,
   an acre of forest, or something completely different. What different areas have in
   common is that players can be located in them and that they can have exits leading to
   other, neighboring areas. An area has a name and a description.
   The variable inaccessible describes if the player can access the area(when variable has value None)
   or the reason why the player cannot access the area.
*/
class Area(val name: String, var description: String, var inaccessible: Option[String] = None):

  private val neighbors = Map[String, Area]()
  private val contents = Map[String, (Item, Int)]()

  /* Returns the area that can be reached from this area by moving in the given direction. The result
     is returned in an `Option`; `None` is returned if there is no exit in the given direction. */
  def neighbor(direction: String) = this.neighbors.get(direction)

  /* Adds an exit from this area to the given area. The neighboring area is reached by moving in
     the specified direction from this area. */
  def setNeighbor(direction: String, neighbor: Area): Unit =
    this.neighbors += direction -> neighbor

  /* Adds exits from this area to the given areas. Calling this method is equivalent to calling
     the `setNeighbor` method on each of the given direction–area pairs.*/
  def setNeighbors(exits: Vector[(String, Area)]): Unit =
    this.neighbors ++= exits

  /* Returns a multi-line description of the area as a player sees it. This includes a basic
    description of the area as well as information about exits and items. If there are no
    items present, the return value has the form "DESCRIPTION\n\nExits available:
    DIRECTIONS SEPARATED BY SPACES". If there are one or more items present, the return
    value has the form "DESCRIPTION\nYou see here: ITEMS SEPARATED BY SPACES\n\nExits available:
    DIRECTIONS SEPARATED BY SPACES". The items and directions are listed in an arbitrary order. */
  def fullDescription =
    def formItemsDescription: Vector[String] =
      this.contents.map( (name: String, item: (Item, Int) ) => name + "(x" + item(1) + ")" ).toVector

    val contentsList = if this.contents.isEmpty then "" else "\nYou see here: " + formItemsDescription.mkString(" ")
    val exitList = "\nExits available: " + this.neighbors.keys.mkString(" ")
    this.description + contentsList + exitList

  /* Returns a single-line description of the area for debugging purposes. */
  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

  def numberOfItems(itemName: String): Int =
    if this.contents.contains(itemName) then
      this.contents(itemName)(1)
    else
      0

  /* Places an item in the area so that it can be, for instance, picked up. */
  def addItem(item: Item): Unit =
      this.contents(item.name) = (item, this.numberOfItems(item.name) + 1)

  // Places multiple copies of the same item
  def addItemCopies(item: Item, copies: Int) =
    this.contents.put(item.name, (item, copies))

  /* Determines if the area contains an item of the given name. */
  def contains(itemName: String) = this.contents.contains(itemName)

  /* Removes the item of the given name from the area, assuming an item with that name
    was there to begin with. If there are multiple copies of the same item, only one of them is removed.
    Returns the removed item wrapped in an `Option` or `None`
    in the case there was no such item present. */
  def removeItem(itemName: String): Option[Item] =

    if !this.contents.contains(itemName) || this.contents(itemName)(1) == 1 then
      this.contents.remove(itemName).map(_(0))
    else
      val item = this.contents(itemName)(0)
      val occurances = this.contents(itemName)(1)
      this.contents(itemName) = (item, occurances - 1)
      Some(item)

  def getItem(itemName: String): Option[Item] = this.contents.get(itemName).map( _(0) )

end Area