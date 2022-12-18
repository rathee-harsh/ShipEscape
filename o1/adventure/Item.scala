package o1.adventure


// Creating some Tools to be used in the game. (See class Tool below)
val shovel = Tool("shovel", "Used to Mine coal", UnminedCoal)
val axe = Tool("axe", "Use it to cut down trees", Tree)
val pickaxe = Tool("pickaxe", "Use it break rocks and boulders", Boulder)

// Creating some non-usable items. (See class NonUsable below)
val seaMap = NonUsable("sea map", "Use it to get back to your home")
val log = NonUsable("log", "A small piece of wooden log.")
val stick = NonUsable("stick", "Used to repair tools.")
val stone = NonUsable("stone", "")
val coal = NonUsable("coal", "used to make torches")
val torch = Craftable("torch", "The man must be happy, I got a torch.", Vector((coal, 2), (stick, 1)))
val wall = Craftable("wall", "Use it to repair ship.", Vector((log, 2), (stone, 3)))
val craftables = Map[String, Craftable]("torch" -> torch, "wall" -> wall)


/* The trait `Item` represents items in a text adventure game. Each item has a name
 , a description and a mehtod use. The method is an abstract one that is defined by objects
 inheriting from the trait.
 N.B. It is assumed, but not enforced by this class, that items have unique names.
 That is, no two items in a game world have the same name. */
trait Item(val name: String, val description: String):
  val canPick: Boolean
  // Returns a short textual representation of the item (its name, that is).
  override def toString = this.name
end Item

trait Extractable(val toolRequired: Tool) extends Item:
  val canPick = false
  def itemsOnMine: Vector[Item]
end Extractable

trait Usable extends Item:
  def use(player: Player): String
end Usable

class NonUsable(name: String, description: String) extends Item(name, description):
  val canPick = true
end NonUsable

class Craftable(name: String, description: String, itemsRequired: Vector[(Item, Int)]) extends NonUsable(name, description):
  def craft(player: Player) =
    if itemsRequired.forall( (item: Item, count: Int) => count <= player.getCountOf(item.name) ) then
      itemsRequired.foreach( (item: Item, count: Int) => for _ <- 1 to count do player.removeItem(item.name) )
      player.location.addItem(this)
      player.get(this.name)
      player.increaseHunger(1)
      player.game.advanceTime()
      "You make a " + this.name
    else
      val requiredMaterials = this.itemsRequired.map( (item: (Item, Int)) => item(0).name + "(x" + item(1) + ")").toVector.mkString(", ")
      "You don't have the required materials for crafting this item.\nMaterials Required: " + requiredMaterials
end Craftable

object Apple extends Usable, Item("apple", "Eat it to reduce hunger."):
  val canPick = true
  def use(player: Player): String =
    player.hunger = 0
    player.game.advanceTime()
    player.removeItem("apple")
    "You eat an " + this.name
end Apple

object Tree extends Extractable(axe), Item("tree", "Use a pickaxe to chop it"):
  def itemsOnMine: Vector[Item] = Vector.fill(1)(log) ++ Vector(Apple) ++ Vector(stick)
end Tree

object Boulder extends Extractable(pickaxe), Item("boulder", "I must use a pickaxe to remove this from my path"):
  def itemsOnMine = Vector.fill(10)(stone)
end Boulder

object UnminedCoal extends Extractable(shovel), Item("unmined coal", "Can be mined using a shovel."):
  def itemsOnMine = Vector(coal)
end UnminedCoal

class Tool(name: String, description: String, val target: Extractable) extends Usable, Item(name, description):
  val canPick = true
  private var health = 3
  def use(player: Player) =
    if this.health != 0 then
      if player.location.getItem(target.name).isDefined then
        target.itemsOnMine.foreach( player.location.addItem )
        player.location.removeItem(target.name)
        this.health = math.max(0, this.health - 1)
        player.game.advanceTime()
        player.increaseHunger(1);
        "You use " + this.name + " on a " + this.target.name + "."
      else
        "No " + this.target.name + " here."
    else
      "This tool is damaged, repair it with a stick."
  def repair(player: Player) =
    if player.has("stick") then
      player.removeItem("stick")
      this.health = 3
      player.increaseHunger(1)
      player.game.advanceTime()
      "You repair the tool."
    else
      "You need a stick to repair this tool."
end Tool

// Represents the man in the cave in game. Note that the game assusmes non-human players to be items.
object Man extends Item("man", "He is looking to be in a good mood."):
  val canPick = false
  private var alreadyMet = false
  private var torchGiven = false

  def speak(player: Player): String =
    if !this.alreadyMet then
      this.alreadyMet = true
      player.game.workshop.inaccessible = None
      player.game.updateLocationOfMan(player.game.workshop)
      player.game.advanceCycle()
      player.game.cave.inaccessible = Some("Nothing useful anymore in that cave. I should not waste time going there.")
      s"""You talk to the man and describle your situation...\nMan: Oh I'm really sorry to hear that.
      I can give you a sea map which you can use after you find your ship but I also need your help. I want you to make me a torch.
      Come with me.\nYou enter into the man's house... He takes you to his yard.\nMan: To make a torch, you'll need 2 pieces of coal and 1 stick. To get these items, you need some tools which you'll find here.
      Take them and get me a torch. You will find me in my workshop in your east. Now go."""
    else if !this.torchGiven then
      if player.has("torch") then
        player.removeItem("torch")
        this.torchGiven = true
        player.location.addItem(seaMap)
        player.get(seaMap.name)
        "Man: Ah. Thank You! As promised, take this sea map. Also keep those tools with you, you might need them in your journey."
      else
        "Man: Did you get it? No! Please hurry up then."
    else
      "Man: Unfortunately, that's all I can do to help you."

end Man