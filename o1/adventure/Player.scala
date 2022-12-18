package o1.adventure
import scala.collection.mutable.Map

/* A `Player` object represents a player character controlled by the real-life user of the program.
A player object’s state is mutable: the player’s location and possessions can change,
for instance. */
class Player(val game: ShipEscape, startingArea: Area):

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var hasWonGame = false

  def gameWon = this.hasWonGame


  //  Denotes the hunger level of the player.
  //  NOTE: Hunger level increases when the player uses the following command -
  //    make(increase by 1), repair(increase by 1), use (increase by 1), go(increse by 1), rest (incerase by 2), swim (increase by 3)
  var hunger = 0

  // Denotes the level of cold the player feels.
  // Cold increases in night by a value of 1.5 every turn except when the player is inside the house, workshop or the ship or if the player has a torch.
  var cold = 0.0

  /* The variable represents whether the player is dead or not. If the variable has value 'None', the player is still alive.
  When the player dies, its value is updated to Some(reasonForDeath)
  The player dies if any one of the condition is met -
    1. Hunger level crosses 10.
    2. Cold level crosses 10
    3. Player tries to rest in water - drown. */
  private var deathStatus: Option[String] = None

  private val possessions = Map[String, (Item, Int)]()     // container of all the items that the player has

  def hasDied = this.deathStatus.isDefined

  /* Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  // Returns the player’s current location.
  def location = this.currentLocation

  // Count the number of a certain item in the player's inventory
  def getCountOf(itemName: String): Int =
    this.possessions.get(itemName) match
      case Some((_, count: Int)) => count
      case None => 0

  // Increase the cold level by 1.5 to a maximum value of 10
  def increaseCold() =
    this.cold = math.min(10, this.cold + 1.5)
  // Decrease the cold level by 1 only till 0
  def decreaseCold() =
    this.cold = math.max(0, this.cold - 1)

  // Increase hunger by a value to a maximum of 10
  def increaseHunger(increaseBy: Int) =
    this.hunger = math.min(10, this.hunger + increaseBy)

  // This function updates the current status of the player
  def updateStatus(): Unit =
    if this.deathStatus.isEmpty then
      if this.hunger == 10 then
          this.deathStatus = Some("You are too hungry to do anything now. You starve to death!")
          return()
      else if this.cold == 10.0 then
        this.deathStatus = Some("It's too cold out here for you to survive. You freeze to death!")
        return()
      else
        this.deathStatus = None

    if this.location == this.game.ship && this.has("wall") then
      this.removeItem("wall")
      if this.has("sea map") then
        this.hasWonGame = true
      else
        this.game.ship.description = "I just need a map now to get back home!"
        println("I just need a map now to get back home!")


  def causeOfDeath = this.deathStatus.getOrElse("The player is still alive!")


  private def tryToGo(direction: String, destination: Option[Area], action: String) =
    val toGo = destination.getOrElse(this.currentLocation)
      if toGo.inaccessible.isEmpty then
        this.currentLocation = toGo

      /* The output of the siwm/walk command that would be returned. There are three different output types
      corresponding to the following situations:
      1. the neighbor in the direction exist and is accesible
      2. the neighbor exist but is inaccessible
      3. the neighbor doesn't exist */
      if destination.isDefined then
        toGo.inaccessible.getOrElse("You " + action + " "  + direction + ".")
      else
        "You can't " + action + " " + direction + "."


  // Try to swim to a direction if it exists.
  def swim(direction: String): String =
    val destination = this.location.neighbor(direction)
    if this.location != this.game.river then
      "No water body here to swim."
    else
      if destination.exists(_.inaccessible.isEmpty) then
        this.increaseHunger(3)
        this.game.advanceTime()
      this.tryToGo(direction, destination, "swim")


  /* Attempts to move the player in the given direction. This is successful if there
  is an exit from the player’s current location towards the direction name. Returns
  a description of the result: "You go/swim DIRECTION." or "You can't go/swim DIRECTION." */
  def walk(direction: String): String =
    val destination = this.location.neighbor(direction)
    if this.location == this.game.river then
      "You're no Jesus. Try swimming"
    else
      // increase hunger level and advance time only if the neighboring area exists and is is accesible.
      val output = this.tryToGo(direction, destination, "walk")
      if destination.exists(_.inaccessible.isEmpty) then
        this.increaseHunger(1)
        this.game.advanceTime()
      output


  /* Causes the player to rest for a short while (This causes the time of the day to change and
  increases the hunger level of player by 2.
  Returns a description of game state after the player wakes up.*/
  def rest(): String =
    if this.location == this.game.river then
      this.deathStatus = Some("You drown!")
      "You try to sleep in water."
    else
      this.hunger += 2
      this.game.advanceCycle()
      if !this.hasDied then
        "You rest for a while. It's " + this.game.time + " now. Better get moving."
      else
        "You try to sleep."

  def quit() =
    this.quitCommandGiven = true
    ""

  //Returns a brief description of the player’s state, for debugging purposes.
  override def toString = "Now at: " + this.location.name

  // Add item to inventory
  private def addtoInventory(item: Item) =
      val count = if this.possessions.contains(item.name) then this.possessions(item.name)(1) else 0
      this.possessions(item.name) = item -> (count + 1)

  /* Tries to pick up an item of the given name. This is successful if such an item can be picked and is
  located in the player’s current location. If so, the item is added to the player’s
  inventory. Returns a description of the result */
  def get(itemName: String): String =

    this.location.getItem(itemName) match
      case None =>
        "There is no " + itemName + " here to pick up."
      case Some(item) if !item.canPick =>
        "You can't pick up " + itemName + "."
      case Some(item) =>
        this.location.removeItem(item.name)
        addtoInventory(item)
        "You pick up " + itemName


  // Determines whether the player is carrying an item of the given name.
  def has(itemName: String) = this.possessions.contains(itemName)

  /* Tries to drop an item of the given name. This is successful if such an item is
  currently in the player’s possession.If so, the item is removed from the
  player’s inventory and placed in the area. If many of the same type of items are
  present in the inventory, only one of them is dropped.
  Returns a description of the result of the attempt: "You drop the ITEM." or "You don't have that!". */
  def drop(itemName: String) =
    this.possessions.get(itemName) match
      case None => "You don't have that item!"
      case Some((item, 1)) => this.possessions.remove(itemName); this.location.addItem(item); "You drop the " + itemName + "."
      case Some((item, higherCount)) => this.possessions(itemName) = (item, higherCount - 1); this.location.addItem(item); "You drop the " + itemName + "."

  // remove an item from the player inventory. Unlike the drop method, the item is not returned to the location but completely removed.
  def removeItem(itemName: String) =
    this.drop(itemName)
    this.location.removeItem(itemName)

  /* Causes the player to examine the item of the given name. This is successful if such
  an item is currently in the player’s possession. Returns a description of the result,
  which, if the attempt is successful, includes a description of the item. The description
  has the form: "You look closely at the ITEM.\nDESCRIPTION" or "If you want
  to examine something, you need to pick it up first." */
  def examine(itemName: String): String =
    def lookText(item: Item) = "You look closely at the " + item.name + ".\n" + item.description
    val failText = "If you want to examine something, you need to pick it up first."
    this.possessions.get(itemName).map( (item: Item, _: Int) => lookText(item) ).getOrElse(failText)

  /* Causes the player to list what they are carrying. Returns a listing of the player’s
  possessions or a statement indicating that the player is carrying nothing. The return
  value has the form "You are carrying:\nITEMS ON SEPARATE LINES" or "You are empty-handed."
  The items are listed in an arbitrary order. */
  def inventory =
    def formItemsDescription: Vector[String] =
      this.possessions.map( (name: String, items: (Item, Int) )  => name + "(x" + items(1) + ")" ).toVector
    if this.possessions.isEmpty then
      "You are empty-handed."
    else
      "You are carrying:\n" + formItemsDescription.mkString("\n")

  // Makes the player talk to the man.
  def talk(): String =
    if this.location == this.game.locationOfMan then
      if this.location == this.game.cave then
        this.currentLocation = this.game.backyard
      Man.speak(this)
    else
      "Nobody here to talk to."

  // Tke player tries to craft an item. If the player has all the starting materials needed, the task is successfull and the starting materials are consumed.
  def make(itemName: String) =
    craftables.get(itemName) match
      case Some(craftable) => craftable.craft(this)
      case None => "You can't craft that item!"

  // The player uses an item if it is present in the inventory. If the item used is an apple, player's hunger is reduced. If it is a tool,
  // it is used on other items in the player's location.
  def use(itemName: String): String =
    this.possessions.get(itemName) match
      case Some((item: Usable, _)) => item.use(this)
      case Some((item: Item, _)) => "There's a time and place for everything but not now!"
      case None => "You don't have " + itemName + " to use."

  // The player repairs a damaged item. This only works for tools in player's inventory which get damaged after three successful uses.
  def repairItem(itemName: String): String =
    this.possessions.get(itemName) match
      case Some((tool: Tool, _)) => tool.repair(this)
      case Some((item: Item, _))  => "This item can't be repaired."
      case None => "You don't have a " + itemName + " to repair."

end Player