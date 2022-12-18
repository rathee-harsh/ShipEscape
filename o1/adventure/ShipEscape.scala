package o1.adventure
import scala.collection.mutable.Map

class ShipEscape:

  val title = "Ship Escape"

  // The help text to be printed out for each command.
  val helpDescription: Map[String, String] = Map
    ("walk" -> "USAGE: 'walk DIRECTION'\nCauses the player to walk in the given direction if it exists.\nNOTE: This command increases hunger by 1 and advances time.",
      "swim" -> "USAGE: 'swim DIRECTION'\nCauses the player to swim in a given direction if it exists.\nNOTE: This command increases hunger by 3 and advances time.",
      "talk" -> "USAGE: 'talk'\nMakes the player to talk. In the current implementation, there is only only character the player cank talk to so no second arguemnts for this command are needed.",
      "rest" -> "USAGE: 'rest'\nThe player takes a long nap.\nNOTE: This command causes time to advance one full cycle (10 units) and hunger to increase by 2 units.",
      "quit" -> "USAGE: 'quit'\nIf you get frustrated and want to quit the game :(",
      "inventory" -> "USAGE: 'inventory'\nLists all the items currently in the player's inventory.",
      "get" -> "USAGE: 'get ITEM'\nMakes the player pickup a item if it is at player's location and can be picked.",
      "drop" -> "USAGE: 'drop ITEM'\nMakes the player drop a item from the inventory and adds the item to player's current location.",
      "examine" -> "USAGE: 'examine ITEM'\nThe player carefully looks at the item. (prints a short description of the item)",
      "use" -> "USAGE: 'use ITEM'\nThe player uses the item. The effect of this command depends on the item.\nFor an apple, it is consumed and player's hunger decreased.\nFor tools, this command is used to extract other items.\nNOTE: This command increases hunger by 1(only for tools) and advances time",
      "repair" -> "USAGE: 'repair TOOL'\nRepairs a tool.\nNOTE: Any tool can only be used three times before it required to be repaired.\nNOTE: This command increases hunger by 1 and advances time",
      "make" -> "USAGE: 'make ITEM'\nUsed for crafting items. In this version, a torch and a wall are the only two items that can be crafted.\nNOTE: This command increases hunger by 1 and advances time")

  // Define the areas (Refer Area.scala)
  val start = Area("Start", "You regain conciousness. Everything here seems unfamiliar.")
  val muddyRoad = Area("A muddy road", "")
  val house = Area("A house", "Nothing useful here.")
  val workshop = Area("Workshop", "", Some("A note in front of the door reads: Do not enter without permission.\n I should not go in."))
  val backyard = Area("Backyard", "It's full of flowers and plants.")
  val plateu = Area("Plateu", "You can see a swamp in the south.")
  val orchard = Area("Orchard", "An apple orchard. Use an axe to cut trees.")
  val cave = Area("Cave", "There is a man working. Maybe I should ask him for help.")
  val swamps = Area("Swamps", "It is really difficult to walk here.")
  val river = Area("River", "Luckily, the current is steady and you can easily swim thourgh it.")
  val island = Area("An Island", "You can see your ship in the east.")
  val ship = Area("Your Ship", "One corner of the ship is damaged. I need a wall to repair it.", Some("A big boulder has blocked the way. I must use a pickaxe and remove it."))

  // Link all the areas
  this.start.setNeighbor("south", this.muddyRoad)
  this.muddyRoad.setNeighbors(Vector( "east" -> this.plateu, "south" -> this.house ))
  this.plateu.setNeighbors(Vector("north"->this.cave, "east"->this.orchard, "west"->this.muddyRoad, "south"->this.swamps))
  this.cave.setNeighbor("south", this.plateu)
  this.orchard.setNeighbor("west", this.plateu)
  this.house.setNeighbors(Vector("north"->this.muddyRoad, "east"->this.workshop, "west"->this.backyard))
  this.workshop.setNeighbor("west", this.house)
  this.backyard.setNeighbor("east", this.house)
  this.swamps.setNeighbors(Vector("north"->this.plateu, "west"->this.river))
  this.river.setNeighbors(Vector("north"->this.island, "east"->this.swamps))
  this.island.setNeighbors(Vector("east"->this.ship, "south"->this.river))
  this.ship.setNeighbor("west", this.island)

  // Add items to the game. (Refer Item.scala)
  this.start.addItemCopies(Apple, 2)
  this.plateu.addItemCopies(UnminedCoal, 10)
  this.orchard.addItemCopies(Tree, 10)
  this.cave.addItem(Man)
  this.backyard.addItem(shovel)
  this.backyard.addItem(axe)
  this.backyard.addItem(pickaxe)
  this.island.addItem(Boulder)

  val player = Player(this, this.start)
  // This most recent holder stores the value of the current location of the man
  private var mansLocation = this.cave
  // Get the current location of the man
  def locationOfMan = this.mansLocation
  // Move the man to a new location.
  def updateLocationOfMan(newLocation: Area) =
    this.locationOfMan.removeItem("man")
    newLocation.addItem(Man)
    this.mansLocation = newLocation

  /*
  This variable represents the time of the Day. The time passes after each time-advancing command (.
  Takes values from -10 to 9 (both included). Negative value indicates that it's night time
  while a positive or a zero value indicates that its day.
  The value of this counter increases successively and when it reaches 9, its value is set to -10 and
  this loop continues.
  Every command except rest causes this value to increase by one unit. Rest causes its value to increase
  by 10. So for example, if the value of this counter is 7 then rest changes this value to -3.
  */
  var timeCounter: Int = 0

  // Returns a textual description of the current time.
  def time = if this.timeCounter >= 0 then "day" else "night"

  // Advances time by one unit.
  def advanceTime() =
    this.updateGameStatus()

    if this.timeCounter == 9 then
      this.timeCounter = -10
    else
      this.timeCounter += 1

  // Advance time by 10 units i.e shift from daytime to night and vice-versa (called by rest command)
  def advanceCycle() =
    (0 until 10).foreach( _ => this.advanceTime() )

  def welcomeMessage =
    """You are an explorer who sailed in-hope for discovering a new piece of land. Unfortunately, a big storm hit your ship. Having lost all hope,
    you just wait for your death. Just wishing that it's painless. By some miracle, you somehow survive but find yourself here in an unknown area.
    You now have to find your ship and get a map to return back home.
    """.stripMargin + this.getHelp("")

  def goodbyeMessage: String =
    if this.player.hasDied then
      this.player.causeOfDeath + "\n!!!GAME OVER!!!"
    else if this.player.hasQuit then
      "Ha-ha looser."
    else
      "Finally! Finally you repair the ship and set sail for home.\nYOU WIN!"

  def playTurn(command: String): String =
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    outcomeReport.getOrElse(s"""Unknown command: "$command".""")

  // return the help text to be printed.
  def getHelp(getHelpWith: String): String =
    val generalHelp = """
    GAME MECHANICS
    --------------
    The player can type certain commands to control the in-game character. The game has daytime and nighttime, both of which are 10 time-units long. One
    time unit passes when a player uses any time-advancing command. The following commands are time advancing: walk, swim, use, repair, make(all advance time
    by one unit), rest(advance by one cycle), and talk(advance by a cycle only in some situations).
    The character also has a hunger and a cold meter. The hunger level increases when the player uses any one of the following command: make(increase by 1),
    repair(increase by 1), use (increase by 1), walk(increse by 1), rest (incerase by 2), swim (increase by 3). If the player eats an apple, the hunger level
    gets back to zero. The cold level increases by 1.5 everytime a time-advancing command is used during the night(expcept when the player is inside the house,
    the workshop, or the ship OR when the player has a torch in their inventory). The cold level decreases by 1 with every time-advancing command
    used in the daytime. The player dies if the either the cold or the hunger level crosses 10. To win, you have to survive in the game and complete the objectives.
    """
    val commandHelp = """
    COMMANDS
    --------
    All the commands for the game are listed below.
    """ + this.helpDescription.keys.mkString(", ") + """
    To get a short description about the command, type 'help COMMAND' in the input prompt. To get this general help message again, just type 'help'."""
    if getHelpWith == "" then
      generalHelp + commandHelp
    else
      this.helpDescription.get(getHelpWith) match
        case Some(desciption) => desciption
        case None => "No such command found. The available commands are:\n" + this.helpDescription.keys.mkString(",")

  private def updateGameStatus(): Unit =
    if this.time == "night" && this.cave.inaccessible.isEmpty then
      this.cave.inaccessible = Some("You enter a cave but it's too dark to see anything. You exit out of it.")
    else if this.cave.inaccessible == Some("You enter a cave but it's too dark to see anything. You exit out of it.") then
      this.cave.inaccessible = None

    if this.time == "night" && this.player.location != this.house && this.player.location != this.workshop && this.player.location != this.ship && !this.player.has("torch") then
      this.player.increaseCold()
    else
      this.player.decreaseCold()

    if this.island.getItem("boulder").isEmpty then
      this.ship.inaccessible = None

    this.player.updateStatus()

  
  def isOver = this.player.hasDied || this.player.gameWon || this.player.hasQuit
  
end ShipEscape