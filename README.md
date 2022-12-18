# Ship Escape

## Walkthrough

To win, you need to get a map and find your ship.  You also need a wall to repair the ship. To get the map, you need to find a man in 
a cave who asks you to bring him a torch, and he would give you the map in-return.
Entering the following commands.

First pick up the two apples in the start area.
>> get apple
>> get apple
Now walk get to the man in the cave.
>> walk south
>> walk east
>> walk north
Now talk to the man
>> talk
After you talk, he takes you to his garden and asks you to bring him a torch to his workshop. You should also get a torch for yourself so that you
don't feel cold in the nighttime. Pickup the tools from the yard.
>> get axe
>> get shovel
>> get pickaxe
Before going to make torches, go inside the house and rest so that the night passes and you don't feel cold.
>> walk east
>> rest
Now eat an apple so that you don't starve.
>> use apple
Now go to the plateu, and use a shovel there to get coal and then to the orchard to get a stick.
>> walk north
>> walk east
>> use shovel
>> use shovel
>> use shovel
Now the shovel is damaged and to use it again, you need to repair it. To do that, you would need a stick. Pickup the coal and go to the orchard on the east.
>> get coal
>> get coal
>> get coal
>> walk east
You must be hungry again by now, eat one more apple
>> use apple
Now use the axe 3 times.
>> use axe
>> use axe
>> use axe
 Collect some apples, 3 sticks (oen for each torch and one for repairing the shovel). 
Also make sure to get two pieces of log. You'll need them later to make a wall.
>> get stick
>> get stick
>> get stick
>> get apple
>> get apple
>> get log
>> get log
Now make one torch so that you don't feel cold and then repair the shovel
>> make torch
>> repair shovel
Now head west and use shovel for one last time.
>> walk west
>> use shovel
Now pickup the coal and make the second torch
>> get coal
>> make torch
Go back to the man's workshop now.
>> walk west
>> use apple
>> walk south
>> walk east
Now talk to him, and he'll give you the sea map.
>> talk
After this is done, you now need to get to your ship.
>> walk west
>> walk north
>> walk east
>> walk south
>> walk west
Make sure to eat an apple before swimming!
>> use apple
>> swim north
Now you'll reach an island where your ship is. To get to it, you need to remove a boulder in the way.
>> use pickaxe
Pickup 3 stones that you get from breaking the boulder.
>> get stone
>> get stone
>> get stone
Now make a wall to repair the ship.
>> make wall
>> Now enter the ship and you'll win!
>> walk east
NOTE: Some commands like rest, drop etc. are not used here (since the game can be won without using them if the player plays perfectly.)
Rest makes you fall asleep and causes the time to change.
Older commands - drop, examine, and inventory from the orignal implementation still work as expected.

## NOTE
When using the GUI version of the game, please maximize the window so that no text is left out of the screen. In the GUI version,
the player status and the time are in the bottom left corner of the screen

## CREDITS
The game is built on top of the Adventure module (defining the UI part and some starting code for the internal model of the game) in the O1 programming course at Aalto University which is available at: https://gitmanager.cs.aalto.fi/static/O1_2022/modules/given/Adventure/
