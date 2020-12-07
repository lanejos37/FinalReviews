The plot of the game is that you wandered into a witch’s house deep in the forest during a hunting escapade. 
Your goal is to escape before she comes back
Once you start, run:
 (go (the door))
It will take you to the first room
Here you're supposed to check for a key to the third room.Once you enter a door, it's locked and you cant go 
through it again.
In the second room, you have three physical items where the key may be hidden: a rocking chair,a cabinet and a gramophone.
However a bomb is also hidden in one of those places. If you check in the wrong place, you might cause the bomb
to defuse hence ending the game so choose wisely
To check for the key in a place
 Run:
 (check-under (the key) (the --place you want to search ie gramophone, cabinet or rocking-chair--))
 Also the three items have their own methods
To sit on the chair run:
 (sit-down (the rocking-chair))
To lean back run:
(lean-back (the rocking-chair))
To lean up run:
(lean-up (the rocking-chair))
To stand-up run:
(stand-up (the rocking-chair))
For the cabinet
To open it run:
 (open (the cabinet))

To close it run:
(close (the cabinet))
To take food inside run:
(take (within (the cabinet) "The food you want. It's displayed once you open the cabinet")

For the gramophone
To switch it on run:
(switch-on (the gramophone))
To switch it off run:
(switch-off (the gramophone))
To eat the food Run:
(eat (the --food you've taken--))

There is also a gun in the second room which you can take and arm yourself against the witch if she comes back
To pick it run:
(take (the gun))
To load it:
(load (the gun))

Once you have gotten the key
You can now proceed to the last room
Run:
(go (the white and promising door))
Here you'll find a horse you can use to escape to freedom
Once there run :
(ride (the horse))
Happy playing
