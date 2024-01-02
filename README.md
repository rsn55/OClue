# OClue
A text-based, command-line version of Clue, the classic mystery game. Includes all the elements of the board game as well as options to compete against bots of various levels of intelligence.

CREATED BY RACHEL NASH, MEREDITH DOBRZYNSKI, AND KAUSHIK RAVIKUMAR AT CORNELL UNIVERSITY

Instructions to Play:

Run `make play` from the top level of this repository. You must have OCaml installed ([instructions](https://ocaml.org/docs/installing-ocaml)).

From here, you can personalize the game by loading any JSON file that lists weapons, suspects, rooms, and secret passages. The file clue.json has the specifications of the classic board game and shows the general structure that must be used. You can just type 'clue.json' to use this one.

Then, you can select the difficulty level of the bots you want to play against. Level 'Easy' bots only eliminate cards shown to them directly. Level 'Medium' bots can eliminate cards by inference and observation of the moves of other players. Level 'Hard' bots make logical deductions based on every player's past and present moves. They also use probability to make more educated guesses and will try to hide their findings from other players.

In the beginning of the game, you are told the name of the suspect that represents you, the cards in your hand, and your current room. Cards, start rooms, and suspect representations are assigned randomly. The rules of OClue are the same as the classic board game: you can only make a suggestion about your current room, the suspect of a suggestion is moved to the room of the suggestion, you can sneak through secret passages, see the locations of other players, make an accusation once you know the answer, etc. 
However, since this game is text-based, you roll a die and either move to a new room or stay in your current location based on the outcome. This is unlike the board game, where you move the number of spaces that you roll.

You can type 'options' at any time to view various commands. On each turn, you have the option to roll, but you must make a suggestion. Then, you will see summaries of the moves of the other players before it is your turn again.

There is a detective notes feature which can be accessed by typing 'notes.' Cards you have seen directly are eliminated for you, and any other notes you may want to add can be included with 'write' and 'erase.'

If you win against the bots, you can add yourself to a leaderboard for that difficulty level and JSON board file.

If you are playing on level 'Easy,' you can quit and save your progress by typing 'save' and later loading a saved game instead of picking a difficulty level. Only one game can be saved at a time. Other levels do not have this functionality, so if you type 'quit,' the game will exit and your progress will be lost.

Have fun! If you have any questions, comments, or suggestions, please email rsn55@cornell.edu
