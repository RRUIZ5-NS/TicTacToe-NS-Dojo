# TicTacToe-NS-Dojo

To run the files it is necessary to download [DrRacket](https://download.racket-lang.org/)

The file `TicTacToe.rkt` contains the logic of the game.

`TicTacToe-test.rkt` holds the unit tests.

Lastly, `TicTacToeRunner.rkt` is the file to see the "bots" playing against one another.

If you want to play yourself instead of seeing the "bots", you can do so by inverting the comment line:

```racket
(define player1 read-player)
;(define player1 random-player)
```
