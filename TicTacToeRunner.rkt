#lang racket/base
(require racket/list)
(require "TicTacToe.rkt")

(define all-coords
  '((0 0) (0 1) (0 2)
    (1 0) (1 1) (1 2)
    (2 0) (2 1) (2 2)))

(define (read-player board player)
  (let ([x (read)]
        [y (read)])
    (make-movement! board player x y)))

(define (random-player board player)
  (letrec ([actual-coord (map (λ (coord) (list (read-board-pair coord board) coord)) all-coords)]
           [available (filter (λ (pair) (eqv? (car pair) #\-)) actual-coord)]
           [selected (car (shuffle available))]
           [movement-coord (cadr selected)]
           )
    (make-movement! board player (car movement-coord) (cadr movement-coord))
    ))

;(define player1 read-player)
(define player1 random-player)
(define player2 random-player)

(define (next-player current-player)
  (case current-player
    ['p1 'p2]
    ['p2 'p1]))

(define (simulate current-player board)
  (begin
    (cond
      [(eqv? current-player 'p1) (player1 board 'p1)]
      [(eqv? current-player 'p2) (player2 board 'p2)]
      )
    (displayln (board->string board))
    (case (analysis board)
      ['p1 (displayln "Player 1 has won!")]
      ['p2 (displayln "Player 2 has won!")]
      ['draw (displayln "Game ended in a draw")]
      ['ongoing (displayln "Game is still ongoing")])
    (newline)
    (case (analysis board)
      [('p1 'p2 'draw) #t]
      ['ongoing (simulate (next-player current-player) board)])
    ))

(define board (make-board))
(simulate 'p1 board)