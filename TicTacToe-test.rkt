#lang racket/base
 
(require rackunit "TicTacToe.rkt")

(define TicTacToe-tests
  (test-suite
   "Tests for TicTacToe.rkt"

   (test-case
    "A board can be made from a string"
    (letrec ([string "XOX\nXOO\nOXX"]
             [board (make-board-from-string string)]
             [string-board (board->string board)])
      (check-equal? string-board string)))

   (test-case
    "A new empty board can be printed"
    (letrec ([board (make-board)]
             [string-board (board->string board)])
      (check-equal? string-board "---\n---\n---")))

   (test-case
    "A movement on a new board can be made"
    (let ([board (make-board)])
      (begin
        (make-movement! board 'p1 0 0)
        (let ([string-board (board->string board)])
          (check-equal? string-board "X--\n---\n---")))))

   (test-case
    "An invalid movement raises an error"
    (let ([board (make-board)])
      (begin
        (make-movement! board 'p1 0 0)
        (check-exn exn:fail? (λ () (make-movement! board 'p2 0 0))))))

   (test-case
    "Game won by p2"
    (letrec ([board (make-board-from-string "XXO\n-OX\nOOX")]
             [result (analysis board)])
      (check-equal? result 'p2)))

   (test-case
    "Game won by p1"
    (letrec ([board (make-board-from-string "XXX\n-OX\nOOX")]
             [result (analysis board)])
      (check-equal? result 'p1)))

   (test-case
    "Game draw"
    (letrec ([board (make-board-from-string "XXO\nOOX\nXOX")]
             [result (analysis board)])
      (check-equal? result 'draw)))

   (test-case
    "Game ongoing"
    (letrec ([board (make-board-from-string "XXO\nO-X\nXOX")]
             [result (analysis board)])
      (check-equal? result 'ongoing)))

   (test-case
    "Read board"
    (letrec ([board (make-board-from-string "XXO\nO-X\nXOX")]
             [result (read-board 2 0 board)])
      (check-equal? result #\X)))

   (test-case
    "Read board pair"
    (letrec ([board (make-board-from-string "XXO\nO-X\nXOX")]
             [result (read-board-pair '(2 0) board)])
      (check-equal? result #\X)))

  ))

(require rackunit/text-ui)
 
(run-tests TicTacToe-tests)