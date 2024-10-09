#lang racket/base
(require racket/string)
(require racket/contract)

(define (make-row) (make-vector 3 #\-))
(define (valid-board-char char)
  (case char
    [(#\O #\X #\newline #\-) #t]
    [else #f]))

 (define/contract (make-board-from-string string)
       (->
        (λ (string)
          (and
           (eqv? (string-length string) 11)
           (foldl (λ (char acc) (and acc (valid-board-char char))) #t (string->list string))
           ))
        vector?)
     (let ([split (string-split string "\n")])
       (list->vector (map (λ (string) (list->vector (string->list string))) split))
       )
  )

(define (make-board)
  (make-board-from-string "---\n---\n---"))

(define (board->list board)
  (list
   (vector->list (vector-ref board 0))
   (vector->list (vector-ref board 1))
   (vector->list (vector-ref board 2))))

(define (board->string board)
  (letrec ([as-list (board->list board)]
           [as-string-list (map list->string as-list)])
    (string-join as-string-list "\n")))

(define (player-char player)
  (case player
    ['p1 #\X]
    ['p2 #\O]
    [else ""]))

(define/contract (make-movement! board player x y)
    (->
     vector?
     (λ (player) (or (eqv? 'p1 player) (eqv? 'p2 player)))
     (λ (x) (and (>= x 0) (<= x 2)))
     (λ (y) (and (>= y 0) (<= y 2)))
     any)
  (let ([player-char (player-char player)])
    (if
     (eqv? (vector-ref (vector-ref board x) y) #\-)
     (vector-set! (vector-ref board x) y player-char)
     (error "invalid movement"))
  ))

(define winning-coords '(
                 ((0 0) (0 1) (0 2))
                 ((0 0) (1 1) (2 2))
                 ((0 0) (1 0) (2 0))
                 ((0 1) (1 1) (2 1))
                 ((0 2) (1 2) (2 2))
                 ((1 0) (1 1) (1 2))
                 ((2 0) (2 1) (2 2))
                 ((2 0) (1 1) (0 2))
                 ))

(define/contract (analysis board)
    (->
     vector?
     (λ (result) (or (eqv? 'p1 result) (eqv? 'p2 result) (eqv? 'draw result)  (eqv? 'ongoing result))))
  (letrec (
           [values (map (λ (triplet) (values-from-triplet board triplet)) winning-coords)]
           [states (map (λ (triplet) (state triplet)) values)]
           )
    (cond
      [(ormap (λ (state) (eqv? 'p1 state)) states) 'p1]
      [(ormap (λ (state) (eqv? 'p2 state)) states) 'p2]
      [(ormap (λ (state) (eqv? 'ongoing state)) states) 'ongoing]
      [else 'draw])
    ))

(define (state triplet)
  (cond
    [(andmap (λ (char) (eqv? char #\O)) triplet) 'p2]
    [(andmap (λ (char) (eqv? char #\X)) triplet) 'p1]
    [(ormap (λ (char) (eqv? char #\-)) triplet) 'ongoing]
    [else 'draw]))

(define (values-from-triplet board triplet)
  (map (λ (pair) (value-from-pair board pair)) triplet))

(define (value-from-pair board pair)
  (let ([x (car pair)]
        [y (cadr pair)])
    (vector-ref (vector-ref board x) y)))

(define (read-board x y board)
  (vector-ref (vector-ref board x) y))

(define (read-board-pair pair board)
  (vector-ref (vector-ref board (car pair)) (cadr pair)))

(provide
 make-board
 make-board-from-string
 board->string
 make-movement!
 analysis
 read-board
 read-board-pair)
                       