#lang racket

; Helper function to read file contents and split into lines
(define (read-file-lines filename)
  (with-input-from-file filename
    (lambda () 
      (let loop ([lines '()])
        (let ([line (read-line)])
          (if (eof-object? line)
              (reverse lines)
              (loop (cons (string->list line) lines))))))))

; Direction mappings
(define direction-order (vector 'U 'R 'D 'L))
(define directions 
  (hash 'U (cons -1 0)
        'R (cons 0 1)
        'D (cons 1 0)
        'L (cons 0 -1)))

; Find start position
(define (find-start contents)
  (let loop-rows ([r 0] [rows contents])
    (if (null? rows)
        (cons #f #f)
        (let loop-cols ([c 0] [chars (car rows)])
          (cond
            [(null? chars) (loop-rows (add1 r) (cdr rows))]
            [(char=? (car chars) #\^) (cons r c)]
            [else (loop-cols (add1 c) (cdr chars))])))))

; Check if position is within bounds
(define (in-bounds? row col max-row max-col)
  (and (<= 0 row) (< row max-row)
       (<= 0 col) (< col max-col)))

; Get character at position
(define (get-char-at contents row col)
  (list-ref (list-ref contents row) col))

; Initial path finding
(define (find-initial-path contents start-row start-col)
  (let ([max-row (length contents)]
        [max-col (length (car contents))]
        [visited (set (cons start-row start-col))])
    (let loop ([row start-row]
               [col start-col]
               [dir-idx 0])
      (let* ([dir (vector-ref direction-order dir-idx)]
             [dr (car (hash-ref directions dir))]
             [dc (cdr (hash-ref directions dir))]
             [front-r (+ row dr)]
             [front-c (+ col dc)])
        (if (not (in-bounds? front-r front-c max-row max-col))
            visited
            (if (char=? (get-char-at contents front-r front-c) #\#)
                (loop row col (modulo (add1 dir-idx) 4))
                (begin
                  (set! visited (set-add visited (cons front-r front-c)))
                  (loop front-r front-c dir-idx))))))))

; Check if position creates loop
(define (creates-loop? contents start-row start-col test-r test-c)
  (let ([max-row (length contents)]
        [max-col (length (car contents))]
        [curr-visited (set)]
        [curr-states (set)])
    (let loop ([row start-row]
               [col start-col]
               [dir-idx 0])
      (set! curr-visited (set-add curr-visited (cons row col)))
      (let ([state (list row col dir-idx)])
        (cond
          [(set-member? curr-states state) #t]
          [(> (set-count curr-states) (* max-row max-col 4)) #f]
          [else
           (set! curr-states (set-add curr-states state))
           (let* ([dir (vector-ref direction-order dir-idx)]
                  [dr (car (hash-ref directions dir))]
                  [dc (cdr (hash-ref directions dir))]
                  [front-r (+ row dr)]
                  [front-c (+ col dc)])
             (if (not (in-bounds? front-r front-c max-row max-col))
                 #f
                 (if (or (char=? (get-char-at contents front-r front-c) #\#)
                        (and (= front-r test-r) (= front-c test-c)))
                     (loop row col (modulo (add1 dir-idx) 4))
                     (loop front-r front-c dir-idx))))])))))

; Main program
(let* ([contents (read-file-lines "input.txt")]
       [start-pos (find-start contents)]
       [start-row (car start-pos)]
       [start-col (cdr start-pos)])
  (let* ([visited (find-initial-path contents start-row start-col)]
         [loop-positions 
          (for/fold ([positions (set)])
                    ([pos (set->list visited)]
                     #:when (not (and (= (car pos) start-row)
                                    (= (cdr pos) start-col))))
            (if (creates-loop? contents start-row start-col 
                             (car pos) (cdr pos))
                (set-add positions pos)
                positions))])
    (printf "~a\n" (set-count visited))
    (printf "~a\n" (set-count loop-positions))))