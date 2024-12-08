#lang racket

; Helper function to read the grid from a file
(define (read-grid filename)
  (with-input-from-file filename
    (λ () 
      (let loop ([lines '()])
        (let ([line (read-line)])
          (if (eof-object? line)
              (reverse lines)
              (loop (cons (string->list line) lines))))))))

; Helper function to find all antenna positions
(define (find-antennas grid)
  (let ([antenna-hash (make-hash)])
    (for* ([r (range (length grid))]
           [c (range (length (first grid)))])
      (let ([ch (list-ref (list-ref grid r) c)])
        (when (not (char=? ch #\.))
          (hash-update! antenna-hash ch
                       (λ (lst) (cons (cons r c) lst))
                       '()))))
    antenna-hash))

; Helper function to calculate GCD
(define (my-gcd a b)
  (if (= b 0)
      (abs a)
      (my-gcd b (remainder a b))))

; Helper function to normalize vector
(define (normalize-vector dr dc)
  (if (and (= dr 0) (= dc 0))
      (cons 0 0)
      (let* ([g (my-gcd (abs dr) (abs dc))]
             [ndr (quotient dr g)]
             [ndc (quotient dc g)])
        (if (or (< ndr 0) (and (= ndr 0) (< ndc 0)))
            (cons (- ndr) (- ndc))
            (cons ndr ndc)))))

; Helper function to check if position is valid
(define (valid-position? r c height width)
  (and (<= 0 r (sub1 height))
       (<= 0 c (sub1 width))))

; Helper function to generate combinations of pairs
(define (combinations lst)
  (if (null? lst)
      '()
      (append
       (map (λ (x) (list (car lst) x))
            (cdr lst))
       (combinations (cdr lst)))))

; Part 1 solution
(define (solve-part1 grid antennas)
  (let* ([height (length grid)]
         [width (length (first grid))]
         [antinode-set (mutable-set)])
    
    (for* ([positions (hash-values antennas)]
           #:when (>= (length positions) 2)
           [pair (combinations positions)])
      (let* ([pos1 (car pair)]
             [pos2 (cadr pair)]
             [r1 (car pos1)]
             [c1 (cdr pos1)]
             [r2 (car pos2)]
             [c2 (cdr pos2)]
             [dr (- r2 r1)]
             [dc (- c2 c1)]
             [a1-r (- r1 dr)]
             [a1-c (- c1 dc)]
             [a2-r (+ r2 dr)]
             [a2-c (+ c2 dc)])
        
        (when (valid-position? a1-r a1-c height width)
          (set-add! antinode-set (cons a1-r a1-c)))
        (when (valid-position? a2-r a2-c height width)
          (set-add! antinode-set (cons a2-r a2-c)))))
    
    (set-count antinode-set)))

; Part 2 solution
(define (solve-part2 grid antennas)
  (let* ([height (length grid)]
         [width (length (first grid))]
         [antinode-set (mutable-set)])
    
    (for* ([positions (hash-values antennas)]
           #:when (>= (length positions) 2)
           [pair (combinations positions)])
      (let* ([pos1 (car pair)]
             [pos2 (cadr pair)]
             [r1 (car pos1)]
             [c1 (cdr pos1)]
             [r2 (car pos2)]
             [c2 (cdr pos2)]
             [dir (normalize-vector (- r2 r1) (- c2 c1))]
             [dr (car dir)]
             [dc (cdr dir)])
        
        ; Process both antennas
        (for ([start (list pos1 pos2)])
          ; Forward direction
          (let loop ([curr-r (car start)]
                    [curr-c (cdr start)])
            (let ([next-r (+ curr-r dr)]
                  [next-c (+ curr-c dc)])
              (when (valid-position? next-r next-c height width)
                (set-add! antinode-set (cons next-r next-c))
                (loop next-r next-c))))
          
          ; Backward direction
          (let loop ([curr-r (car start)]
                    [curr-c (cdr start)])
            (let ([next-r (- curr-r dr)]
                  [next-c (- curr-c dc)])
              (when (valid-position? next-r next-c height width)
                (set-add! antinode-set (cons next-r next-c))
                (loop next-r next-c)))))))
    
    (set-count antinode-set)))

; Main program
(define (main)
  (let* ([grid (read-grid "input.txt")]
         [antennas (find-antennas grid)]
         [part1-result (solve-part1 grid antennas)]
         [part2-result (solve-part2 grid antennas)])
    (printf "Part 1: ~a~n" part1-result)
    (printf "Part 2: ~a~n" part2-result)))

(main)