#lang racket
(require racket/string)
(require rackunit)

;; Read input file
(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (port->string))))

(define (take-while pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst))
     (cons (car lst) (take-while pred (cdr lst)))]
    [else '()]))

(define (drop-while pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst))
     (drop-while pred (cdr lst))]
    [else lst]))

;; Check for non-empty strings
(define (non-empty-string? s)
  (not (regexp-match? #rx"^\\s*$" s)))

;; Parse the input into rules and updates
(define (parse-input input)
  (let* ([lines (string-split input "\n")]
         [non-empty-lines (filter non-empty-string? lines)]
         [rules-section (take-while (lambda (line) (regexp-match? #rx"\\|" line)) non-empty-lines)]
         [updates-section (drop-while (lambda (line) (regexp-match? #rx"\\|" line)) non-empty-lines)]
         [rules (map parse-rule rules-section)]
         [updates (map parse-update updates-section)])
    (values rules updates)))

;; Parse a single rule line into a pair of numbers (X . Y)
(define (parse-rule line)
  (let* ([parts (string-split line "|")]
         [x (string->number (string-trim (first parts)))]
         [y (string->number (string-trim (second parts)))])
    (cons x y)))

;; Parse a single update line into a list of numbers
(define (parse-update line)
  (map string->number
       (map string-trim
            (string-split line ","))))

;; Find index of item in a list
(define (index-of lst item)
  (let loop ([lst lst] [idx 0])
    (cond
      [(null? lst) -1]
      [(equal? (car lst) item) idx]
      [else (loop (cdr lst) (+ idx 1))])))

;; Check if an update is correctly ordered according to the rules
(define (update-correctly-ordered? update rules)
  (let loop ([rules rules])
    (cond
      [(null? rules) #t]
      [else
       (let* ([rule (car rules)]
              [x (car rule)]
              [y (cdr rule)])
         (if (and (member x update)
                  (member y update))
             (let ([index-x (index-of update x)]
                   [index-y (index-of update y)])
               (if (< index-x index-y)
                   (loop (cdr rules))
                   #f))
             (loop (cdr rules))))])))

;; Get predecessors for a number based on rules
(define (get-p num rules numbers)
  (filter
   (lambda (x) (member x numbers))
   (filter-map
    (lambda (rule)
      (if (= (cdr rule) num)
          (car rule)  ; num depends on (car rule)
          #f))
    rules)))

(define (get-degree num rules numbers)
  (length (get-p num rules numbers)))

(define (sort-by-dependencies numbers rules)
  (define result '())
  (define remaining numbers)
  
  (let loop ()
    (if (null? remaining)
        (reverse result)
        (let* ([degrees (map (lambda (n) (cons n (get-degree n rules remaining))) remaining)]
               [min-degree (apply min (map cdr degrees))]
               [candidates (filter (lambda (p) (= (cdr p) min-degree)) degrees)]
               [next (car (sort (map car candidates) >))])
          (set! result (cons next result))
          (set! remaining (remove next remaining))
          (loop)))))

(define (middle-page-number update)
  (list-ref update (quotient (length update) 2)))

(define (process-updates-part1 updates rules)
  (define total 0)
  (for-each
   (lambda (update)
     (when (update-correctly-ordered? update rules)
       (set! total (+ total (middle-page-number update)))))
   updates)
  total)

(define (process-updates-part2 updates rules)
  (define total 0)
  (for-each
   (lambda (update)
     (when (not (update-correctly-ordered? update rules))
       (define sorted-update (sort-by-dependencies update rules))
       (set! total (+ total (middle-page-number sorted-update)))))
   updates)
  total)


(let* ([input-content (read-input "input.txt")])
  (define-values (rules updates) (parse-input input-content))
  (printf "Part 1 - Sum of middle numbers from correct updates: ~a\n" 
          (process-updates-part1 updates rules))
  (printf "Part 2 - Sum of middle numbers from reordered incorrect updates: ~a\n" 
          (process-updates-part2 updates rules)))