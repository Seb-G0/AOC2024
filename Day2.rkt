#lang racket
(require racket/string)
(require rackunit)

; Read input file
(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (port->string))))

; Check for non-empty strings
(define (non-empty-string? s)
  (not (regexp-match? #rx"^\\s*$" s)))

; Convert input based on whether it's multiline or single line
(define (input->list input)
  (if (string-contains? input "\n")
      ; If multiline, split by newlines and remove empty strings
      (filter non-empty-string? (string-split input "\n"))
      ; If single line, split into individual characters
      (map string (string->list input))))

;input parsing specific to Day 2
(define (pr line)
  (map string->number (string-split line)))

(define (s? l)
  (let* ((diffs (map - (rest l) (drop-right l 1)))
         (ai? (andmap (lambda (d) (> d 0)) diffs))
         (ad? (andmap (lambda (d) (< d 0)) diffs))
         (adv? (andmap (lambda (d) (and (>= (abs d) 1) (<= (abs d) 3))) diffs)))
    (and (or ai? ad?) adv?)))

; Part 1 solution
(define (solve-part1 input)
  (let* ((inp (map pr input))
         (safe-reports (filter s? inp)))
    (length safe-reports)))

(define (damp? l)
  (or (s? l)
      (ormap (lambda (i) (s? (append (take l i) (drop l (add1 i))))) (build-list (length l) identity))))

; Part 2 solution
(define (solve-part2 input)
  (let* ((inp (map pr input))
         (safe-reports (filter damp? inp)))
    (length safe-reports)))


; Main execution
(define input-file "input.txt")
(define raw-input (read-input input-file))
(define input (input->list raw-input))

; Solve and display results
(printf "Part 1: ~a\n" (solve-part1 input))
(printf "Part 2: ~a\n" (solve-part2 input))


