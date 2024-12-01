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

; Parse a line into a pair of numbers
(define (parse-line line)
  (let* ((tokens (string-split (string-trim line)))
         (nums (map string->number tokens)))
    (if (= (length nums) 2)
        (cons (first nums) (second nums))
        (error "Invalid line format" line))))

; Part 1 solution
(define (solve-part1 input)
  (let* ((pairs (map parse-line input))
         (left-list (map car pairs))
         (right-list (map cdr pairs))
         (sorted-left (sort left-list <))
         (sorted-right (sort right-list <))
         (differences (map (lambda (l r) (abs (- l r))) sorted-left sorted-right)))
    (apply + differences)))

; Part 2 solution
(define (build-counts lst)
  (define counts (make-hash))
  (for ([n (in-list lst)])
    (hash-update! counts n add1 0))
  counts)

(define (solve-part2 input)
  (let* ((pairs (map parse-line input))
         (left-list (map car pairs))
         (right-list (map cdr pairs))
         (right-counts (build-counts right-list)))
    (for/sum ([n (in-list left-list)])
      (* n (hash-ref right-counts n 0)))))

; Main execution
(define input-file "input.txt")
(define raw-input (read-input input-file))
(define input (input->list raw-input))

; Solve and display results
(printf "Part 1: ~a\n" (solve-part1 input))
(printf "Part 2: ~a\n" (solve-part2 input))

; Test cases
(module+ test
  (require rackunit)
  
  ; Test input processing
  (test-case "Multiline conversion"
    (check-equal? 
     (input->list "ABC\nDEF\nGHI")
     '("ABC" "DEF" "GHI")
     "Multiline should keep lines intact as strings"))
  
  ; Test parse-line function
  (test-case "Parse line"
    (check-equal?
     (parse-line "3   4")
     (cons 3 4)
     "Parsing '3   4' should return (3 . 4)"))
  
  ; Test solve-part1 with sample input
  (test-case "Sample Input Part 1"
    (define sample-input
      (list "3   4"
            "4   3"
            "2   5"
            "1   3"
            "3   9"
            "3   3"))
    (check-equal?
     (solve-part1 sample-input)
     11
     "Total distance for sample input should be 11"))
  
  ; Test solve-part2 with sample input
  (test-case "Sample Input Part 2"
    (define sample-input
      (list "3   4"
            "4   3"
            "2   5"
            "1   3"
            "3   9"
            "3   3"))
    (check-equal?
     (solve-part2 sample-input)
     31
     "Similarity score for sample input should be 31")))
