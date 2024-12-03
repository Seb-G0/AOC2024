#lang racket
(require racket/string)
(require rackunit)

; Read input file
(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (port->string))))

(define (exmul input)
  (regexp-match* #px"mul\\(([0-9]{1,3}),([0-9]{1,3})\\)" input #:match-select cdr))

(define (calc matches)
  (map (lambda (match)
         (* (string->number (first match))
            (string->number (second match))))
       matches))

(define (solve-part1 input)
  (define matches (exmul input))
  (define prods (calc matches))
  (apply + prods))

(define (solve-part2 input)
  (define instruction-regex
    #px"(mul\\((\\d{1,3}),(\\d{1,3})\\)|do\\(\\)|don't\\(\\))")
  (define matches
    (regexp-match* instruction-regex input))
  (define-values (total _)
    (for/fold ([sum 0] [enabled? #t])
              ([match (in-list matches)])
      (define instr (if (list? match) (first match) match))
      (cond
        [(string=? instr "do()") (values sum #t)]
        [(string=? instr "don't()") (values sum #f)]
        [(and enabled? (regexp-match #px"mul\\((\\d{1,3}),(\\d{1,3})\\)" instr))
         (let* ([m (regexp-match #px"mul\\((\\d{1,3}),(\\d{1,3})\\)" instr)]
                [x (string->number (list-ref m 1))]
                [y (string->number (list-ref m 2))])
           (values (+ sum (* x y)) enabled?))]
        [else (values sum enabled?)])))
  total)



; Main execution
(define input-file "input.txt")
(define raw-input (read-input input-file))

; Solve and display results
(define part1-result (solve-part1 raw-input))
(printf "Part 1: ~a\n" part1-result)
(define part2-result (solve-part2 raw-input))
(printf "Part 2: ~a\n" part2-result)

; Test cases
(module+ test
  (require rackunit)
  
  (define sample-input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  (define sample-input2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)do()?mul(8,5))")
  
  (test-case "Sample Input Test"
    (check-equal?
     (solve-part1 sample-input)
     161
     "The sum should be 161 (2*4 + 5*5 + 11*8 + 8*5)"))
  
  (test-case "Part 2 Sample Input Test"
    (check-equal?
     (solve-part2 sample-input2)
     48
     "The sum should be 48 (2*4 + 8*5)")))