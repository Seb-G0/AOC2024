#lang racket
(require racket/string)
(require rackunit) ; Ensure rackunit is required for the tests

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

; Part 1 solution
(define (solve-part1 input)
  (let* ((grid input)
         (num-rows (length grid))
         (num-cols (string-length (car grid)))
         (directions '((0 1)
                       (1 0)
                       (1 1)
                       (1 -1)
                       (0 -1)
                       (-1 0)
                       (-1 -1)
                       (-1 1)))
         (word "XMAS")
         (word-length (string-length word)))
    (define (get-char ni nj)
      (string-ref (list-ref grid ni) nj))
    (define (extract-word i j dx dy)
      (let loop ((k 0) (chars '()))
        (if (= k word-length)
            (apply string (reverse chars))
            (let ((ni (+ i (* k dx)))
                  (nj (+ j (* k dy))))
              (if (and (>= ni 0) (< ni num-rows) (>= nj 0) (< nj num-cols)) (loop (+ k 1) (cons (get-char ni nj) chars))
                  "")))))
    (for*/sum ((i (in-range num-rows)) (j (in-range num-cols)) (dir directions))
      (let ((dx (first dir)) (dy (second dir)))
        (if (string-ci=? (extract-word i j dx dy) word)
            1
            0)))))

; Part 2 solution (if applicable)
(define (solve-part2 input)
  (let* ((grid input)
         (num-rows (length grid))
         (num-cols (string-length (car grid)))
         (count 0))
    (define (valid-diagonal? start end)
      (or (and (char-ci=? start #\M) (char-ci=? end #\S))
          (and (char-ci=? start #\S) (char-ci=? end #\M))))
    
    (for ([i (in-range 1 (- num-rows 1))])
      (for ([j (in-range 1 (- num-cols 1))])
        (when (char-ci=? (string-ref (list-ref grid i) j) #\A)
          (let ((nw (string-ref (list-ref grid (- i 1)) (- j 1)))
                (ne (string-ref (list-ref grid (- i 1)) (+ j 1)))
                (sw (string-ref (list-ref grid (+ i 1)) (- j 1)))
                (se (string-ref (list-ref grid (+ i 1)) (+ j 1))))
            (when (and (valid-diagonal? nw se)
                      (valid-diagonal? ne sw))
              (set! count (+ count 1)))))))
    count))


; Main execution
(define input-file "input.txt")
(define raw-input (read-input input-file))
(define input (input->list raw-input))

; Print processed data and its type
(printf "Input type is list?: ~a\n" (list? input))
(printf "Processed Data: ~a\n" input)

; Solve and display results
(printf "Part 1: ~a\n" (solve-part1 input))
(printf "Part 2: ~a\n" (solve-part2 input))

; Test cases
(module+ test
  (require rackunit)
  
  ; Test input processing
  (test-case "Single line conversion"
    (check-equal? 
     (input->list "ABCD")
     '("A" "B" "C" "D")
     "Single line should split into characters as strings"))
  
  (test-case "Multiline conversion"
    (check-equal? 
     (input->list "ABC\nDEF\nGHI")
     '("ABC" "DEF" "GHI")
     "Multiline should keep lines intact as strings"))
  
  ; Test puzzle solution for the sample input
  (test-case "Part 1 Sample Input"
    (check-equal? 
     (solve-part1 '("MMMSXXMASM"
                    "MSAMXMSMSA"
                    "AMXSXMAAMM"
                    "MSAMASMSMX"
                    "XMASAMXAMM"
                    "XXAMMXXAMA"
                    "SMSMSASXSS"
                    "SAXAMASAAA"
                    "MAMMMXMMMM"
                    "MXMXAXMASX"))
     18
     "Part 1 sample input test failed")))

(test-case "Part 2 Sample Input"
    (check-equal?
      (solve-part2 '("MMMSXXMASM"
                     "MSAMXMSMSA"
                     "AMXSXMAAMM"
                     "MSAMASMSMX"
                     "XMASAMXAMM"
                     "XXAMMXXAMA"
                     "SMSMSASXSS"
                     "SAXAMASAAA"
                     "MAMMMXMMMM"
                     "MXMXAXMASX"))
      9
      "Part 2 sample input test failed"))
