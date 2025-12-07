(import (scheme file)
        (srfi 69))
(define input (open-input-file "day7.txt"))
(define make-map-row
  (lambda (y x line acc)
    (if (eqv? line '())
        acc
        (make-map-row y
                      (+ 1 x)
                      (cdr line)
                      (if (char=? (car line) #\^)
                          (cons (list x y) acc)
                          acc)))))
(define make-map
  (lambda (y acc)
    (let ((line (read-line input)))
      (if (eof-object? line)
          acc
          (make-map (+ y 1) (make-map-row y 0 (string->list line) acc))))))
(define make-map-table
  (lambda (table list)
    (if (eqv? list '())
        '()
        (begin
          (hash-table-set! table (car list) #t)
          (make-map-table table (cdr list))))))
(define mappar (make-map 0 '()))
(define maptab (make-hash-table))
(make-map-table maptab mappar)
(define step
  (lambda (y cols acc acc2)
    (if (eqv? cols '())
        (cons acc acc2)
        (if (hash-table-exists? maptab (list (caar cols) y))
            (step y
                  (cdr cols)
                  `((,(- (caar cols) 1) . ,(cdar cols)) (,(+ (caar cols) 1) . ,(cdar cols)) . ,acc)
                  (+ acc2 (cdar cols)))
            (step y (cdr cols) (cons (car cols) acc) acc2)))))
(define contains?
  (lambda (set elem)
    (if (eqv? set '())
        #f
        (or (equal? (car elem) (caar set)) (contains? (cdr set) elem)))))
(define addset
  (lambda (set elem addition)
    (if (eqv? set '())
        '()
        (if (equal? elem (caar set))
            (cons (cons (caar set) (+ addition (cdar set))) (cdr set))
            (cons (car set) (addset (cdr set) elem addition))))))
(define uniq
  (lambda (arr)
    (if (eqv? arr '())
        '()
        (if (contains? (cdr arr) (car arr))
            (addset (uniq (cdr arr)) (caar arr) (cdar arr))
            (cons (car arr) (uniq (cdr arr)))))))
(define solve
  (lambda (y cols acc)
    (if (> y 200)
        acc
        (let ((stepped (step y cols '() 0)))
          (solve (+ 1 y) (uniq (car stepped)) (+ acc (cdr stepped)))))))
(display (solve 0 '((70 . 1)) 1))
