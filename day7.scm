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
        (if (hash-table-exists? maptab (list (car cols) y))
            (step y (cdr cols) `(,(- (car cols) 1) ,(+ (car cols) 1) . ,acc) (+ acc2 1))
            (step y (cdr cols) (cons (car cols) acc) acc2)))))
(define contains?
  (lambda (set elem)
    (if (eqv? set '())
        #f
        (or (equal? elem (car set)) (contains? (cdr set) elem)))))
(define uniq
  (lambda (arr)
    (if (eqv? arr '())
        '()
        (if (contains? (cdr arr) (car arr))
            (uniq (cdr arr))
            (cons (car arr) (uniq (cdr arr)))))))
(define solve
  (lambda (y cols acc)
    (if (> y 200)
        acc
        (let ((stepped (step y cols '() 0)))
          (solve (+ 1 y) (uniq (car stepped)) (+ acc (cdr stepped)))))))
(display (solve 0 '(70) 0))
