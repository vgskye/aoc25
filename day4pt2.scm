(import (scheme file)
        (srfi 69))
(define input (open-input-file "day4.txt"))
(define make-map-row
  (lambda (y x line acc)
    (if (eqv? line '())
        acc
        (make-map-row y
                      (+ 1 x)
                      (cdr line)
                      (if (char=? (car line) #\@)
                          (cons `(,x ,y) acc)
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
(define neighbours
  (lambda (x y)
    `((,(- x 1) ,(- y 1)) (,(- x 1) ,y)
                          (,(- x 1) ,(+ y 1))
                          (,x ,(- y 1))
                          (,x ,(+ y 1))
                          (,(+ x 1) ,(- y 1))
                          (,(+ x 1) ,y)
                          (,(+ x 1) ,(+ y 1)))))
(define countneighbours
  (lambda (set pos)
    (apply + (map (lambda (a) (if (hash-table-exists? set a) 1 0)) (apply neighbours pos)))))
(define filter
  (lambda (table pred keys)
    (if (eqv? keys '())
        '()
        (begin
          (if (pred (car keys))
              (hash-table-delete! table (car keys)))
          (filter table pred (cdr keys))))))
(define mappar (make-map 0 '()))
(define maptab (make-hash-table))
(make-map-table maptab mappar)
(define solve
  (lambda (cur init)
    (let ((cursize (hash-table-size cur)))
      (filter cur (lambda (pos) (< (countneighbours cur pos) 4)) (hash-table-keys cur))
      (if (= (hash-table-size cur) cursize)
          (- init cursize)
          (solve cur init)))))
(display (solve maptab (hash-table-size maptab)))
