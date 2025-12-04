(import (scheme file))
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
(define contains?
  (lambda (set elem)
    (if (eqv? set '())
        #f
        (or (equal? elem (car set)) (contains? (cdr set) elem)))))
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
  (lambda (set pos) (apply + (map (lambda (a) (if (contains? set a) 1 0)) (apply neighbours pos)))))
(define solve
  (lambda (set cur acc)
    (if (eqv? cur '())
        acc
        (solve set
               (cdr cur)
               (if (< (countneighbours set (car cur)) 4)
                   (+ 1 acc)
                   acc)))))
(define mappar (make-map 0 '()))
(display (solve mappar mappar 0))
