(import (scheme file))
(define input (open-input-file "day3.txt"))
(define strfst
  (lambda (l a)
    (if (= (car l) a)
        (cdr l)
        (strfst (cdr l) a))))
(define nomdig
  (lambda (line left acc)
    (if (= 0 left)
        acc
        (let ((dig (apply max (list-tail (reverse line) (- left 1)))))
          (nomdig (strfst line dig) (- left 1) (+ (* acc 10) dig))))))
(define solve (lambda (line) (nomdig line 12 0)))
(define step
  (lambda (n)
    (let ((line (read-line input)))
      (if (eof-object? line)
          n
          (step (+ n (solve (map digit-value (string->list line)))))))))
(print (step 0))
