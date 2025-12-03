(import (scheme file))
(define input (open-input-file "day3.txt"))
(define strfst
  (lambda (l a)
    (if (= (car l) a)
        (cdr l)
        (strfst (cdr l) a))))
(define solve
  (lambda (line)
    (let* ((fid (apply max (cdr (reverse line))))
           (sid (apply max (strfst line fid))))
      (+ (* fid 10) sid))))
(define step
  (lambda (n)
    (let ((line (read-line input)))
      (if (eof-object? line)
          n
          (step (+ n (solve (map digit-value (string->list line)))))))))
(print (step 0))
