(import (scheme file)
        (scheme cxr))
(define input (open-input-file "day5.txt"))
(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b)
        parts
        (cons (substring string a b) parts)))
  (let ((n (string-length string)))
    (let loop ((a 0)
               (b 0)
               (parts '()))
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))
(define read-ranges
  (lambda (acc)
    (let ((line (read-line input)))
      (if (string=? line "")
          acc
          (read-ranges (cons (map string->number (string-split (lambda (c) (char=? c #\-)) line))
                             acc))))))
(define contains?
  (lambda (set elem)
    (if (eqv? set '())
        #f
        (or (and (>= elem (caar set)) (<= elem (cadar set))) (contains? (cdr set) elem)))))
(define ranges (read-ranges '()))
(define solve
  (lambda (acc)
    (let ((line (read-line input)))
      (if (eof-object? line)
          acc
          (solve (if (contains? ranges (string->number line))
                     (+ 1 acc)
                     acc))))))
(display (solve 0))
