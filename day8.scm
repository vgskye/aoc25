(import (scheme file)
        (scheme cxr)
        (srfi 69)
        (srfi 132))
(define input (open-input-file "day8.txt"))
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
      (if (eof-object? line)
          acc
          (read-ranges (cons (map string->number (string-split (lambda (c) (char=? c #\,)) line))
                             acc))))))
(define lamps (read-ranges '()))
(define pairs
  (letrec ((ipairs (lambda (val dec acc)
                     (if (eqv? dec '())
                         acc
                         (ipairs val (cdr dec) (cons (list val (car dec)) acc))))))
    (lambda (dec acc)
      (if (eqv? dec '())
          acc
          (pairs (cdr dec) (ipairs (car dec) (cdr dec) acc))))))
(define sorted-pairs
  (let ((dist (lambda (pair)
                (let ((dx (- (caar pair) (caadr pair)))
                      (dy (- (cadar pair) (cadadr pair)))
                      (dz (- (caddar pair) (cadr (cdadr pair)))))
                  (+ (* dx dx) (* dy dy) (* dz dz))))))
    (list-sort (lambda (a b) (< (dist a) (dist b))) (pairs lamps '()))))
(define list-head
  (lambda (list n)
    (if (= n 0)
        '()
        (cons (car list) (list-head (cdr list) (- n 1))))))
(define conns (list-head sorted-pairs 1000))
(define neighbours
  (lambda (pos dec acc)
    (if (eqv? dec '())
        acc
        (neighbours pos
                    (cdr dec)
                    (if (equal? (caar dec) pos)
                        (cons (cadar dec) acc)
                        (if (equal? (cadar dec) pos)
                            (cons (caar dec) acc)
                            acc))))))
(define visited (make-hash-table))
(define search-node
  (lambda (pos)
    (if (hash-table-exists? visited pos)
        0
        (begin
          (hash-table-set! visited pos #t)
          (+ 1 (apply + (map search-node (neighbours pos conns '()))))))))
(define nets
  (map (lambda (a)
         (display a)
         (display "\n")
         (search-node a))
       lamps))
(display (apply * (list-head (list-sort > nets) 3)))
