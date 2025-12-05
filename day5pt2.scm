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
(define get-range
  (lambda (set elem)
    (if (eqv? set '())
        '()
        (if (and (>= elem (caar set)) (<= elem (cadar set)))
            (car set)
            (get-range (cdr set) elem)))))
(define has-matryoshka-range?
  (lambda (set elem)
    (if (eqv? set '())
        #f
        (or (and (>= (car elem) (caar set)) (<= (cadr elem) (cadar set)))
            (has-matryoshka-range? (cdr set) elem)))))
(define ranges (read-ranges '()))
(define presimplify-ranges
  (lambda (dec acc)
    (if (eqv? dec '())
        acc
        (if (or (has-matryoshka-range? (cdr dec) (car dec)) (has-matryoshka-range? acc (car dec)))
            (presimplify-ranges (cdr dec) acc)
            (presimplify-ranges (cdr dec) (cons (car dec) acc))))))
(define lmp
  (lambda (x acc)
    (let ((rg (get-range acc x)))
      (if (eqv? rg '())
          x
          (lmp (+ (cadr rg) 1) acc)))))
(define rmp
  (lambda (x acc)
    (let ((rg (get-range acc x)))
      (if (eqv? rg '())
          x
          (rmp (- (car rg) 1) acc)))))
(define simplify-ranges
  (lambda (dec acc)
    (if (eqv? dec '())
        acc

        (let ((newl (lmp (caar dec) acc))
              (newr (rmp (cadar dec) acc)))
          (if (< newr newl)
              (simplify-ranges (cdr dec) acc)
              (simplify-ranges (cdr dec) (cons (list newl newr) acc)))))))
(define solve
  (lambda (dec acc)
    (if (eqv? dec '())
        acc
        (solve (cdr dec) (+ (- (cadar dec) (caar dec)) acc 1)))))
(display (solve (simplify-ranges (presimplify-ranges ranges '()) '()) 0))
