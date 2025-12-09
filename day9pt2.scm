(import (scheme file)
        (scheme cxr))
(define input (open-input-file "day9.txt"))
(define (string-split char-delimiter? string)
  (define (maybe-add a b parts)
    (if (= a b)
        parts
        (cons (substring string a b) parts)))
  (let ([n (string-length string)])
    (let loop ([a 0]
               [b 0]
               [parts '()])
      (if (< b n)
          (if (not (char-delimiter? (string-ref string b)))
              (loop a (+ b 1) parts)
              (loop (+ b 1) (+ b 1) (maybe-add a b parts)))
          (reverse (maybe-add a b parts))))))
(define read-ranges
  (lambda (acc)
    (let ([line (read-line input)])
      (if (eof-object? line)
          acc
          (read-ranges (cons (map string->number (string-split (lambda (c) (char=? c #\,)) line))
                             acc))))))
(define tiles (read-ranges '()))
(define lines (map list (append (list-tail tiles 1) (list (car tiles))) tiles))
; (define vlines
;   (letrec ([vfilt (lambda (dec acc)
;                     (if (eqv? dec '())
;                         acc
;                         (vfilt (cdr dec)
;                                (if (= (caaar dec) (caadar dec))
;                                    (cons (car dec) acc)
;                                    acc))))])
;     (vfilt lines '())))
; (define isleft
;   (lambda (x y dec acc)
;     (if (eqv? dec '())
;         acc
;         (isleft x
;                 y
;                 (cdr dec)
;                 (cons (and (>= x (caaar dec))
;                            (>= y (min (cadaar dec) (cadar (cdar dec))))
;                            (< y (max (cadaar dec) (cadar (cdar dec)))))
;                       acc)))))
(define windings (lambda (x y dec acc) ))
(define minx (apply min (map car tiles)))
(define maxx (apply max (map car tiles)))
(define miny (apply min (map cadr tiles)))
(define maxy (apply max (map cadr tiles)))
(define (range first last)
  (if (> first last)
      '()
      (cons first (range (+ first 1) last))))
(define pairs
  (letrec ([ipairs (lambda (val dec acc)
                     (if (eqv? dec '())
                         acc
                         (ipairs val (cdr dec) (cons (list val (car dec)) acc))))])
    (lambda (dec acc)
      (if (eqv? dec '())
          acc
          (pairs (cdr dec) (ipairs (car dec) (cdr dec) acc))))))
(define dist
  (lambda (pair)
    (let ([dx (- (caar pair) (caadr pair))]
          [dy (- (cadar pair) (cadadr pair))])
      (* (+ 1 (abs dx)) (+ 1 (abs dy))))))
(define sorted-pairs (list-sort (lambda (a b) (> (dist a) (dist b))) (pairs tiles '())))
(define xor
  (lambda a
    (if (= (length a) 1)
        (car a)
        (apply xor (cons (not (eqv? (car a) (cadr a))) (cddr a))))))
(define all
  (lambda (pred? list)
    (if (eqv? list '())
        #t
        (if (pred? (car list)) (all pred? (cdr list)) #f))))
(define incl (lambda (x y) (apply xor (isleft x y vlines '()))))
(define pair-works?
  (lambda (pair)
    (let ([xmin (apply min (map car pair))]
          [xmax (apply max (map car pair))]
          [ymin (apply min (map cadr pair))]
          [ymax (apply max (map cadr pair))])
      (and (all (lambda (x) (and (incl x ymin) (incl x ymax))) (range xmin xmax))
           (all (lambda (y) (and (incl xmin y) (incl xmax y))) (range ymin ymax))))))
(define any
  (lambda (pred? list)
      (display (car list))
      (display "\n")
    (if (eqv? list '())
        '()
        (if (pred? (car list))
            (car list)
            (any pred? (cdr list))))))
(display (any pair-works? sorted-pairs))
