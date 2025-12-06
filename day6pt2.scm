(import (scheme file)
        (scheme cxr))
(define input (open-input-file "day6.txt"))
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
(define filter-blank
  (lambda (dec)
    (if (eqv? dec '())
        '()
        (if (string=? (car dec) "")
            (filter-blank (cdr dec))
            (cons (car dec) (filter-blank (cdr dec)))))))
(define make-map
  (lambda (acc)
    (let ((line (read-line input)))
      (if (eof-object? line)
          acc
          (make-map (cons (string->list line) acc))))))
(define any
  (lambda (pred list)
    (if (eqv? list '())
        #f
        (or (pred (car list)) (any pred (cdr list))))))
(define zip
  (lambda a
    (if (any (lambda (a) (eqv? a '())) a)
        '()
        (cons (map car a) (apply zip (map cdr a))))))
(define trim-ws
  (lambda (a)
    (if (eqv? a '())
        '()
        (if (char=? (car a) #\ )
            (trim-ws (cdr a))
            (cons (car a) (trim-ws (cdr a)))))))
(define list-split
  (lambda (pred? list)
    (if (eqv? list '())
        '(())
        (if (pred? (car list))
            (cons '() (list-split pred? (cdr list)))
            (let ((rest (list-split pred? (cdr list))))
              (cons (cons (car list) (car rest)) (cdr rest)))))))
(define solve
  (lambda (entries)
    (let ((operator (caar entries))
          (numbers (map string->number
                        (map list->string (map trim-ws (map reverse (map cdr entries)))))))
      (if (char=? operator #\+)
          (apply + numbers)
          (apply * numbers)))))
(display
 (apply + (map solve (list-split (lambda (a) (eqv? '() (trim-ws a))) (apply zip (make-map '()))))))
