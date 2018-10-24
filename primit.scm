(define map
  (lambda (op list)
    (if (null? list)
        '()
        (cons (op (car list))
              (map op (cdr list))))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))              

(define flatmap
  (lambda (f xs)
    (if (null? xs)
        '()
        (append (f (car xs))
                (flatmap f (cdr xs))))))              

(define filter
  (lambda (pred l)
    (cond ((null? l) '())
          ((pred (car l)) (cons (car l) (filter pred (cdr l))))
          (else
           (filter pred (cdr l))))))

(define range
  (lambda (a b)
    (if (> a b)
        '()
        (cons a (range (+ a 1) b)))))

(define length
    (lambda (l)
        (if (null? l)
            0
            (+ 1 (length (cdr l))))))

(define test
  (lambda (n xs gap)
    (if (null? xs)
        #t
        (and (not (= n (car xs)))
             (not (= n (+ (car xs) gap)))
             (not (= n (- (car xs) gap)))
             (test n (cdr xs) (+ gap 1))))))

(define test0
  (lambda (x xs)
    (test x xs 1)))

(define r8 (range 1 8))

(define queens
  (lambda (n)
    (if (= n 0)
        '(())
        (let ((pre (queens (- n 1))))
          (flatmap
           (lambda (x)
             (flatmap
              (lambda (xs)
                (map
                 (lambda (nobodycares)
                   (cons x xs))
                 (if (test0 x xs)
                     '(())
                     '())))
              pre))
           r8)))))