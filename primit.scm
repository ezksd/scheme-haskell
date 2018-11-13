(define (id x) x)

(define (map op xs)
  (foldr (lambda (a b)
           (cons (op a) b))
         '()
         xs))
              
(define (append l1 l2)
  (foldr (lambda (a b)
                 (cons a b))
         l2
         l1))  

(define (flatmap f xs1)
  (foldr (lambda (a b)
           (append (f a) b))
         '()
         xs1))

(define (filter p xs)
  (foldr (lambda (a b)
           (if (p a)
               (cons a b)
               b))
         xs))

(define (length xs)
  (foldr (lambda (a b)
           (+ b 1))
         0
         xs))

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

(define (queens pre)
  (flatmap (lambda (x1)
             (flatmap (lambda (xs)
                        (if (test0 x1 xs)
                            (cons (cons x1 xs) '())
                            '()))
                      pre))
           r8))

(define q8
  (foldr (lambda (a b)
           (queens b))
         '(())
         r8))