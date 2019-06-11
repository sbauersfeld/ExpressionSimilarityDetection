#lang racket

; the special lambda symbol
(define lam-sym (string->symbol "\u03BB"))

; x is an atom if its not a pair (or a list)
(define (atom? x)
  (not (pair? x)))

; this function returns true if either x or y is quoted
(define (is-quoted? x y)
    (or (and (pair? x) (equal? 'quote (car x)))
        (and (pair? y) (equal? 'quote (car y)))))

; this function takes a list of arguments and the old list of bindings
; it prepends the new bindings to the old list of bindings
(define (new-bind args diff-lst)
    (cond ((null? args) diff-lst)
          ((atom? args) (cons (list args args) diff-lst))
          (#t (cons (list (car args) (car args)) (new-bind (cdr args) diff-lst)))))

; this helper function produces the new symbols x!y when the lambda arugments differ and appends the updated
; binding list to the front of the previous binding list
(define (diff-lst-helper x y op op2 res)
    (if (equal? (op x) (op y))
        (list (op2 (op x) (car res)) (cons (list (op x) (op x)) (cadr res)) (cons (list (op y) (op y)) (caddr res)))
        (let ([sym (string->symbol (string-append (symbol->string (op x)) "!" 
            (symbol->string (op y))))])
            (list (op2 sym (car res)) (cons (list (op x) sym) (cadr res)) (cons (list (op y) sym) (caddr res))))))

; a remove function used specifically for removing shadowed bindings in the argument list
(define (my-remove target lst)
    (cond ((null? lst) '())
          ((equal? target (caar lst)) (my-remove target (cdr lst)))
          (#t (cons (car lst) (my-remove target (cdr lst))))))

; this function returns the new binding lists for the x and y expressions along with the new symbol list
(define (gen-diff-lst x y dx dy)
    (cond ((and (null? x) (null? y)) (list '() dx dy))
          ((and (atom? x) (atom? y)) (diff-lst-helper x y (lambda (x) x) (lambda (x y) x) (list '() dx dy)))
          (#t (let ([new-dx (my-remove (car x) dx)]
                    [new-dy (my-remove (car y) dy)])
                (let ([res (gen-diff-lst (cdr x) (cdr y) new-dx new-dy)])
                    (diff-lst-helper x y car cons res))))))

; this function returns true if either list is improper
(define (varg? x y)
    (or (and (pair? x) (not (list? x)))
        (and (pair? y) (not (list? y)))))

; this function returns true if both x and y are lists or if both x and y are not lists
(define (varg-eq? x y) 
    (or (and (list? x) (list? y))
        (and (not (list? x)) (not (list? y)))))

; this function returns true if both x and y are atoms, 
; or if the length of the proper or improper list of x and y is equal
(define (varg-eqv? x y)
    (cond ((and (atom? x) (atom? y)) #t)
          ((or (atom? x) (atom? y)) #f)
          (#t (varg-eqv? (cdr x) (cdr y)))))

; this function handles lambda functions by creating a new binding list for the arguments 
; and continuing execution of the program with the new bindings applied
(define (handle-lambda res x y dx dy orig-x orig-y)
    (if (and (varg-eq? (car x) (car y)) (varg-eqv? (car x) (car y)))
        (let ([diff-lst (gen-diff-lst (car x) (car y) dx dy)])
            (cons res (cons (car diff-lst) (unroll-lst (cdr x) (cdr y) (cadr diff-lst) (caddr diff-lst)))))
        ; (cons (list 'if '% (car x) (car y)) (unroll-lst (cdr x) (cdr y) dx dy)))) ; (new-bind (car x) dx) (new-bind (car y) dy)))))
        (list 'if '% (cons orig-x x) (cons orig-y y))))

; this function handles the case when x or y is still a list
; a depth first search from left to right is performed to reach all atoms
(define (unroll-lst x y dx dy)
    (if (and (null? x) (null? y))
        '() ; end of execution is reached here
        (let ([res (expr-compare-helper (car x) (car y) dx dy)]) ; depth first search
            (if (or (equal? res 'lambda) (equal? res lam-sym))
                (handle-lambda res (cdr x) (cdr y) dx dy (car x) (car y)) ; handle lambda expressions
                (cons res (unroll-lst (cdr x) (cdr y) dx dy)))))) ; continue execution left to right

; this function returns true if both the input lists are functions and one is an if statement
(define (fn-mismatch? x y)
    (and (pair? x) (pair? y) 
         (atom? (car x)) (atom? (car y)) 
         (not (equal? (car x) (car y)))
         (or (equal? (car x) 'if) (equal? (car y) 'if))))

; this function compares two atoms and returns the atom if they are equivalent, or 
; the correct if statement is they are different 
(define (ret-atom-diff x y)
    (cond ((equal? x y) x)
          ((and (boolean? x) (boolean? y)) (if x '% '(not %)))
          ((or (equal? x lam-sym) (equal? y lam-sym)) lam-sym)
          (#t (list 'if '% x y))))

; this function checks the binding list and returns a substituted value for the first binding that is found
; if no binding is found, the original value is returned
(define (cb val diff-lst)
    (if (null? diff-lst)
        val
        (if (equal? (caar diff-lst) val)
            (cadar diff-lst)
            (cb val (cdr diff-lst)))))

; this helper function looks up the binding for x and y if they are atoms
; and returns the appropiate expression. If x and y are not both atoms,
; dfs is performed from left to right across the subexpressions
(define (expr-compare-helper x y dx dy)
    (cond ((and (atom? x) (atom? y))
            (let ([x_val (cb x dx)]
                  [y_val (cb y dy)])
                (ret-atom-diff x_val y_val)))
          ((or (is-quoted? x y) (varg? x y)) (ret-atom-diff x y)) ; handle quoted and improper lists
          ((or (atom? x) (atom? y) ; mismatched list with atom
               (not (equal? (length x) (length y))) ; handle unequal lists
               (fn-mismatch? x y)) ; handle mismatched if statements
               (list 'if '% (cb x dx) (cb y dy)))
          (#t (unroll-lst x y dx dy)))) ; do dfs comparison across subexpressions left to right

; the top level function passes x, y, and empty bindings to the main program
; an expression is returned that evaluates to x when % is converted to #t,
; evaluates to y when % is converted to #f
(define (expr-compare x y)
    (expr-compare-helper x y '() '()))

; this tests the expr-compare function above by converting % to #t and #f and
; comparing the output with the output of expressions x and y
(define (test-expr-compare x y)
    (and (equal? (eval x) (eval `(let ([% #t]), (expr-compare x y))))
         (equal? (eval y) (eval `(let ([% #f]), (expr-compare x y))))))

; test-expr-x and test-expr-y provide complex test cases to test the funcionality of expr-compare
(define test-expr-x '(list (list (+ 3 ((lambda (a b z) 
    ((lambda (a b) (+ (- a b) z)) (+ a b) (- b a))) 1 2 3)) '()) 
        #f (quote (cons a b)) (list 1 2 3)))

(define test-expr-y `(list (cons (+ 2 ((lambda (a c x)
    ((,lam-sym (e c) (+ (- e c) x)) (+ a c) (- c a))) 1 2 3)) '())
        #t (quote (list c d)) (if 1 2 3)))