#lang racket
(define (fizz-buzz)
  (for ([i (in-range 1 101)])
     (cond
    ((and (eq? (remainder i 3) 0) (eq? (remainder i 5) 0)) (displayln "fizzbuzz"))
    ((eq? (remainder i 3) 0) (displayln "fizz"))
    ((eq? (remainder i 5) 0) (displayln "buzz"))
    (else
     (displayln i))))
  )


(define (my-gcd m n)
  (cond
    ((eq? m 0) (displayln n))
    ((eq? n 0) (displayln m))
    ((eq? m n) (displayln m))
    ((> m n) (my-gcd (- m n) n))
    ((< m n) (my-gcd m (- n m)))
   )
  )


(define (linear-search items key)
  (cond
    [(empty? items) #f]
    {(list? items) [if (= (first items) key) #t (linear-search (rest items) key) ]}
    [else #f]
   )
 )



(define (my-remove items key)
  (cond
   [(empty? items) '()]
   [(equal? (first items) key) (my-remove (rest items) key) ]
   {else (cons (first items) (my-remove (rest items) key))}
   )
  )



(define (add-to-binary-search-tree bst item)
  (cond
    [(null? bst) (list item )]
    [(and (< item (first bst)) (= (length bst) 1 )) (list (first bst) (append '() (list item)))]
    [(and (< item (first bst)) (= (length bst) 2 )) (list (first bst) (add-to-binary-search-tree (second bst) item))]
    [(and (< item (first bst)) (= (length bst) 3 )) (list (first bst) (add-to-binary-search-tree (second bst) item) (append '() (third bst)))]
    [(and (> item (first bst)) (= (length bst) 1 )) (list (first bst) '() (append '() (list item)))]
    [(and (> item (first bst)) (= (length bst) 2 )) (list (first bst) (add-to-binary-search-tree (second bst) (first (second bst))) (append '() (list item)))]
    [(and (> item (first bst)) (= (length bst) 3 )) (list (first bst) (append '() (second bst)) (add-to-binary-search-tree (third bst) item))]
    [(= item (first bst)) bst]
    ) 
  )


(define (create-binary-search-tree items)
;(add-to-binary-search-tree '() (first items))
;(create-binary-search-tree (my-remove items (first items)))
  (define bst null)
 (if (null? items) bst
(cond
  [(= (length bst) 0)(list (first items) )]

   )

  )
  )



(define (search-binary-search-tree bst key)
 (if (null? bst) #f
     (cond
       [(= (first bst) key) #t]
       [(and (not(= key (first bst))) (= (length bst) 1 )) #f]
       [(and (< key (first bst)) (> (length bst) 1 )) (search-binary-search-tree (second bst) key)]
       [(and (> key (first bst)) (= (length bst) 2 )) #f]
       [(and (> key (first bst)) (= (length bst) 3 )) (search-binary-search-tree (third bst) key)]
   
      )
  )
  )


(define (binary-search-tree-to-list bst traversal)
  (if(null? bst) (append '())
      (cond
        ;postorder
        [(and (eq? traversal "postorder") (=(length bst) 1)) (list(first bst) )]
        [(and (eq? traversal "postorder") (=(length bst) 2)) (append (binary-search-tree-to-list (second bst) "postorder") (list(first bst)))]
        [(and (eq? traversal "postorder") (=(length bst) 3)) (append (binary-search-tree-to-list (second bst) "postorder") (binary-search-tree-to-list (third bst) "postorder") (list(first bst)))]
        ;preorder
        [(and (eq? traversal "preorder") (=(length bst) 1)) (list(first bst) )]
        [(and (eq? traversal "preorder") (=(length bst) 2)) (append (list(first bst)) (binary-search-tree-to-list (second bst) "preorder") )]
        [(and (eq? traversal "preorder") (=(length bst) 3)) (append (list(first bst)) (binary-search-tree-to-list (second bst) "preorder") (binary-search-tree-to-list (third bst) "preorder"))]
        ;inorder
        [(and (eq? traversal "inorder") (=(length bst) 1)) (list(first bst) )]
        [(and (eq? traversal "inorder") (=(length bst) 2)) (append (binary-search-tree-to-list (second bst) "inorder") (list(first bst)))]
        [(and (eq? traversal "inorder") (=(length bst) 3)) (append (binary-search-tree-to-list (second bst) "inorder") (list(first bst)) (binary-search-tree-to-list (third bst) "inorder"))]
       )
  )
  )