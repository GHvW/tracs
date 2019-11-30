#lang typed/racket

;; Binomial Queue
(define-type (a) BinomialHeap (Listof (BinomialTree a)))

(define-type (a) BinomialTree (Node a))

(struct (a) Node
  ([data : a]
   [rank : Integer]
   [tree-list : (Listof BinomialTree)]))

(: root (All (a) (-> BinomialTree a)))
(define (root node) (Node-data node))

(: rank (-> BinomialTree Integer))
(define (rank node) (Node-rank node))

(: link (-> BinomialTree BinomialTree BinomialTree))
(define (link other this)
  (if (<= (root this) (root other))
      (Node (root this) (+ (rank this) 1) (other . (Node-tree-list this)))
      (Node (root other) (+ (rank other) 1) (this . (Node-tree-list other)))))

(: ins (-> BinomialTree (Listof BinomialTree) (Listof BinomialTree)))
(define (ins tree tree-list)
  (if (empty? tree-list)
      (list tree)
      (if (< (rank tree) (rank (car tree-list)))
          (cons tree tree-list)
          (ins (link tree (car tree-list)) (cdr tree-list)))))

(: empty? (-> (Listof BinomialTree) Boolean))
(define (empty? tree-list) (empty? tree-list))

(: empty-heap (-> BinomialHeap))
(define (empty-heap) '())

(: insert (All (a) (-> a (BinomialHeap a) (BinomialHeap a))))
(define (insert data heap) (ins (Node data 0 '()) heap))

