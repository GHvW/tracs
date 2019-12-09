#lang typed/racket
(require threading)

;; Binomial Queue


(define-type (BinomialTree a) (Node a))

(struct (a) Node
  ([data : a]
   [rank : Integer]
   [tree-list : (Listof (BinomialTree a))]))

(define-type (BinomialHeap a) (Listof (BinomialTree a)))

(: make->binomial-tree (All (a) (-> a (BinomialTree a))))
(define (make->binomial-tree data)
  (Node data 0 '()))

(: root (All (a) (-> BinomialTree a)))
(define (root node) (Node-data node))

(: rank (All (a) (-> (BinomialTree a) Integer)))
(define (rank node) (Node-rank node))

(: link (All (a) (-> (BinomialTree a) (BinomialTree a) (BinomialTree a))))
(define (link other this)
  (if (<= (root this) (root other))
      (Node (root this) (+ (rank this) 1) (other . (Node-tree-list this)))
      (Node (root other) (+ (rank other) 1) (this . (Node-tree-list other)))))

(: ins (All (a) (-> BinomialTree (Listof (BinomialTree a)) (Listof (BinomialTree a)))))
(define (ins tree tree-list)
  (if (empty? tree-list)
      (list tree)
      (if (< (rank tree) (rank (car tree-list)))
          (cons tree tree-list)
          (ins (link tree (car tree-list)) (cdr tree-list)))))

(: empty? (All (a) (-> (Listof (BinomialTree a)) Boolean)))
(define (empty? tree-list) (empty? tree-list))

(: empty-heap (All (a) (-> (BinomialHeap a))))
(define empty-heap '())

(: insert (All (a) (-> a (BinomialHeap a) (BinomialHeap a))))
(define (insert data heap) (ins (make->binomial-tree data) heap))

