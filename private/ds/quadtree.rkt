#lang racket/base

(require racket/generic
         racket/match
         racket/list
         racket/vector
         "../aux/transducer.rkt")

(provide gen:spatial
         gen:tree
         tree:data
         (struct-out Point)
         (struct-out AABB)
         contains?
         intersects?
         QuadTree
         (rename-out [insert! QuadTree-insert!]
                     [remove! QuadTree-remove!]
                     [tree-map tree:map]
                     [tree-each tree:each]
                     [tree-fold tree:fold]
                     [Node-bounds quadtree:bounds]
                     [tree-transduce tree:transduce]
                     [quadtree-query-at-point QuadTree-at-point]
                     [quadtree-query-region QuadTree-in-region]))

(define-generics spatial
  (spatial:get-origin spatial))

(define-generics tree
  (tree:children tree)
  (tree:data tree))

(define (QuadTree bounds . init)
  (let ([qt (make-node bounds)])
    (for ([elt (in-list init)])
      (insert! qt elt))
    qt))

;; FIXME: We actually don't need this struct, we can just return values instead.
(struct Point (x y)
  #:transparent
  #:methods gen:spatial
  [(define (spatial:get-origin self) self)])

(struct AABB (x y w h) #:transparent)
(struct Node (bounds data children)
  #:mutable
  #:methods gen:tree
  [(define (tree:children self) (Node-children self))
   (define (tree:data self) (Node-data self))])

(define (insert! node elt)
  (match-let ([(and (Point x y)
                    (var point)) (spatial:get-origin elt)])
    (if (contains? point (Node-bounds node))
        (if (and (or (null? (Node-data node))
                     (let* ([point2 (spatial:get-origin (car (Node-data node)))]
                            [x2 (Point-x point2)]
                            [y2 (Point-y point2)])
                       (eq? x x2) (eq? y y2)))
                 (not (Node-children node)))
            (set-Node-data! node (cons elt (Node-data node)))
            (begin
              (when (not (Node-children node))
                (set-Node-children! node (subdivide node))
                (for ([elt (in-list (Node-data node))])
                  (for/or ([quad (in-vector (Node-children node))])
                    (insert! quad elt)))
                (set-Node-data! node '()))
              (for/or ([quad (in-vector (Node-children node))])
                (insert! quad elt))))
        #f)))

#;(define (find node elt))
(define (remove! node elt)
  (match-let ([(and (Point x y) (var point)) (spatial:get-origin elt)])
    (if (contains? point (Node-bounds node))
        (if (not (Node-children node))
            ;; No children, the node to remove must be here?
            (if (member elt (Node-data node))
                (and (set-Node-data! node (remove elt (Node-data node)))
                     (when (null? (Node-data node))
                       ;; TODO: When there's nothing left in this node - we
                       ;; should delete this node somehow. We can only really do
                       ;; this (in the current implementation) by asking the
                       ;; parent, and even then - only when no child nodes
                       ;; contain data.
                       (set-Node-children! node #f))
                     #t)
                #f)
            (ormap (λ (x) x) (vector->list (vector-map (λ (n) (remove! n elt)) (Node-children node)))))
        #f)))
#;(define (update node elt fn))

(define (subdivide node)
  (match-let ([(AABB x y w h) (Node-bounds node)])
    (let* ([w (/ w 2)] [h (/ h 2)]
                       [ne (make-node (AABB x y w h))]
                       [nw (make-node (AABB (+ x w) y w h))]
                       [se (make-node (AABB (+ x w) (+ y h) w h))]
                       [sw (make-node (AABB x (+ y h) w h))])
      (vector ne nw se sw))))

(define (contains? point bounds)
  (match-let ([(AABB x y w h) bounds]
              [(Point px py) point])
    (and (>= px x)
         (>= py y)
         (< px (+ x w))
         (< py (+ y h)))))

(define (intersects? region bounds)
  (match-let ([(AABB x1 y1 w1 h1) region]
              [(AABB x2 y2 w2 h2) bounds])
    (or (<= x2 x1 (+ x2 w2))
        (<= x1 x2 (+ x1 w1))
        (<= y2 y1 (+ y2 h2))
        (<= y1 y2 (+ y1 h1)))))

(define (make-node bounds [data '()])
  (Node bounds data #f))

;; Queries and Transducer Stuff ---------------------------------

(define (tree-map tree fn)
  (tree-transduce
   (map fn)
   (completing cons)
   '() tree))

(define (tree-each tree fn)
  (tree-map tree fn)
  (void))

(define (tree-fold fn init tree)
  (let ([val (fn tree init)])
    (unless (or (not (tree:children tree))
                (reduced? val))
      (for ([child (tree:children tree)])
        (set! val (unreduced (tree-fold fn val child)))))
    val))

(define tree-transduce
  (case-lambda
    ([xform f coll] (tree-transduce xform f (f) coll))
    ([xform f init coll]
     (let* ([xf (xform f)]
            [ret (tree-fold xf init coll)])
       (unreduced (xf (unreduced ret)))))))

(define (quadtree-query-at-point tree point)
  (flatten (tree-transduce
            (compose (filter (λ (node) (contains? point (Node-bounds node))))
                     (filter (λ (node)
                               (and (not (null? (Node-data node)))
                                    (let* ([p2 (spatial:get-origin (car (Node-data node)))]
                                           [x2 (Point-x p2)] [y2 (Point-y p2)])
                                      (and (eq? (Point-x point) x2)
                                           (eq? (Point-y point) y2))))))
                     (map Node-data))
            (completing cons) '() tree)))

(define (quadtree-query-region tree region)
  (flatten (tree-transduce
            (compose (filter (λ (node) (intersects? region (Node-bounds node))))
                     (filter (λ (node)
                               (and (not (null? (Node-data node)))
                                    (contains? (spatial:get-origin (car (Node-data node)))
                                               region))))
                     (map Node-data))
            (completing cons) '() tree)))

