#lang racket/base

(require racket/generic
         racket/match
         racket/list
         racket/vector
         "../aux/transducer.rkt")

(provide gen:spatial
         gen:tree
         tree:data
         (struct-out point)
         (struct-out aabb)
         contains?
         intersects?
         quadtree
         (rename-out [insert! quadtree-insert!]
                     [remove! quadtree-remove!]
                     [tree-map tree:map]
                     [tree-each tree:each]
                     [tree-fold tree:fold]
                     [node-bounds quadtree-bounds]
                     [tree-transduce tree:transduce]
                     [quadtree-query-at-point quadtree-at-point]
                     [quadtree-query-region quadtree-in-region]))

(define-generics spatial
  (spatial:get-origin spatial))

(define-generics tree
  (tree:children tree)
  (tree:data tree))

(define (quadtree bounds . init)
  (let ([qt (make-node bounds)])
    (for ([elt (in-list init)])
      (insert! qt elt))
    qt))

;; FIXME: We actually don't need this struct, we can just return values instead.
(struct point (x y)
  #:constructor-name $point
  #:transparent
  #:methods gen:spatial
  [(define (spatial:get-origin self) self)])

(struct aabb (x y w h)
  #:constructor-name $aabb
  #:transparent)

(struct node (bounds data children)
  #:constructor-name $node
  #:mutable
  #:methods gen:tree
  [(define (tree:children self) (node-children self))
   (define (tree:data self) (node-data self))])

(define (insert! node elt)
  (match-let ([(and (point x y)
                    (var point)) (spatial:get-origin elt)])
    (if (contains? point (node-bounds node))
        (if (and (or (null? (node-data node))
                     (let* ([point2 (spatial:get-origin (car (node-data node)))]
                            [x2 (point-x point2)]
                            [y2 (point-y point2)])
                       (eq? x x2) (eq? y y2)))
                 (not (node-children node)))
            (set-node-data! node (cons elt (node-data node)))
            (begin
              (when (not (node-children node))
                (set-node-children! node (subdivide node))
                (for ([elt (in-list (node-data node))])
                  (for/or ([quad (in-vector (node-children node))])
                    (insert! quad elt)))
                (set-node-data! node '()))
              (for/or ([quad (in-vector (node-children node))])
                (insert! quad elt))))
        #f)))

(define (remove! node elt)
  (match-let ([(and (point x y) (var point)) (spatial:get-origin elt)])
    (if (contains? point (node-bounds node))
        (if (not (node-children node))
            ;; No children, the node to remove must be here?
            (if (member elt (node-data node))
                (and (set-node-data! node (remove elt (node-data node)))
                     (when (null? (node-data node))
                       ;; TODO: When there's nothing left in this node - we
                       ;; should delete this node somehow. We can only really do
                       ;; this (in the current implementation) by asking the
                       ;; parent, and even then - only when no child nodes
                       ;; contain data.
                       (set-node-children! node #f))
                     #t)
                #f)
            (ormap (λ (x) x) (vector->list (vector-map (λ (n) (remove! n elt)) (node-children node)))))
        #f)))

(define (subdivide node)
  (match-let ([(aabb x y w h) (node-bounds node)])
    (let* ([w (/ w 2)] [h (/ h 2)]
                       [ne (make-node ($aabb x y w h))]
                       [nw (make-node ($aabb (+ x w) y w h))]
                       [se (make-node ($aabb (+ x w) (+ y h) w h))]
                       [sw (make-node ($aabb x (+ y h) w h))])
      (vector ne nw se sw))))

(define (contains? pt bounds)
  (match-let ([(aabb x y w h) bounds]
              [(point px py) pt])
    (and (>= px x)
         (>= py y)
         (< px (+ x w))
         (< py (+ y h)))))

(define (intersects? region bounds)
  (match-let ([(aabb x1 y1 w1 h1) region]
              [(aabb x2 y2 w2 h2) bounds])
    (or (<= x2 x1 (+ x2 w2))
        (<= x1 x2 (+ x1 w1))
        (<= y2 y1 (+ y2 h2))
        (<= y1 y2 (+ y1 h1)))))

(define (make-node bounds [data '()])
  ($node bounds data #f))

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
            (compose (filter (λ (node) (contains? point (node-bounds node))))
                     (filter (λ (node)
                               (and (not (null? (node-data node)))
                                    (let* ([p2 (spatial:get-origin (car (node-data node)))]
                                           [x2 (point-x p2)] [y2 (point-y p2)])
                                      (and (eq? (point-x point) x2)
                                           (eq? (point-y point) y2))))))
                     (map node-data))
            (completing cons) '() tree)))

(define (quadtree-query-region tree region)
  (flatten (tree-transduce
            (compose (filter (λ (node) (intersects? region (node-bounds node))))
                     (filter (λ (node)
                               (and (not (null? (node-data node)))
                                    (contains? (spatial:get-origin (car (node-data node)))
                                               region))))
                     (map node-data))
            (completing cons) '() tree)))

