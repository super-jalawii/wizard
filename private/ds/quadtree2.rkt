#lang racket






(struct Point (x y) #:transparent)
(struct AABB (x y w h) #:transparent)

(struct Node (bounds capacity points children) #:mutable #:transparent)

(define (insert node point)
  (if (contains? point (Node-bounds node))
      (if (and #;((Node-capacity node) . > . (length (Node-points node)))
               ;; Only split when the two points given are different
               (or (null? (Node-points node))
                   (and (eq? (Point-x (car (Node-points node))) (Point-x point))
                        (eq? (Point-y (car (Node-points node))) (Point-y point))))
               (not (Node-children node)))
        (set-Node-points! node (cons point (Node-points node)))
        ;; Otherwise, we need to subdivide and insert all the points from this
        ;; node into child nodes (in addition to inserting the actual point
        ;; we're trying to insert.)

        (begin
          ;; Subdivide the node if it isn't already subdivided.
          (when (not (Node-children node))
            (set-Node-children! node (subdivide node))
            ;; And now that we've subdivided, try to pawn off all your points
            ;; onto your children.
            (for ([point (in-list (Node-points node))])
              (for/or ([quad (in-vector (Node-children node))])
                (insert quad point)))
            (set-Node-points! node '()))
          ;; Try to file away the points in this node into child nodes.
          (for/or ([quad (in-vector (Node-children node))])
            (insert quad point))))
    #f))

(define (subdivide node)
  (match-let ([(AABB x y w h) (Node-bounds node)])
    (let* ([w (/ w 2)] [h (/ h 2)]
           [ne (make-node (AABB x y w h)
                          #:capacity (Node-capacity node))]
           [nw (make-node (AABB (+ x w) y w h)
                          #:capacity (Node-capacity node))]
           [se (make-node (AABB (+ x w) (+ y h) w h)
                          #:capacity (Node-capacity node))]
           [sw (make-node (AABB x (+ y h) w h)
                          #:capacity (Node-capacity node))])
      (vector ne nw se sw))))

(define (contains? point bounds)
  (match-let ([(AABB x y w h) bounds]
              [(Point px py) point])
    (and (>= px x)
         (>= py y)
         (<= px (+ x w))
         (<= py (+ y h)))))

(define (intersects? region bounds)
  (match-let ([(AABB x1 y1 w1 h1) region]
              [(AABB x2 y2 w2 h2) bounds])
    (or (<= x2 x1 (+ x2 w2))
        (<= x1 x2 (+ x1 w1))
        (<= y2 y1 (+ y2 h2))
        (<= y1 y2 (+ y1 h1)))))

(define (make-node bounds [points '()] #:capacity [capacity 1])
  (Node bounds capacity points #f))

#;(define (tree-map tree fn)
  (fn tree)
  (when (Node-children tree)
    (for/list ([quad (in-vector (Node-children tree))])
      (tree-map quad fn))))

(define (tree-map tree fn)
  (tree-transduce
   (map fn)
   (completing cons)
   '() tree))

(define (tree-each tree fn)
  (tree-map tree fn)
  (void))

(require "../aux/transducer.rkt")

;; FIXME: For now, tree-transduce will assume implementation details specific to
;; quadtree.

(define (tree-fold fn init tree)
  (let ([val (fn tree init)])
    (unless (or (not (Node-children tree))
                (reduced? val))
      (for ([child (Node-children tree)])
        (set! val (unreduced (tree-fold fn val child)))))
    val))

(define tree-transduce
  (case-lambda
    ([xform f coll] (tree-transduce xform f (f) coll))
    ([xform f init coll]
     (let* ([xf (xform f)] ;; Add the last little bit in there
            [ret (tree-fold xf init coll)]) ;; Do the actual thing - foldl
                                            ;; basically does what we want for
                                            ;; lists. So we need to write
                                            ;; "foldl" for trees.
       (unreduced (xf (unreduced ret)))))))

(define (query-at-point tree point)
  (flatten (tree-transduce (compose (filter (λ (node) (contains? point (Node-bounds node))))
                           (filter (λ (node) (and (not (null? (Node-points node)))
                                                  (and (eq? (Point-x (car (Node-points node)))
                                                            (Point-x point))
                                                       (eq? (Point-y (car (Node-points node)))
                                                            (Point-y point))))))
                           (map Node-points))
                           (completing cons) '() tree)))

(define (query-region tree region)
  (flatten (tree-transduce (compose (filter (λ (node) (intersects? region (Node-bounds node))))
                                    (filter (λ (node) (and (not (null? (Node-points node)))
                                                           (contains? (car (Node-points node)) region))))
                                    (map Node-points))
                           (completing cons) '() tree)))


;; Testing --------------------------

(define qt (make-node (AABB 0 0 800 800) #:capacity 1))

(for ([i (in-range 10)])
  (let ([x (random 400 800)]
        [y (random 400 800)])
    (printf "~n~a" (Point x y))
    (insert qt (Point x y))))

(require raylib)

(define (show tree)

  (init-window 800 800 "QuadTree Test")

  (draw-begin)

  (clear-background WIZARDWHITE)

  (tree-each qt
             (λ (node)
               (match-let ([(AABB x y w h) (Node-bounds node)])
                 (let ([id (random 0 1000)]
                       [col (make-Color (random 0 255)
                                        (random 0 255)
                                        (random 0 255)
                                        255)])
                   #;(draw-text (format "~a,~a" x y) (exact-truncate x) (exact-truncate y) 5 col)
                   (draw-rect-lines
                    (exact-truncate x)
                    (exact-truncate y)
                    (exact-truncate w)
                    (exact-truncate h) col)
                   (for ([pt (in-list (Node-points node))])
                     #;(draw-text (format "~a,~a" (Point-x pt) (Point-y pt))
                                (Point-x pt)
                                (Point-y pt)
                                5
                                col)
                     (draw-circle (Point-x pt) (Point-y pt) 2. col))))))

  (draw-end))

