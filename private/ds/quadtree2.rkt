#lang racket

(require racket/generic)

(define-generics spatial
  (spatial:get-origin spatial))

(struct Point (x y)
  #:transparent
  #:methods gen:spatial
  [(define (spatial:get-origin self) self)])

(struct Position (eid x y)
  #:transparent
  #:methods gen:spatial
  [(define (spatial:get-origin self)
     (Point (Position-x self) (Position-y self)))])

(struct AABB (x y w h) #:transparent)
(struct Node (bounds data children) #:mutable #:transparent)

(define (insert node elt)
  (match-let ([(and (Point x y)
                    (var point)) (spatial:get-origin elt)])
    (if (contains? point (Node-bounds node))
        (if (and #;((Node-capacity node) . > . (length (Node-points node)))
                 ;; Only split when the two points given are different
                 (or (null? (Node-data node))
                     (and (let* ([point2 (spatial:get-origin (car (Node-data node)))]
                                 [x2 (Point-x point2)]
                                 [y2 (Point-y point2)])
                            (eq? x x2) (eq? y y2))))
                 (not (Node-children node)))
            (set-Node-data! node (cons elt (Node-data node)))
            ;; Otherwise, we need to subdivide and insert all the points from this
            ;; node into child nodes (in addition to inserting the actual point
            ;; we're trying to insert.)

            (begin
              ;; Subdivide the node if it isn't already subdivided.
              (when (not (Node-children node))
                (set-Node-children! node (subdivide node))
                ;; And now that we've subdivided, try to pawn off all your points
                ;; onto your children.
                (for ([elt (in-list (Node-data node))])
                  (for/or ([quad (in-vector (Node-children node))])
                    (insert quad elt)))
                (set-Node-data! node '()))
              ;; Try to file away the points in this node into child nodes.
              (for/or ([quad (in-vector (Node-children node))])
                (insert quad elt))))
        #f)))

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
         (<= px (+ x w))
         (<= py (+ y h)))))

(define (intersects? region bounds)
  (match-let ([(AABB x1 y1 w1 h1) region]
              [(AABB x2 y2 w2 h2) bounds])
    (or (<= x2 x1 (+ x2 w2))
        (<= x1 x2 (+ x1 w1))
        (<= y2 y1 (+ y2 h2))
        (<= y1 y2 (+ y1 h1)))))

(define (make-node bounds [data '()])
  (Node bounds data #f))

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

(define (query-region tree region)
  (flatten (tree-transduce
            (compose (filter (λ (node) (intersects? region (Node-bounds node))))
                     (filter (λ (node)
                               (and (not (null? (Node-data node)))
                                    (contains? (spatial:get-origin (car (Node-data node)))
                                               region))))
                     (map Node-data))
            (completing cons) '() tree)))


;; Testing --------------------------

(define qt (make-node (AABB 0 0 800 800)))

(for ([i (in-range 10)])
  (let ([x (random 400 800)]
        [y (random 400 800)]
        [eid (random 0 1000)])
    (printf "~n~a" (Position eid x y))
    (insert qt (Position eid x y))))

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
                   (draw-rect-lines x y w h col)
                   (for ([elt (in-list (Node-data node))])
                     (let* ([pt (spatial:get-origin elt)]
                            [x (Point-x pt)]
                            [y (Point-y pt)])
                       (draw-circle x y 2. col)))))))

  (draw-end))

