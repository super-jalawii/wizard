#lang racket


(require "geom.rkt")
(provide (all-defined-out))

;; Datatypes:

(struct node (bounds objects active-children children parent) #:mutable #:transparent)
(struct octree (root min-size bounds))

;; Node functions:

(define [make-node bounds objects parent]
  (node bounds objects 0 (make-vector 8) parent))

(define [insert! node objects
                 #:min-size   [min-size 1]
                 #:get-bounds object-bounds]

  ; Make sure that objects is a list while allowing the user to insert single
  ; items.
  (unless (list? objects) (set! objects (list objects)))

  ; We work with the old and new objects in aggregate because the act of
  ; inserting these new objects may cause us to "refile" previous ones.
  (let ([objs (append objects (node-objects node))]
        [dim  (dimensions (node-bounds node))     ])

    ; If we don't need to subdivide, then we just insert the objects and leave.
    ; We short circuit the check by looking at active-children first to see if
    ; we already have subdivided. If that's the case we will always want to go
    ; ahead. (I think).
    (if (and (eq? 0 (node-active-children node))
             (not (should-subdivide? node #:min-size min-size)))
        (set-node-objects! node objs)
        (let ([octants  (build-octants (node-bounds node))]
              [octlist  (make-vector 8 '())                     ]
              [delisted '()])

          ; File each object into the appropriate octant if possible. Objects
          ; left over are too big to fit into a child octant, so they stay at
          ; this level.

          (for ([obj (in-list objs)])
            (for*/or ([i (in-range 8)])
              (if (contains? (vector-ref octants i) (object-bounds obj))
                  (begin
                    (vector-set! octlist i (cons obj (vector-ref octlist i)))
                    (set! delisted (cons obj delisted)))
                  #f)))

          ; Now that we've got objects sorted into buckets, insert each object
          ; into the appropriate child node, or keep at this level.

          (set-node-objects! node (for/list ([obj objs] #:unless (member obj delisted)) obj))

          (let ([active (node-active-children node)])

            ; Use a list comprehension to update the active-children bitfield.

            (set-node-active-children!
             node
             (apply bitwise-ior
                    (cons active
                          (for/list ([i (in-range 8)]
                                     #:unless (eq? 0 (length (vector-ref octlist i))))
                            (let* ([octant (vector-ref octants i)]
                                   [child-objects (vector-ref octlist i)]
                                   [child-node (if (bitwise-bit-set? active i)
                                                   (vector-ref (node-children node) i)
                                                   (make-node octant '() node))])
                              (vector-set! (node-children node) i (insert! child-node child-objects
                                                                           #:min-size   min-size
                                                                           #:get-bounds object-bounds)))
                            (arithmetic-shift 1 i)))))))))

  ; Return the modified node after all modifications have been made.

  node)

(define [remove! node objects
                 #:get-bounds object-bounds]

  ; "Coerce" single objects into a list

  (unless (list? objects) (set! objects (list objects)))

  ; For each object in objects, traverse the tree to find the object (based on
  ; bounds), then, using equal?, remove the object (or objects I guess) which
  ; are equal to the current object.

  ;; FIXME: We should consider releasing octants which haven't been used in a
  ;; while and which no longer contain any elements...

  ;; FIXME: This will work, but it kind of sucks because it isn't really using
  ;; the octree at all. We could do this in O(log n) time, but instead it will
  ;; be O(n).
  (for ([obj objects])
    (let ([containing-node
           (node-member obj node #:get-bounds object-bounds)])
      (when containing-node
        (set-node-objects!
         containing-node
         (for/list ([o (node-objects containing-node)] #:unless (equal? o obj)) o))))))


;; Octree Traversal functions:

(define [octree-map fn tree]
  (node-map fn (octree-root tree)))

; For what is hopefully convenience - this function returns a list, not an
; octree (since it would be hard for arbitrary code to keep the invariants of
; the tree anyway). If this doesn't make sense, we can always update the API.
(define [node-map fn node]
  (cons (apply fn (list node))
        (for/list ([ch-node (node-children node)]
                   #:unless (eq? 0 ch-node))
          (node-map fn ch-node))))

; Returns the node which contains the object given. Returns #f when no object is
; found.
(define [node-member object node
                     #:get-bounds object-bounds]
  (or (if (member object (node-objects node)) node #f)

      ; Figure out which octant it could possibly be in
      (for*/or ([ch-node (in-vector (node-children node))]
                [i       (in-range  8)                   ]
                #:unless (eq? 0 ch-node))
        (if (contains? (node-bounds ch-node) (object-bounds object))
            (node-member object ch-node #:get-bounds object-bounds)
            #f))))

;; Octree functions:

; get-bounds is a function can be supplied which, when applied to objects, returns
; the bounding box. If no function is specified, it is assumed that objects
; *are* bounding boxes.
(define [make-octree #:min-size   [min-size 1   ]
                     #:max-size   [max-size 1000]
                     #:get-bounds [bounds   (lambda (bb) bb)]]
  (let ([initial-bbox (bounding-box (vec3        0        0        0)
                                    (vec3 max-size max-size max-size))])
    (octree (make-node initial-bbox '() '()) min-size bounds)))

;; TODO: Consider defining a function which builds an octree with initial
;; elements inserted.

(define [octree-insert! tree obj]
  (insert! (octree-root tree) obj
           #:min-size   (octree-min-size tree)
           #:get-bounds (octree-bounds tree)))

(define [octree-update! tree obj]
  (remove! (octree-root tree) obj)
  (insert! (octree-root tree) obj
           #:min-size   (octree-min-size tree)
           #:get-bounds (octree-bounds tree)))

(define [octree-remove! tree obj]
  (remove! (octree-root tree) obj
           #:get-bounds (octree-bounds tree)))

(define [octree-member obj tree]
  (node-member obj (octree-root tree)
               #:get-bounds (octree-bounds tree)))

;; Aux functions:

(define [build-octants bb]
  (let* ([min    (bounding-box-min bb)]
         [max    (bounding-box-max bb)]
         [dim    (dimensions bb)]
         [half   (vec3-scalar-divide dim 2.0)]
         [center (vec3-add min half)])
    (vector (bounding-box min center)
            (bounding-box (vec3 (vec3-x center) (vec3-y min)    (vec3-z min))
                          (vec3 (vec3-x max)    (vec3-y center) (vec3-z center)))
            (bounding-box (vec3 (vec3-x center) (vec3-y min)    (vec3-z center))
                          (vec3 (vec3-x max)    (vec3-y center) (vec3-z max)))
            (bounding-box (vec3 (vec3-x min)    (vec3-y min)    (vec3-z center))
                          (vec3 (vec3-x center) (vec3-y center) (vec3-z max)))
            (bounding-box (vec3 (vec3-x min)    (vec3-y center) (vec3-z min))
                          (vec3 (vec3-x center) (vec3-y center) (vec3-z min)))
            (bounding-box (vec3 (vec3-x center) (vec3-y center) (vec3-z min))
                          (vec3 (vec3-x max)    (vec3-y max)    (vec3-z center)))
            (bounding-box center max)
            (bounding-box (vec3 (vec3-x min)    (vec3-y center) (vec3-z center))
                          (vec3 (vec3-x center) (vec3-y max)    (vec3-z max))))))

(define [should-subdivide? node #:min-size [min-size 1]]
  (let ([dim (dimensions (node-bounds node))])
    (and (not (null? (node-objects node)))
         (and (> (vec3-x dim) min-size)
              (> (vec3-y dim) min-size)
              (> (vec3-z dim) min-size)))))

