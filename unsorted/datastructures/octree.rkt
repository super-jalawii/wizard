#lang racket


(require graph)



;; FIXME: Move this out into somewhere that makes more sense
(struct vec3 (x y z) #:transparent)

(define [vec3-scalar-divide v n]
  (vec3
   (/ (vec3-x v) n)
   (/ (vec3-y v) n)
   (/ (vec3-z v) n)))

(define [vec3-subtract v1 v2]
  (vec3
   (- (vec3-x v1) (vec3-x v2))
   (- (vec3-y v1) (vec3-y v2))
   (- (vec3-z v1) (vec3-z v2))))


(define [vec3-add v1 v2]
  (vec3
   (+ (vec3-x v1) (vec3-x v2))
   (+ (vec3-y v1) (vec3-y v2))
   (+ (vec3-z v1) (vec3-z v2))))

;; Returns true when v1 is closer to the origin than v2
(define [vec3-lt v1 v2]
  (and (< (vec3-x v1) (vec3-x v2))
       (< (vec3-y v1) (vec3-y v2))
       (< (vec3-z v1) (vec3-z v2))))

(struct cube (pos dim))

;; FIXME: There's nothing currently forcing min to be less than max
(struct bounding-box (min max) #:mutable #:transparent)

(define [dimensions bbox]
  (vec3-subtract (bounding-box-max bbox)
                 (bounding-box-min bbox)))

(define [contains? bb1 bb2]
  (and (vec3-lt (bounding-box-min bb1) (bounding-box-min bb2))
       (vec3-lt (bounding-box-max bb2) (bounding-box-max bb1))))

;; TODO: make-octree returns an octree structure which maintains the root node,
;; parameters which apply to the whole thing (like "min-size" etc) and a queue
;; of objects yet to be inserted. Objects are inserted when accessing the tree
;; or perhaps when 

(struct node (bounding-box objects active-children child-nodes parent-node) #:mutable #:transparent)
(struct octree (root-node pending-inserts min-size))

(define [make-octree #:min-size [min-size 1   ]
                     #:max-size [max-size 1000]]
  (octree (make-node (bounding-box (vec3        0        0        0)
                                   (vec3 max-size max-size max-size))
                     '()
                     '())
          '()
          min-size))

(define [octree-insert! octree obj]
  (insert!! (octree-root-node octree) obj))

;;; NOTE: Bounding-box is supplied starting with the initial octree declaration,
;;; and recalculated by subdividing for child nodes.
(define [make-node bounding-box objects parent]
  ;; Initialize a new node with no children (marking no octants as active)
  (node bounding-box objects 0 (make-vector 8) parent))

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
  (let ([dim (dimensions (node-bounding-box node))])
    (and (not (null? (node-objects node)))
         (and (> (vec3-x dim) min-size)
              (> (vec3-y dim) min-size)
              (> (vec3-z dim) min-size)))))

;;; This function is a mess because the "tutorial" I'm following is also a mess.
;;; Also for performance reasons?
(define [insert! node obj #:min-size [min-size 1 ]]

  ;; Insert an object into the given tree node. If the requirements are met,
  ;; this node should subdivide and pass the object along.

  (let ([dim (dimensions (node-bounding-box node))])
    (println (format "Dimensions: ~a" dim))
    (println (format "Adding ~a to ~a" obj (node-objects node)))
    (set-node-objects! node (cons obj (node-objects node)))


    ;; We only have to build the octants if they didn't already exist (this
    ;; means the child nodes really). The tutorial I think made a distinct
    ;; function for this operation. Which is dumb. So we're gonna have to write
    ;; another one.


    (when (should-subdivide? node #:min-size min-size)
          (let* ([octants  (build-octants (node-bounding-box node))]
                 [octlist  (make-vector 8 '())]
                 [delisted '()])

            ;; TODO: This function is designed to insert multiple objects at the
            ;; same time, but we currently only do one.

            (for*/or ([obj (node-objects node)]
                      [i   (in-range 8)])

              ;; FIXME: We can break after this condition, but I don't know how
              ;; (Take a look at "for*/or")

              (println "Contains?")
              (println (format "~a" obj))
              (println (format "~a" (vector-ref octants i)))

              (if (contains? (vector-ref octants i) obj)
                  (begin
                    (vector-set! octlist i (cons obj (vector-ref octlist i)))
                    (set! delisted (cons obj delisted)))
                  #f))

            (println (format "Octlist: ~a" octlist))
            (println (format "Delisted: ~a" delisted))
            (println (format "Objects: ~a" (node-objects node)))

            (set-node-objects! node (for/list ([obj (node-objects node)] #:unless (member obj delisted)) obj))

            (println (format "Objects After Culling: ~a" (node-objects node)))

            ;; Create child nodes where there are objects in octlist (and set the appropriate bit in active-objects)


            ;; FIXME:::: We're currently getting duplicate items, and they seem
            ;; to be overwriting other items sometimes. I'm not sure what the
            ;; cause is, but I think when we get it sorted out, this function
            ;; will be finally working correctly...


            (for ([i (in-range 8)]
                  #:unless (eq? 0 (length (vector-ref octlist i))))

              (let* ([octant        (vector-ref octants i)]
                     [child-objects (vector-ref octlist i)]
                     [active        (node-active-children node)]
                     [child-node    (make-node octant '() node)])

                (vector-set! (node-child-nodes node) i child-node)
                (set-node-active-children! node (bitwise-ior (arithmetic-shift 1 i) active))
                (for ([ch-obj child-objects]) (insert! child-node ch-obj))))

            (println (format "Bounding Box: ~a" (node-bounding-box node)))
            (println (format "Active Nodes: ~b" (node-active-children node))))))
    node)


(define [insert!! node objects #:min-size [min-size 1]]

  ; Make sure that objects is a list while allowing the user to insert single
  ; items.
  (unless (list? objects) (set! objects (list objects)))

  ; We work with the old and new objects in aggregate because the act of
  ; inserting these new objects may cause us to "refile" previous ones.
  (let ([objs (append objects (node-objects node)) ]
        [dim  (dimensions (node-bounding-box node))])

    ; If we don't need to subdivide, then we just insert the objects and leave.
    ; We short circuit the check by looking at active-children first to see if
    ; we already have subdivided. If that's the case we will always want to go
    ; ahead. (I think).
    (if (and (eq? 0 (node-active-children node))
             (not (should-subdivide? node #:min-size min-size)))
        (set-node-objects! node objs)
        (let ([octants  (build-octants (node-bounding-box node))]
              [octlist  (make-vector 8 '())                     ]
              [delisted '()])

          ; File each object into the appropriate octant if possible. Objects
          ; left over are too big to fit into a child octant, so they stay at
          ; this level.

          (for ([obj (in-list objs)])
            (for*/or ([i (in-range 8)])
              (if (contains? (vector-ref octants i) obj)
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
                                                   (vector-ref (node-child-nodes node) i)
                                                   (make-node octant '() node))])
                              (vector-set! (node-child-nodes node) i (insert!! child-node child-objects)))
                            (arithmetic-shift 1 i)))))))))

  ; Return the modified node after all modifications have been made.

  node)




;;//Start a count down death timer for any leaf nodes which don't have objects or children.
;;//when the timer reaches zero, we delete the leaf. If the node is reused before death, we double its lifespan.
;;//this gives us a "frequency" usage score and lets us avoid allocating and deallocating memory unnecessarily

(define [update! node]

  ;; Update child nodes - this must be done first because child nodes will "push
  ;; up" objects which moved.

  (for ([i      (in-range 8)           ]
        [child  (in-vector (node-child-nodes node))]
        #:when  (bitwise-bit-set? (node-active-children node) i))
    (println (format "Updating child node: ~a" i))
    (update! child))

  ;; //If an object moved, we can insert it into the parent and that will insert it into the correct tree node.
  ;; //note that we have to do this last so that we don't accidentally update the same object more than once per frame.
  ;; //figure out how far up the tree we need to go to reinsert our moved object
  ;; //we are either using a bounding rect or a bounding sphere
  ;; //try to move the object into an enclosing parent node until we've got full containment

  ;; FIXME: We really only want to consider objects that moved. Right now we
  ;; have to look at every object since we don't have that concept yet.
  (let ([parent (node-parent-node node)])
    (unless (null? parent)

      ;; FIXME: Good lord this is awful. A comprehension with side-effects XD
      (set-node-objects!
       node
       (for/list ([obj (node-objects node)]
                  #:when (if (contains? (node-bounding-box node) obj)
                             (begin (insert! node obj)                                   #t)
                             (begin (set-node-objects! (cons obj (node-objects parent))) #f)))
         obj))))

  ;; TODO: Somewhere in here is the part where we can say whether things are
  ;; colliding (but we may want to have some kind of "query" function as well).

  )


;;; FIXME: Our main issue here is the assumption that we're maintaining a
;;; reference to the object. This is where we need to implement objects properly
;;; (in terms of components) so that we can find a particular object. The real
;;; way to update (to me...) would be to remove and reinsert the element after
;;; an update. That would make more sense - we just need to know when it changes.


;; Update the existing functions so that objects are actually components which
;; contain a bounding box. Normally this would be a good use for a generic
;; method, but I don't want to pay for the dynamic dispatch which I think would
;; come with it. With that - we could add two functions, remove, and update
;; (implemented as remove and insert). From there, we can implement query
;; functions and a way to iterate over the tree (we already do this in
;; print-octree).







(define [update!! node]

  ; Update child nodes recursively. This is done first because children push
  ; objects upward, which we can then handle.

  (let ([active (node-active-children node)])
    (for ([i     (in-range 8)]
          [child (in-vector (node-child-nodes node))]
          #:when (bitwise-bit-set? active i))
      (println "Updating child node.")
      (update!! child)))

  (let ([parent (node-parent-node node) ]
        [bbox   (node-bounding-box node)])

    ; The parent node will be null when we reach the root node. Right now, we
    ; just stop, but at some point we will want to update the root node to be a
    ; larger box, of which the old root is an octant.

    (unless (null? parent)

      (for ([obj (node-objects node)])
        (if (contains? bbox obj)

            ;; FIXME: Figure out how we can aggregate these objects and do a
            ;; single insert.

            (insert!! node obj)
            (set-node-objects! parent (cons obj (node-objects parent))))))))

(define [print-node node]
  (println "-----------------------------")
  (let ([bb (node-bounding-box node)])
    (println (format "Node: (~a, ~a, ~a) - (~a, ~a, ~a)"
                     (vec3-x (bounding-box-min bb))
                     (vec3-y (bounding-box-min bb))
                     (vec3-z (bounding-box-min bb))
                     (vec3-x (bounding-box-max bb))
                     (vec3-y (bounding-box-max bb))
                     (vec3-z (bounding-box-max bb)))))
  (for ([obj (node-objects node)])
    (println (format "Object: (~a, ~a, ~a) - (~a, ~a, ~a)"
                      (vec3-x (bounding-box-min obj))
                      (vec3-y (bounding-box-min obj))
                      (vec3-z (bounding-box-min obj))
                      (vec3-x (bounding-box-max obj))
                      (vec3-y (bounding-box-max obj))
                      (vec3-z (bounding-box-max obj)))))
  (println (format "Active Children (bitmask): ~b" (node-active-children node)))
  (for ([ch (node-child-nodes node)]
        #:when (not (eq? 0 ch)))
    (print-node ch))
  (println "End Children -----------"))

(define [print-octree octree]
  (println "Octree: ")
  (print-node (octree-root-node octree)))

;; Test with:
(insert!
 (make-node (bounding-box (vec3 0 0 0) (vec3 10 10 10)) (list (bounding-box (vec3 1 1 1) (vec3 2 2 2))) #f)
 (bounding-box (vec3 1.3 1.3 1.3) (vec3 2.1 2.1 2.1)))
