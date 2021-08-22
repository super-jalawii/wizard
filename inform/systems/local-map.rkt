#lang racket

(require graph
         (except-in wizard contains?))
(require "../core.rkt"
         "map.rkt")

(provide (struct-out LocalMapTile)

         *current-local-map*

         grid-set!
         grid-get
         grid-dimensions

         local-map/set!
         local-map/get

         add-tag!
         remove-tag!
         has-tag?

         build-local-map
         build-wall
         build-exit

         find-open-spot

         print/local-map)

(struct LocalMapTile (solid opaque tags) #:mutable)

(define (grid-dimensions g)
  (let ([x-dim (hash-count g)]
        [y-dim (hash-count (hash-ref g 0 (make-hash)))])
    (values x-dim y-dim)))

(define (grid-set! g x y v)
  (unless (hash-ref g x #f)
    (hash-set! g x (make-hash)))
  (hash-set! (hash-ref g x (make-hash)) y v))

(define (grid-get g x y [v #f])
  (hash-ref (hash-ref g x (make-hash)) y v))

(define (local-map/set! x y v [m (*current-local-map*)])
  (grid-set! m x y v))

(define (local-map/get x y [v #f] [m (*current-local-map*)])
  (grid-get m x y v))

(define *current-local-map* (make-parameter #f))

;; Given a map m (a hash table of hash tables), and a direction d, make all of
;; the tiles on the corresponding side opaque.
(define (build-wall d [m (*current-local-map*)])
  (let* ([x-dim (hash-count m)]
         [y-dim (hash-count (hash-ref m (car (hash-keys m))))]
         [ranges (case d
                   ['north (cons 0 (in-range x-dim))]
                   ['south (cons (- y-dim 1) (in-range x-dim))]
                   ['east  (cons (- x-dim 1) (in-range y-dim))]
                   ['west  (cons 0 (in-range y-dim))])])
    (for ([a (cdr ranges)])
      (let ([x (if (member d '(north south)) a (car ranges))]
            [y (if (member d '(east west)) a (car ranges))])
        (set-LocalMapTile-solid! (grid-get m x y) #t)
        (set-LocalMapTile-opaque! (grid-get m x y) #t)))))

(define (build-exit d [m (*current-local-map*)])
  (let* ([x-dim (hash-count m)]
         [y-dim (hash-count (hash-ref m (car (hash-keys m))))]
         [x-mid (+ (quotient x-dim 2) 0)]
         [y-mid (+ (quotient y-dim 2) 0)])
    (let-values ([(x y) (case d
                          ['north (values x-mid 0)]
                          ['south (values x-mid (- x-dim 1))]
                          ['east  (values (- x-dim 1) y-mid)]
                          ['west  (values 0 y-mid)])])
      (let ([tile (grid-get m x y)])
        (set-LocalMapTile-solid! tile #f)
        (set-LocalMapTile-opaque! tile #f)

        ;; Add the exit tag to this location.
        (add-tag! 'exit tile)))))

(define (add-tag! t map-tile)
  (unless (member t (LocalMapTile-tags map-tile))
    (set-LocalMapTile-tags! map-tile
                            (cons t (LocalMapTile-tags map-tile)))))

(define (has-tag? t map-tile)
  (member t (LocalMapTile-tags map-tile)))

(define (remove-tag! t map-tile)
  (set-LocalMapTile-tags! (remove t map-tile)))

(define (build-local-map area)
  (let ([local-map (make-hash)])
    (for* ([x (in-range 31)]
           [y (in-range 31)])
        (grid-set! local-map x y (LocalMapTile #f #f '())))

    ;; Annotate Exits and Build Walls on Non-Exit sides.
    (for ([d (in-list '(north south east west))])
      (build-wall d local-map))
    (for ([e (room/exits area)])
      (build-exit e local-map))

    local-map))

(define (todo! msg) (error msg))

;; Generate trees for the forest area - we have to place them, which means we
;; need to identify spots where trees can go. We also need to indicate the
;; footprint and graphics used.


;; FIXME: The math is wrong on these. Figure out what abstractions would make
;; this clearer.
(define (find-open-spot
         [r 1] ;; radius
         [p 0] ;; padding (margin)
         [m (*current-local-map*)]
         #:open-pred [fn (λ (t) (not (LocalMapTile-solid t)))])

  (let ([r (+ r p)])
    (let-values ([(x-dim y-dim) (grid-dimensions m)])
      (for*/or ([x (in-range (- x-dim (sub1 r)))]
                [y (in-range (- y-dim (sub1 r)))])
        (open-spot? x y (- r p) p m #:open-pred fn)))))

(define (random-spot
         [r 1] [p 0]
         [m (*current-local-map*)])
  (let-values ([(x-dim y-dim) (grid-dimensions m)])
    (let try-again ([x (random p (- x-dim (sub1 r)))]
                    [y (random 0 (- y-dim (sub1 r)))])
      (let ([spot (open-spot? x y r p m)])
        (if spot spot (try-again (random 0 x-dim)
                                 (random 0 y-dim)))))))

(define (open-spot?
         x y
         [r 1] [p 0]
         [m (*current-local-map*)]
         #:open-pred [fn (λ (t) (not (LocalMapTile-solid t)))])
  (let ([r (+ r p)])
    (if (for*/and ([x2 (in-range x (- r x))]
                   [y2 (in-range y (- r y))])
          (fn (grid-get m (+ x x2) (+ y y2))))
        (cons (+ p x) (+ p y))
        #f)))

(define (print/local-map [m (*current-local-map*)])

  ;; TODO: Draw the player, other actors, furniture / containers, etc.

  (let-values ([(x-dim y-dim) (grid-dimensions m)])
    (for* ([x (in-range x-dim)]
           [y (in-range y-dim)])
      (when (eq? y 0) (printf "~n"))
      (let ([tile (grid-get m x y)])
        (printf "~a"
                (cond
                  [(has-tag? 'exit tile)     "x "]
                  [(LocalMapTile-solid tile) "# "]
                  [else                      ". "]))))))

