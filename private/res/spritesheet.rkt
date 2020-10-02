#lang racket


(require raylib
         wizard)

(provide (struct-out spritesheet)
         (struct-out sprite)
         *asset-path*
         sprite-by-idx
         get-spritesheet
         load-spritesheet
         unload-spritesheet)

(define *asset-path* (make-parameter "assets"))
(define *sprite-store* (make-parameter (make-hash)))

(struct spritesheet (tex w h tw th) #:constructor-name $spritesheet)

;; src-rect is just an attempt at optimizing by not allocating as much during
;; the actual gameloop. I don't know if it does anything. Also, the src rect is
;; the same for every tile with that index - so it might be better to rethink
;; this anyway.
(define/component sprite (x y w h ts src-rect))

(define [sprite-by-idx ts-handle idx]
  (let* ([sprs (hash-ref (*sprite-store*) ts-handle)         ]
         [tw   (/ (spritesheet-w sprs) (spritesheet-tw sprs))]
         [x    (* (remainder idx tw)   (spritesheet-tw sprs))]
         [y    (* (quotient  idx tw)   (spritesheet-th sprs))])
    (@sprite x y
            (spritesheet-tw sprs)
            (spritesheet-th sprs)
            (spritesheet-tex sprs)
            (make-Rect (+ 0. x) (+ 0. y)
                       (+ 0. (spritesheet-tw sprs))
                       (+ 0. (spritesheet-th sprs))))))

(define [get-spritesheet handle]
  (hash-ref (*sprite-store*) handle))

;; WARN: DO NOT CALL THIS BEFORE INITIALIZING WINDOW.
(define [load-spritesheet filename handle w h tw th]
  (printf "Current Directory: ~a~nLoading Spritesheet From: ~a"
          (current-directory)
          (format "~a/~a" (*asset-path*) filename))
  (let* ([tex  (load-tex (format "~a/~a" (*asset-path*) filename))]
         [sprs ($spritesheet tex w h tw th)])
    (hash-set! (*sprite-store*) handle sprs)))

(define [unload-spritesheet handle]
  (unload-tex (spritesheet-tex (hash-ref (*sprite-store*) handle))))

