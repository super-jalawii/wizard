#lang racket


(require raylib
         wizard)

(provide (struct-out Spritesheet)
         (struct-out Sprite)
         *asset-path*
         Sprite-by-idx
         get-spritesheet
         load-spritesheet
         unload-spritesheet)

(define *asset-path* (make-parameter "assets"))
(define *sprite-store* (make-parameter (make-hash)))

(struct Spritesheet (tex w h tw th))

;; src-rect is just an attempt at optimizing by not allocating as much during
;; the actual gameloop. I don't know if it does anything. Also, the src rect is
;; the same for every tile with that index - so it might be better to rethink
;; this anyway.
(define/component Sprite (x y w h ts src-rect))

(define [Sprite-by-idx ts-handle idx]
  (let* ([sprs (hash-ref (*sprite-store*) ts-handle)         ]
         [tw   (/ (Spritesheet-w sprs) (Spritesheet-tw sprs))]
         [x    (* (remainder idx tw)   (Spritesheet-tw sprs))]
         [y    (* (quotient  idx tw)   (Spritesheet-th sprs))])
    (Sprite x y
            (Spritesheet-tw sprs)
            (Spritesheet-th sprs)
            (Spritesheet-tex sprs)
            (make-Rect (+ 0. x) (+ 0. y)
                       (+ 0. (Spritesheet-tw sprs))
                       (+ 0. (Spritesheet-th sprs))))))

(define [get-spritesheet handle]
  (hash-ref (*sprite-store*) handle))

;; NOTE: DO NOT CALL THIS BEFORE INITIALIZING WINDOW.
(define [load-spritesheet filename handle w h tw th]
  (printf "Current Directory: ~a~nLoading Spritesheet From: ~a"
          (current-directory)
          (format "~a/~a" (*asset-path*) filename))
  (let* ([tex  (load-tex (format "~a/~a" (*asset-path*) filename))]
         [sprs (Spritesheet tex w h tw th)])
    (hash-set! (*sprite-store*) handle sprs)))

(define [unload-spritesheet handle]
  (unload-tex (Spritesheet-tex (hash-ref (*sprite-store*) handle))))

