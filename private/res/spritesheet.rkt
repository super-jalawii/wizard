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

(define *asset-path*   (make-parameter "assets"))
(define *sprite-store* (make-parameter (make-hash)))

(struct Spritesheet (tex w h tw th))

(define/component Sprite (x y w h ts-handle))

(define [Sprite-by-idx ts-handle idx]
  (let* ([sprs (hash-ref (*sprite-store*) ts-handle)         ]
         [tw   (/ (Spritesheet-w sprs) (Spritesheet-tw sprs))]
         [x    (* (remainder idx tw)   (Spritesheet-tw sprs))]
         [y    (* (quotient  idx tw)   (Spritesheet-th sprs))])
    (Sprite x y
            (Spritesheet-tw sprs)
            (Spritesheet-th sprs)
            ts-handle)))

(define [get-spritesheet handle]
  (hash-ref (*sprite-store*) handle))

;; NOTE: DO NOT CALL THIS BEFORE INITIALIZING WINDOW.
(define [load-spritesheet filename handle w h tw th]
  (printf "Current Directory: ~a Loading Spritesheet From: ~a"
          (current-directory)
          (format "~a/~a" (*asset-path*) filename))
  (let* ([tex  (load-tex (format "~a/~a" (*asset-path*) filename))]
         [sprs (Spritesheet tex w h tw th)])
    (hash-set! (*sprite-store*) handle sprs)))

(define [unload-spritesheet handle]
  (unload-tex (Spritesheet-tex (hash-ref (*sprite-store*) handle))))

