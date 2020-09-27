#lang racket

(require wizard
         "main.rkt"
         raylib
         (for-syntax racket/syntax
                     syntax/parse))

(provide (struct-out Tooltip)
         (struct-out WindowPos)
         (struct-out StatusBar)
         (struct-out Range)
         (struct-out Gradient)
         (struct-out Target)
         Target-value
         draw-tooltips
         draw-statusbars)

(define/component WindowPos (x y))
(define/component Tooltip   (text offset-x offset-y))
(define/component StatusBar (text target gradient))

(struct Range    (current maximum))
(struct Gradient (from-color to-color))
(struct Target   (eid value-fn))

(define-syntax [Target-value stx]
  (syntax-parse stx
    [(_ comp:id field:id)
     #`(λ (eid)
         (let ([c (Component-for-entity #,(format-id #'comp "struct:~a" #'comp) eid)])
           #,(with-syntax ([getter (format-id #'comp "~a-~a" #'comp #'field)])
               #`(#,(datum->syntax #'comp #'getter) c))))]))

(define [draw-tooltips]
  (let ([margin-x 5])
    (let/ecs ([(Tooltip text ox oy) Tooltip ]
              [(Position eid  x  y) Position])
             ;; Calculate the size of the text and build the appropriate
             ;; background. Optionally, we could make sure the background is a
             ;; multiple of the tile width (taking into account scaling factor).
             (let ([origin-x (+ (exact-truncate (* -1 (*cam-offset-x*)))
                                (* 10 (config:get 'gfx-scale-x) x) ox)]
                   [origin-y (+ (exact-truncate (* -1 (*cam-offset-y*)))
                                (* 10 (config:get 'gfx-scale-y) y) oy)]
                   [text-w   (measure-text text (* 10 (config:get 'gfx-scale-x)))])
               (draw-Rect (make-Rect (+ 0. origin-x)
                                     (+ 0. origin-y)
                                     (+ 0. (* 2 margin-x) text-w)
                                     (* 10. (config:get 'gfx-scale-y)))
                          DARKGRAY)
               (draw-text text
                          (+ margin-x origin-x)
                          origin-y
                          (* 10 (config:get 'gfx-scale-x))
                          WIZARDWHITE)))))

(define [draw-statusbars]
  (let/ecs ([(StatusBar text target gradient) StatusBar]
            [(WindowPos x y)                  WindowPos])
           (let ([w    300]
                 [h     20]
                 [value ((Target-value-fn target) (Target-eid target))])
             (draw-box x y w h DARKGRAY #:px 5 #:py 2 #:fill-color (Gradient-to-color gradient))
             (draw-box x y (* (/ (Range-current value)
                                 (Range-maximum value)) w) h
                       CLEAR #:px 5 #:py 2 #:fill-color (Gradient-from-color gradient))
             ;; Draw the text label
             (draw-text (format text
                                (Range-current value)
                                (Range-maximum value))
                        ;; Draw in the middle I guess...
                        (+ x 30) ;; Where does this number even come from??
                        (+ y  h)
                        20
                        WIZARDWHITE))))

;; TODO: Implement grid-snap
(define [draw-box x y w h color
                  #:mx [mx 0] #:my [my 0]
                  #:px [px 0] #:py [py 0]
                  #:grid-snap  [grid-snap #t]
                  #:fill-color [fill-color color]]
  (draw-rect (- x mx)       (- y my)
             (+ w (* 2 mx)) (+ h (* 2 my))
             color)
  (when (not (eq? fill-color color))
    (draw-rect (+ x px)       (+ y py)
               (- w (* 2 px)) (- h (* 2 py))
               fill-color)))
