#lang racket

(require wizard
         "main.rkt"
         raylib
         (for-syntax racket/syntax
                     syntax/parse))

(provide (struct-out tooltip)
         (struct-out window-pos)
         (struct-out status-bar)
         (struct-out range)
         (struct-out gradient)
         (struct-out target)
         target-value
         draw/tooltips
         draw/statusbars)

(define/component window-pos (x y))
(define/component tooltip (text offset-x offset-y))
(define/component status-bar (text target gradient))

(struct range (current maximum) #:constructor-name $range)
(struct gradient (from-color to-color) #:constructor-name $gradient)
(struct target (eid value-fn) #:constructor-name $target)

(define-syntax [target-value stx]
  (syntax-parse stx
    [(_ comp:id field:id)
     #`(λ (eid)
         (let ([c (component:for-entity #,(format-id #'comp "struct:~a" #'comp) eid)])
           #,(with-syntax ([getter (format-id #'comp "~a-~a" #'comp #'field)])
               #`(#,(datum->syntax #'comp #'getter) c))))]))

(define [draw/tooltips]
  (let ([margin-x 5])
    (let/ecs ([(tooltip text ox oy) tooltip ]
              [(position eid  x  y) position])
             ;; Calculate the size of the text and build the appropriate
             ;; background. Optionally, we could make sure the background is a
             ;; multiple of the tile width (taking into account scaling factor).
             (let ([origin-x (+ (exact-truncate (* -1 (*cam-offset-x*)))
                                (* 10 (config-ref 'gfx-scale-x) x) ox)]
                   [origin-y (+ (exact-truncate (* -1 (*cam-offset-y*)))
                                (* 10 (config-ref 'gfx-scale-y) y) oy)]
                   [text-w   (measure-text text (* 10 (config-ref 'gfx-scale-x)))])
               (draw-Rect (make-Rect (+ 0. origin-x)
                                     (+ 0. origin-y)
                                     (+ 0. (* 2 margin-x) text-w)
                                     (* 10. (config-ref 'gfx-scale-y)))
                          DARKGRAY)
               (draw-text text
                          (+ margin-x origin-x)
                          origin-y
                          (* 10 (config-ref 'gfx-scale-x))
                          WIZARDWHITE)))))

(define [draw/statusbars]
  (let/ecs ([(status-bar text target gradient) status-bar]
            [(window-pos x y)                  window-pos])
           (let ([w    300]
                 [h     20]
                 [value ((target-value-fn target) (target-eid target))])
             (draw/box x y w h DARKGRAY #:px 5 #:py 2 #:fill-color (gradient-to-color gradient))
             (draw/box x y (* (/ (range-current value)
                                 (range-maximum value)) w) h
                       CLEAR #:px 5 #:py 2 #:fill-color (gradient-from-color gradient))
             ;; Draw the text label
             (draw-text (format text
                                (range-current value)
                                (range-maximum value))
                        ;; Draw in the middle I guess...
                        (+ x 30) ;; Where does this number even come from??
                        (+ y  h)
                        20
                        WIZARDWHITE))))

;; TODO: Implement grid-snap
(define [draw/box x y w h color
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
