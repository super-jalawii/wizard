#lang racket

(require graph)

(require (except-in wizard contains?)
         "../core.rkt")

(provide (struct-out anatomy)
         maim
         human-graph)

(define/component anatomy (skeleton))

(define human-graph
  (unweighted-graph/undirected
   '((head neck)
     (neck chest)
     (chest gut)
     (gut pelvis)
     (pelvis groin)
     (pelvis left-theigh)
     (pelvis right-theigh)
     (left-theigh left-knee)
     (right-theigh right-knee)
     (left-knee left-calf)
     (right-knee right-calf)
     (left-calf left-ankle)
     (right-calf right-ankle)
     (left-ankle left-foot)
     (right-ankle right-foot)
     (left-foot left-toes)
     (right-foot right-toes)
     (chest left-shoulder)
     (chest right-shoulder)
     (left-shoulder left-upper-arm)
     (right-shoulder right-upper-arm)
     (left-upper-arm left-elbow)
     (right-upper-arm right-elbow)
     (left-elbow left-lower-arm)
     (right-elbow right-lower-arm)
     (left-lower-arm left-wrist)
     (right-lower-arm right-wrist)
     (left-wrist left-hand)
     (right-wrist right-hand)
     (left-eye head)
     (right-eye head)
     (nose head)
     (mouth head)
     (left-ear head)
     (right-ear head))))

(define (maim ent part)
  ;; Make a copy of the graph so we don't mess up the prototypical skeleton.
  (let* ([animal (component:for-entity struct:animal ent)]
         [skeleton (graph-copy (animal-skeleton animal))]
         [to-trunk (fewest-vertices-path skeleton part 'chest)]
         [second-to-last (cadr to-trunk)])

    (remove-edge! skeleton part second-to-last)

    ;; Figure out which graph is the body and which is the appendage.
    (let* ([segments (cc skeleton)]
           ;; Pretty sure theres a partition function we could use here...
           [trunk-segment (for/or ([seg (in-list segments)])
                            (if (member 'chest seg) seg #f))]
           #;[other-segment (for/or ([seg (in-list segments)])
                            (if (not (member 'chest seg)) seg #f))])

      ;; TODO: Severed body part should be created as a distinct entity.
      ;; Remove the separated bodyparts.
      (let* ([skeleton (move-vertices skeleton trunk-segment)])
        (set-animal-skeleton! animal skeleton)))))

(define (move-vertices g vs)
  (let ([new-graph (unweighted-graph/undirected '())])
    (for ([v (in-list vs)])
      ;; Create the vertex in our new graph, recreating each connection.
      (add-vertex! new-graph v)
      (map (λ (n) (add-edge! new-graph v n)) (get-neighbors g v))
      (remove-vertex! g v))
    new-graph))

