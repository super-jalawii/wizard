#lang racket/base

(require wizard)

(provide of
         make-packet-from
         packet-transfer!
         packet-use!
         packet-empty?
         packet-has?)

(struct packet (resource [amount #:mutable])
  #:constructor-name $packet
  #:transparent)

(define (of amt res)
  ; A packet constructor designed to exploit the "inline"
  ; function-call syntax like:
  ;      (($gallons 3) . of . resource::petrol)
  ($packet res amt))

(define (make-packet-from pkt #:amount [amt 0])
  ($packet (packet-resource pkt)
           (if (and amt (uom? amt))
               amt
               (uom- (packet-amount pkt)
                     (packet-amount pkt)))))

(define (packet-transfer! from-pkt to-pkt #:upto [capacity #f])
  ; Move resources from one packet to another. If #:upto is specified, then
  ; resources will only be transferred up to the stated capacity. It is an error
  ; to call packet-transfer! on packets containing different types of resources.
  (unless (eq? (packet-resource from-pkt)
               (packet-resource to-pkt))
    (error "Packets must contain the same type of resource to participate in a transfer."))
  (let*-values ([(pkt1-amt pkt2-amt) (normalize (packet-amount from-pkt)
                                                (packet-amount to-pkt))]
                [(_ capacity) (normalize pkt1-amt
                                         (if capacity
                                             capacity
                                             (uom+ pkt1-amt pkt2-amt)))])
    (let* ([transfer-qty (max 0 (min (uom-value pkt1-amt)
                                     (uom-value (uom- capacity pkt2-amt))))]
           [transfer-amount (uom- pkt1-amt pkt1-amt)])
      (set-uom-value! transfer-amount transfer-qty)
      (set-packet-amount! from-pkt (uom- pkt1-amt transfer-amount))
      (set-packet-amount! to-pkt (uom+ pkt2-amt transfer-amount))
      from-pkt)))

(define (packet-empty? pkt)
  (eq? 0 (uom-value (packet-amount pkt))))

(define (packet-use! pkt amt)
  (set-packet-amount! pkt (uom- (packet-amount pkt) amt))
  ;  NOTE: Without normalization, we can only compare to zero, and even that's not
  ;  generally true (see farenheit and celsius)
  (when ((uom-value (packet-amount pkt)) . < . 0)
    (error "Attempted to extract more resources from packet than available."))
  pkt)

(define (packet-has? pkt amt)
  (let-values ([(pkt-amt amt) (normalize (packet-amount pkt) amt)])
    ((uom-value pkt-amt) . >= . (uom-value amt))))

(module+ test
  (require rackunit
           threading
           "unit.rkt")

  (define resource::petrol 'RESOURCE_PETROL)
  (define resource::water 'RESOURCE_WATER)

  (test-case
      "packet-transfer! : Transferring the contents of one packet to another with no upper limit"
    (let ([packet1 ($packet resource::petrol ($gallons 4))]
          [packet2 ($packet resource::petrol ($gallons 10))])
      (check-equal? (packet-amount (packet-transfer! packet1 packet2))
                    ($gallons 0)
                    "Not all of packet transferred?")
      (check-equal? (packet-amount packet1) ($gallons  0) "First packet not emptied?")
      (check-equal? (packet-amount packet2) ($gallons 14) "Second packet didn't receive all of transferred resources?")))

  (test-case
      "packet-transfer! : Transferring the contents of a packet to another up to a maximum amount"
    (let ([packet1 ($packet resource::petrol ($gallons 10))]
          [packet2 ($packet resource::petrol ($gallons 10))])
      (check-equal? (packet-amount (packet-transfer! packet1 packet2 #:upto ($gallons 15)))
                    ($gallons 5)
                    "Overflow packet doesn't contain the correct amount of resources?")
      (check-equal? (packet-amount packet1) ($gallons 5) "First packet emptied?")
      (check-equal? (packet-amount packet2) ($gallons 15) "Second packet not filled?")))

  (test-case
      "packet-transfer! : Transferring the contents of a packet to another when the second packet already exceeds the capacity"
    (let ([packet1 ($packet resource::petrol ($gallons 5))]
          [packet2 ($packet resource::petrol ($gallons 10))])
      (check-equal? (packet-amount (packet-transfer! packet1 packet2 #:upto ($gallons 7)))
                    ($gallons 5)
                    "Overflow packet received resources from truncation of second packet?")
      (check-equal? (packet-amount packet1) ($gallons 5) "First packet emptied?")
      (check-equal? (packet-amount packet2) ($gallons 10) "Second packet truncated by capacity?")))

  (test-case
      "packet-transfer! : Transferring contents of one packet to another should fail when the resources are of different types"
    (let ([packet1 ($packet resource::petrol ($gallons 10))]
          [packet2 ($packet resource::water ($gallons 10))])
      (check-exn exn:fail?
                 (λ () (packet-transfer! packet1 packet2))
                 "Heterogenous packets combined?")))


  (test-case "packet-emtpy? : Empty packet is empty"
    (check-true (packet-empty? ($packet resource::petrol ($gallons 0)))))

  (test-case "packet-empty? : Packet with resources is not empty"
    (check-false (packet-empty? ($packet resource::petrol ($gallons 1)))))

  (test-case "packet-use! : Packet has less resources after some are used"
    (check-equal? (packet-amount (packet-use! ($packet resource::petrol ($gallons 10))
                                            ($gallons 4)))
                 ($gallons 6)))

  (test-case "packet-use! : Error occurs when using more resource than a packet has"
    (check-exn exn:fail?
               (λ () (packet-use! ($packet resource::petrol ($gallons 10))
                                 ($gallons 15)))))

  (test-case "packet-has? : Packet has enough resources"
    (check-true (packet-has? ($packet resource::petrol ($gallons 4))
                             ($gallons 2))))

  (test-case "packet-has? : Packet has less resource than required"
    (check-false (packet-has? ($packet resource::petrol ($gallons 4))
                              ($gallons 6))))

  (test-case "packet-has? : Packet has exactly enough resources"
    (check-true (packet-has? ($packet resource::petrol ($gallons 4))
                             ($gallons 4))))

  )
