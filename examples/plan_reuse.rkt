#lang racket

(require "../lib/spec.rkt" "../lib/text-generator.rkt")

(provide (all-defined-out))

(define-airport KBOS
  [size large]
  [radio
   ("D-ATIS" 135.0)
   ("Tower East" 132.225)
   ("Tower West" 128.8)
   ("Ground" 121.75)
   ("Clearance" 121.65)
   ("Ramp" 134.05)]
  [coordinates 42.36197 -71.0079]
  [country US]
  [elevation 20])
(define-airport KBGR
  [country US]
  [size medium]
  [radio
   ("D-ATIS" 127.75)
   ("Tower" 120.7)
   ("Ground" 121.9)
   ("Clearance" 135.9)]
  [coordinates 44.8074 -68.828102]
  [elevation 192])
(define-airport KCON
  [size medium]
  [radio
   ("Approach/Departure" 127.35)
   ("Clearance" 133.65)
   ("Bangor RDO" 122.3)]
  [coordinates 43.20270157 -71.50229645]
  [elevation 342]
  [country US])

(define-vor PSM
  [frequency 116.5]
  [power medium]
  [country US]
  [elevation 99]
  [coordinates 43.0844993591309 -70.8320007324219])
(define-vor ENE
  [frequency 117.1]
  [power high]
  [country US]
  [elevation 190]
  [coordinates 43.4257011413574 -70.6135025024414])
(define-vor AUG
  [frequency 111.4]
  [power medium]
  [country US]
  [elevation 349]
  [coordinates 44.3199996948242 -69.7966003417969])
(define-vor BGR
  [frequency 114.8]
  [power high]
  [country US]
  [elevation 359]
  [coordinates 44.8418006896973 -68.8740005493164])
(define-vor SFO
  [frequency 115.8]
  [power medium]
  [country US]
  [elevation 13]
  [coordinates 37.619499206543 -122.374000549316])
(define-vor CON
  [frequency 112.9]
  [power medium]
  [country US]
  [elevation 710]
  [coordinates 43.2197990417481 -71.5755004882813])

(define-plan boston-north-departure
  KBOS
  D-> PSM)

(define-plan boston-to-bangor
  boston-north-departure
  D-> ENE
  D-> AUG
  D-> BGR
  D-> KBGR)

(define-plan boston-to-concord
  boston-north-departure
  D-> CON
  D-> KCON)

(define make-report
  (make-text-report-generator
   [airport name ":\n"
            elevation " - " coordinates "\n"
            (radio "Clearance") "\n"
            "----------------------------------------------"]
   [vor name ":\n"
        country " - " power "\n"
        "----------------------------------------------"]))

(display (make-report boston-to-bangor))
(display "\n\n\n")
(display (make-report boston-to-concord))


