#lang racket

;; Flight Planning DSL

;; Purpose
;; Allows you to create a flight plan consisting of
;; Airports, VOR/DME/NDB beacons, and waypoints
;; From this information lets you query information about
;; your path like radio frequencies, headings, or airspace clearance

;; Scope
;; Generating flight plans from just SRC,DST is not in scope

;; Concepts
;; data structures & typing
;; VOR, Airport, waypoints are all navigation points
;; but with different available information

;; Composition
;; Flight plan is a list of navigation points with indications of direct-to as infix
'(    KBOS
      D-> PSM
      APPR KPWM)
;; optional approach hold before destination



;; Source Data ---------------------------------
(struct airport (...))
(struct VOR (...))


;; Syntax
#;(define-airport
    airport: KBOS
    coordinates: 1234 1234
    other: ...
    )
#;(define-vor
    VOR: ...
    coordinates: 1234 1234
    other: ...
    )

#;
(define-aircraft my-cessna
  max-range: 2778
  ...
  )

#;(define-plan second-part
    (KPWM
     D-> ...))

#;(plan-route my-cessna
   KBOS
   D-> PSM
   D-> KPWM
   D-> second-part
   D-> (repeat 7 second-part)
   (pdf txt fuel radios))

#;(define Generator (-> file (-> Route Any)))
#;(define-generator (λ (route) (txt route)))

;; example
#;(define-plan boston-north-to-maine-part1
    (KBOS D-> PSM))
#;(define-plan boston-north-to-maine-part2
    (D-> KBGR))
#;(plan-route
   boston-north-to-maine-part1
   D-> boston-north-to-maine-part2)

;; Semantics
;; final route plan must start and end at an airport
;; all airports and navigation points exist and have been defined
;; route is not beyond airplane's capabilities
;; each next navigation points is reasonably close to previous navigation point
;; possibly warnings if next navigation point is almost too far
;; produce outputs in multiple forms (pdf, maps, text files)



;;; TODO
;; express syntax for defining symbols (beacons, airports, waypoints)
;; express syntax for defining plane capabilities
;; express static and runtime checks
;;   eg. distance between navigation points
;;   calculations for max range, fuel needed, etc
;;   ability to express and insert holding patterns
;;   ability to compose flight plans or small reroutes inside other flight plans


