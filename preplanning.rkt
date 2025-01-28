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


;;; TODO
;; express syntax for defining symbols (beacons, airports, waypoints)
;; express syntax for defining plane capabilities
;; look for data sources to import publically available data
;; express static and runtime checks
;;   eg. distance between navigation points
;;   calculations for max range, fuel needed, etc
;;   ability to express and insert holding patterns
;;   ability to compose flight plans or small reroutes inside other flight plans
