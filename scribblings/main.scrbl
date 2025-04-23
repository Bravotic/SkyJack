#lang scribble/manual

@(require scribble/example
          (for-syntax racket/base)
          (for-label racket skyjack/lib/spec skyjack/lib/runtime-data skyjack/lib/runtime-functions))

@(define eval (make-base-eval '(require racket skyjack/lib/spec skyjack/lib/runtime-data)))

@title{SkyJack: A language for flight plans}
@author["Alexander Chang-Davidson, Collin McKinley"]

@defmodule[skyjack]

SkyJack is a programming language made specifically for representing flight
plans using VORs and Airports. It allows flight plans to be generated
programmatically and leverages to power of Racket's macro system to verify the
validitiy of flight plans in real time. Once the flight plan is created, SkyJack
gives you the tools to create a variety of reports using the data.

In effect, the SkyJack can be split up into 3 independent parts: defining
airports and VORs, flight planning, and report generation.

@table-of-contents[]

@section["Defining VORs and Airports"]

Before you can use VORs and Airports in flight plans, you must first define
them. Once defined, the same VOR or Airport can be used as many times as needed
in any of the code which follows it. VORs and airports can also be required from
other source files or libraries.

@defform[#:literals [coordinates elevation country size radio]
         (define-airport nav-id
           [coordinates latitude longitude]
           [elevation elevation-in-feet]
           [country country-code]
           [size airport-size]
           [radio
            (channel-name frequency) ...])
         #:contracts ([nav-id nav-id/c]
                      [latitude latitude/c]
                      [longitude longitude/c]
                      [country-code country/c]
                      [airport-size size/c]
                      [channel-name string?]
                      [frequency airport-freq/c])]{Defines an airport using the following information commonly found on the
  airport chart. Once defined, the airport can be referenced using its
                                                    @racket[nav-id].}

The following is an example of an airport definition for @link["https://www.flightaware.com/resources/airport/BOS/APD/AIRPORT+DIAGRAM/pdf"]{General Edward
Lawrence Logan International Airport} following the information from its chart.

@racketblock[
(define-airport KBOS
  [coordinates 42.36197 -71.0079]
  [elevation 20]
  [country US]
  [size large]
  [radio
   ("D-ATIS" 135.0)
   ("Tower East" 132.225)
   ("Tower West" 128.8)
   ("Ground" 121.75)
   ("Clearance" 121.65)
   ("Ramp" 134.05)])]


@defform[#:literals [coordinates elevation country frequency power]
         (define-vor nav-id
           [coordinates latitude longitude]
           [elevation elevation-in-feet]
           [country country-code]
           [frequency vor-freq]
           [power vor-power])
         #:contracts ([nav-id nav-id/c]
                      [latitude latitude/c]
                      [longitude longitude/c]
                      [country-code country/c]
                      [vor-freq vor-freq/c]
                      [vor-power power/c]
                      )]{Defines a VOR using information also commonly found on
                          its chart. Once defined it can also be referenced
                          using its @racket[nav-id].}

The following is the defition for the PVD VOR.

@racketblock[
(define-vor PVD
  [coordinates 41.7243003845215 -71.4296035766602]
  [elevation 49]
  [country US]
  [frequency 115.6]
  [power high])]

@section["Flight planning"]

After defining airports and VORs, they can be chained together to create a
flight plan. Flight plans can be created using the @racket[define-plan]
function, which allows flgiht plans to be associated with a name.

@defform[#:literals [D->]
         (define-plan nav-id
           navigable
           D-> navigable ...
           D-> navigable)
         #:grammar
         [(navigable flight-plan/c
                       airport/c
                       vor/c)]]{Defines a flight plan using previous flight
                                 plans, airports, or VORs as waypoints along the
                                 route.}

The following is an example of a flight plan starting in Boston, MA and ending in New York City, NY:

@racketblock[
(define-plan boston-to-new-york-city
  KBOS
  D-> PVD
  D-> ORW
  D-> CCC
  D-> KJFK)]

Since flight plans can contain other flight plans, similar portions of plans can be defined and later reused. The following is an example of plan reuse for a Boston north departure:

@racketblock[
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
]

@section["Report generation"]

After making a flight plan, SkyJack gives you the tools to generate textual reports of the flight plan.

@defform[#:literals (airport vor name coordinates latitude longitude elevation country size radio freq power)
         (make-text-report-generator
          [airport
           format-string ...]
          [vor
           format-string ...])
         #:grammar
         [(format-string name
                         coordinates
                         latitude
                         longitude
                         elevation
                         country
                         size
                         radio
                         (radio channel-name)
                         freq
                         power
                         string?)]]{Creates a generator which applies the given format to a given plan. Returns a function which
                                            takes one argument and returns a string.}

The following is a basic example of a flight plan text report generator:

@racketblock[
(define basic-report
  (make-text-report-generator
   [airport name ":\n"
            elevation " - " coordinates "\n"
            (radio "Clearance") "\n"
            "----------------------------------------------"]
   [vor name ":\n"
        country " - " power "\n"
        "----------------------------------------------"]))
]

Once defined, the report generator can be used as follows:

@racketblock[
(display (basic-report boston-to-bangor))
(display "\n\n\n")
(display (basic-report boston-to-concord))
]
