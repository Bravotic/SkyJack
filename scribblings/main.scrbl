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
