#lang racket

(define r 'r)  ; Definición previa como un símbolo.
(define m 'm)
(define c 'c)
(define t 't)

(define (station id name type stop-time)
  (if (and (integer? id)
           (string? name)
           (member type '(r m c t))  ; Asume que type será una variable ya definida como símbolo.
           (integer? stop-time)
           (> stop-time 0))
      (list id name type stop-time)
      (error "Parámetros inválidos de estación")))