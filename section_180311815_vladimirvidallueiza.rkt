#lang racket

(provide (all-defined-out))

(require "station_180311815_vladimirvidallueiza.rkt")


; TDA section.
; Representada por una lista con
; point1(station) X point2(station) X distance(positive number) X cost(non-negative number) 

;---------------------Constructor---------------------

; Crea una section entre dos estaciones con una distancia y un costo especificados.
; Dominio: point1(station) X point2(station) X distance(positive number) X cost(non-negative number)
; Recorrido: section
(define section
  (lambda (point1 point2 distance cost)
    (if (and (station? point1)  ; Verifica que point1 sea una estación válida.
             (station? point2)  ; Verifica que point2 sea una estación válida.
             (not (equal? (get-station-id point1) (get-station-id point2)))  ; Asegura que las estaciones no sean la misma.
             (number? distance)  ; Confirma que la distancia es un número.
             (> distance 0)       ; La distancia debe ser positiva.
             (number? cost)      ; Confirma que el costo es un número.
             (>= cost 0))        ; El costo no puede ser negativo.
        (list point1 point2 distance cost)  ; Devuelve una lista que representa la sección.
        (raise "Invalid section parameters"))))  ; Lanza un error si los parámetros son inválidos.

;---------------------Pertenencia---------------------

; Verifica si un objeto es section válida.
; Dominio: section
; Recorrido: boolean
(define (section? section)
  (and (list? section)  ; Verifica que la sección sea una lista.
       (= (length section) 4)  ; La sección debe tener exactamente cuatro componentes.
       (station? (get-section-point1 section))  ; El primer punto debe ser una estación válida.
       (station? (get-section-point2 section))  ; El segundo punto debe ser una estación válida.
       (number? (get-section-distance section))  ; La distancia debe ser un número.
       (> (get-section-distance section) 0)  ; La distancia debe ser positiva.
       (number? (get-section-cost section))  ; El costo debe ser un número.
       (>= (get-section-cost section) 0)))  ; El costo no puede ser negativo.

;---------------------Selectores---------------------

; get-section-point1: selecciona la estación inicial de section.
; Dominio: section
; Recorrido: station
(define (get-section-point1 section)
  (first-of-list section))

; get-section-point2: selecciona la estación final de una sección.
; Dominio: section
; Recorrido: station
(define (get-section-point2 section)
  (second-of-list section))

; get-section-distance: selecciona la distancia entre dos estaciones de section.
; Dominio: section
; Recorrido: positive number
(define (get-section-distance section)
  (third-of-list section))

; get-section-cost: selecciona el costo asociado con section.
; Dominio: section
; Recorrido: non-negative number
(define (get-section-cost section)
  (fourth-of-list section))
