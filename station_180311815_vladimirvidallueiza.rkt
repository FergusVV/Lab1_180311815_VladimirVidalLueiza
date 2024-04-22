#lang racket

(provide (all-defined-out))

; Renombrar las funciones selectoras de listas.
(define first-of-list car) ; Obtiene el primer elemento de una lista.
(define second-of-list second) ; Obtiene el segundo elemento de una lista.
(define third-of-list third) ; Obtiene el tercer elemento de una lista.
(define fourth-of-list fourth) ; Obtiene el cuarto elemento de una lista.
(define rest-of-list cdr) ; Elimina el primer elemento y devuelve el resto de la lista.

; Definición previa como un símbolo para el manejo de los tipos de estación.
(define r 'r) ; Regular (r): solo para abordar y dejar pasajeros.
(define m 'm) ; Mantenimiento (m): donde se realizan mantenimientos de trenes.
(define c 'c) ; Combinación (c): que permite cambiar a otras líneas o servicios de transporte externo.
(define t 't) ; Terminal (t): estaciones finales de una línea y que pueden operar como combinaciones con otras líneas o servicios de transporte externo.

; TDA station.
; Representada por una lista con
; Id(int) X name(String) X type(station-type) X stop-time(positive integer)  

;---------------------Constructor---------------------

; station: función que crea una estación a partir de su Id, name, type y stop-time.
; Dominio: Id(int) X name(String) X type(station-type) X stop-time(positive integer) 
; Recorrido: station
(define station (lambda (id name type stop-time)
  (if (and (integer? id)
           (string? name)
           (member type '(r m c t))  ;Se utiliza en este contexto para verificar que el tipo de station (type)
           (integer? stop-time)
           (> stop-time 0))
      (list id name type stop-time)
      (raise "Invalid Station Parameters"))))

;---------------------Pertenencia---------------------

; station?: comprueba que un listado de elementos cumpla con el formato de station.
; Dominio: station
; Recorrido: boolean

(define station? (lambda (station)
  (and (list? station)                         ; Debe ser una lista.
       (= (length station) 4)                  ; Debe tener exactamente cuatro elementos.
       (integer? (get-station-id station))     ; El id debe ser un entero.
       (string? (get-station-name station))    ; El nombre debe ser una cadena.
       (member (get-station-type station) '(r m c t))  ; El tipo debe ser uno de los símbolos válidos.
       (integer? (get-station-stop-time station))      ; El tiempo de parada debe ser un entero.
       (> (get-station-stop-time station) 0))))        ; El tiempo de parada debe ser positivo.

;---------------------Selectores---------------------

; get-station-id: selecciona el Id de la estación.
; Dominio: station
; Recorrido: Id (int)

(define (get-station-id st)
  (first-of-list st))

; get-station-name: selecciona el nombre de la estación.
; Dominio: station
; Recorrido: name (string)

(define (get-station-name st)
  (second-of-list st))

; get-station-type: selecciona el tipo de la estación.
; Dominio: station
; Recorrido: type (station-type)

(define (get-station-type st)
  (third-of-list st))

; get-station-stop-time: selecciona el tiempo de parada de la estación.
; Dominio: station
; Recorrido: stop-time (positive integer)

(define (get-station-stop-time st)
  (fourth-of-list st))

