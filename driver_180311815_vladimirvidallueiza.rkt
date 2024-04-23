#lang racket

(provide (all-defined-out))
(require "station_180311815_vladimirvidallueiza.rkt")

; TDA Driver.
; Representada por una lista con
; id (int) X nombre (string) X train-maker (string)


;---------------------Constructor de Driver---------------------
; driver: Crea un objeto 'driver' con los siguientes atributos validados.
; Dominio: id (int) X nombre (string) X train-maker (string)
; Recorrido: driver

(define (driver id name train-maker)
  (if (and (integer? id)                      ; Verifica que el ID sea un entero.
           (string? name)                     ; Verifica que el nombre sea una cadena.
           (string? train-maker))             ; Verifica que el fabricante del tren sea una cadena.
      (list id name train-maker)              ; Crea la estructura del conductor si todos los tipos son correctos.
      (raise "Invalid input types")))         ; Lanza un error si alguno de los tipos no es correcto.

;---------------------Función de Pertencia---------------------
; driver?: Evalúa si un objeto dado es un 'driver' válido basado en su estructura y tipos de datos.
; Dominio: driver
; Recorrido: Booleano
(define (driver? driver)
  (and (list? driver)                                  ; Debe ser una lista.
       (= (length driver) 3)                           ; Debe tener exactamente tres elementos.
       (integer? (get-driver-id driver))                   ; El ID debe ser un entero.
       (string? (get-driver-name driver))                  ; El nombre debe ser una cadena.
       (string? (get-driver-train-maker driver))))         ; El fabricante del tren debe ser una cadena.

;---------------------Selectores---------------------
; Estas funciones extraen componentes individuales de una estructura 'driver'.

; driver-id: Obtiene el ID de un 'driver'.
; Dominio: driver
; Recorrido: int
(define (get-driver-id driver) 
  (first-of-list driver))  ; Extrae el primer elemento: ID.

; driver-name: Obtiene el nombre de un 'driver'.
; Dominio: driver
; Recorrido: String
(define (get-driver-name driver) 
  (second-of-list driver))  ; Extrae el segundo elemento: nombre.

; driver-train-make: Obtiene el fabricante de tren asociado a un 'driver'.
; Dominio: driver - estructura de 'driver'.
; Recorrido: String - fabricante del tren.
(define (get-driver-train-maker driver) 
  (third-of-list driver))  ; Extrae el tercer elemento: fabricante del tren.
