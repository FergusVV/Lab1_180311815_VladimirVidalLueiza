#lang racket
(provide (all-defined-out))

;; Definición de símbolos para los tipos de carro.
(define tr 'tr)  ; Terminal: Estaciones finales de una línea y que pueden operar como combinaciones con otras líneas o servicios.
(define ct 'ct)  ; Central: Carros que operan en el centro del convoy.
; Renombrar las funciones selectoras de listas.
(define first-of-list car) ; Obtiene el primer elemento de una lista.
(define second-of-list second) ; Obtiene el segundo elemento de una lista.
(define third-of-list third) ; Obtiene el tercer elemento de una lista.
(define fourth-of-list fourth) ; Obtiene el cuarto elemento de una lista.
(define rest-of-list cdr) ; Elimina el primer elemento y devuelve el resto de la lista.

; TDA Pcar.
; Representada por una lista con
; id (int) X capacity (positive integer) X model (string) X type (car-type)

;---------------------Constructor---------------------

;; pcar:Constructor para un carro de pasajero.
;; Los carros pueden ser de tipo terminal (tr) o central (ct).
;; Dominio: id (int), capacity (positive integer), model (string), type (tr or ct).
;; Recorrido: pcar
(define pcar
  (lambda (id capacity model type)
    (if (and (integer? id)                    ; Verifica que el ID sea un entero.
             (positive? capacity)             ; Verifica que la capacidad sea un número positivo.
             (string? model)                  ; Verifica que el modelo sea una cadena de texto.
             (member type (list tr ct)))      ; Verifica que el tipo sea 'tr' o 'ct'.
        (list id capacity model type)         ; Crea y retorna una lista con los detalles del carro.
        (error "Invalid pcar parameters"))))  ; Lanza un error si algún parámetro no es válido.


;---------------------Pertenencia---------------------
;; Función de pertenencia para pcar.
;; Verifica que un objeto sea un carro de pasajero válido según la definición del constructor.
;; Dominio: pcar (lista).
;; Recorrido: booleano.
(define (pcar? pcar)
  (and (list? pcar)                           ; Verifica que pcar sea una lista.
       (= (length pcar) 4)                    ; Verifica que pcar tenga cuatro elementos.
       (integer? (get-pcar-id pcar))              ; Verifica que el ID sea un entero.
       (positive? (get-pcar-capacity pcar))       ; Verifica que la capacidad sea un número positivo.
       (string? (get-pcar-model pcar))            ; Verifica que el modelo sea una cadena de texto.
       (member (get-pcar-type pcar) (list tr ct))))  ; Verifica que el tipo sea 'tr' o 'ct'.

;---------------------Selectores---------------------

;; Selector para el ID del carro.
;; Dominio: pcar.
;; Recorrido: ID del carro (int).
(define (get-pcar-id pcar)
  (first-of-list pcar))

;; Selector para la capacidad del carro.
;; Dominio: pcar.
;; Recorrido: capacidad del carro (positive integer).
(define (get-pcar-capacity pcar)
  (second-of-list pcar))

;; Selector para el modelo del carro.
;; Dominio: pcar.
;; Recorrido: modelo del carro (string).
(define (get-pcar-model pcar)
  (third-of-list pcar))

;; Selector para el tipo del carro.
;; Dominio: pcar.
;; Recorrido: tipo del carro (tr o ct).
(define (get-pcar-type pcar)
  (fourth-of-list pcar))
