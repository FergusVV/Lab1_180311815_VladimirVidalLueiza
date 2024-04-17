#lang racket

(provide pcar)
(provide tr)
(provide ct)
(provide pcar-model)
(provide pcar-type)
(provide rest-of-list)
(define rest-of-list cdr)

(define tr 'tr)  ; Definicion previa como un simbolo
(define ct 'ct)

; Define el constructor para un carro de pasajero
;Los carros pueden ser de ti poterminal (tr) o central (ct)
(define pcar
  (lambda (id capacity model type)
    (if (and (integer? id)                   ; Verifica que el ID sea un entero.
             (positive? capacity)            ; Verifica que la capacidad sea un número positivo.
             (string? model)                 ; Verifica que el modelo sea una cadena de texto.
             (member type (list tr ct)))     ; Verifica que el tipo sea 'tr' o 'ct', usando símbolos definidos.
        (list id capacity model type)        ; Crea una lista con los detalles del carro.
        (error "Invalid pcar parameters")))) ; Lanza un error si algún parámetro no es válido.


(define (pcar-model pcar)
  (third pcar))  ; Asume que el modelo está en la tercera posición.

; Define la función para extraer el tipo de un carro.
(define (pcar-type pcar)
  (fourth pcar))