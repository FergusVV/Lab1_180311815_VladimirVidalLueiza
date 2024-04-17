#lang racket

(require "pcar_180311815_vladimirvidallueiza.rkt")

(define train
  (lambda (id maker rail-type speed station-stay-time . pcars)
    (if (and (integer? id)                            ; Verifica que el ID sea un entero.
             (string? maker)                          ; Verifica que el fabricante sea una cadena de texto.
             (string? rail-type)                      ; Verifica que el tipo de riel sea una cadena de texto.
             (number? speed)                          ; Verifica que la velocidad sea un número.
             (number? station-stay-time)              ; Verifica que el tiempo de estancia sea un número.
             (positive? speed)                        ; Verifica que la velocidad sea positiva.
             (>= station-stay-time 0)                 ; Verifica que el tiempo de estancia no sea negativo.
             (if (null? pcars)                        ; Si no hay carros, omite verificaciones de carros.
                 #t
                 (and (consistent-model? pcars)       ; Verifica la consistencia del modelo entre todos los carros.
                      (valid-train-car-order? pcars)))) ; Verifica el orden correcto de los carros.
        (list id maker rail-type speed station-stay-time pcars)
        (raise "Invalid train configuration"))))

; Verifica que todos los carros tengan el mismo modelo. Funciona correctamente con lista vacía.
(define (consistent-model? pcars)
  (if (null? pcars)
      #t
      (let ((model (pcar-model (first pcars))))
        (andmap (lambda (pcar) (equal? (pcar-model pcar) model)) pcars))))

; Verifica que todos los carros intermedios sean de tipo central. Funciona correctamente con lista vacía.
(define (all-central-car-types? pcars)
  (if (null? pcars)
      #t
      (and (equal? (pcar-type (first pcars)) ct)
           (all-central-car-types? (rest-of-list pcars)))))

; Verifica el orden correcto de los carros en el tren. Funciona correctamente con lista vacía.
(define (valid-train-car-order? pcars)
  (or (null? pcars)  ; Si no hay carros, devuelve verdadero.
      (and (equal? (pcar-type (first pcars)) tr)  ; El primer carro debe ser terminal.
           (equal? (pcar-type (last pcars)) tr)  ; El último carro debe ser terminal.
           (all-central-car-types? (rest-of-list (reverse (rest-of-list (reverse pcars))))))))  ; Carros intermedios deben ser centrales.





; Función auxiliar para insertar un carro en una lista de carros en una posición específica.
(define (insert-car-at cars pcar position)
  (cond ((null? cars) (if (= position 0) (list pcar) (error "Posición fuera de rango")))
        ((= position 0) (cons pcar cars))
        (else (cons (car cars) (insert-car-at (cdr cars) pcar (- position 1))))))

; Función para añadir un carro a un tren en una posición dada.
(define (train-add-car train pcar position)
  (if (< position 0)
      (raise "Posición inválida")
      (make-train (train-id train)
                  (train-maker train)
                  (train-rail-type train)
                  (train-speed train)
                  (train-stay-time train)
                  (insert-car-at (train-cars train) pcar position))))

(define (make-train id maker rail-type speed stay-time cars)
  (list id maker rail-type speed stay-time cars))

; Funciones de acceso para los componentes del tren (se asumen definidas en el entorno).
(define (train-id train) (first train))
(define (train-maker train) (second train))
(define (train-rail-type train) (third train))
(define (train-speed train) (fourth train))
(define (train-stay-time train) (fifth train))
(define (train-cars train) (sixth train))




