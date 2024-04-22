#lang racket
(provide (all-defined-out))

(require "station_180311815_vladimirvidallueiza.rkt")
(require "section_180311815_vladimirvidallueiza.rkt")

(define line
  (lambda (id name rail-type . sections)
    (if (and (integer? id)
             (string? name)
             (string? rail-type)
             (andmap is-section? sections))  ; Asegura que todos los elementos en 'sections' son secciones válidas.
        (list id name rail-type sections)  ; Envuelve 'sections' en una lista para mantener una estructura consistente.
       (raise "Invalid line parameters"))))

(define (es-line? line)
  (and (list? line)
       (integer? (get-line-id line))
       (string? (get-line-name line))
       (string? (get-line-rail-type line))
       (list? (get-line-sections line))  ; Asegura que las secciones son una lista.
       (andmap is-section? (get-line-sections line)))) 

(define get-line-id
  (lambda (line)
    (first line)))

(define get-line-name
  (lambda (line)
    (second line)))

(define get-line-rail-type
  (lambda (line)
    (third line)))

(define get-line-sections
  (lambda (line)
    (if (>= (length line) 4)
        (fourth line)
        '())))

;;otras funciones
;;line-length
;;comprueba si una linea es circular
(define (line-is-circular? line)
  (and (not (null? (get-line-sections line)))
       (equal? (get-station-id (get-section-point1 (first (get-line-sections line))))
               (get-station-id (get-section-point2 (last (get-line-sections line)))))))


(define (regular-line-length line)
  (apply + (map get-section-distance (get-line-sections line))))

(define (circular-line-length line)
  (apply + (map get-section-distance (get-line-sections line))))


(define (line-length line)
  (if (line-is-circular? line)
      (circular-line-length line)
      (regular-line-length line)))

;;Esta función helper es una función interna que utiliza recursión para acumular la distancia.
;;La variable acum es un indicador booleano que comienza como #f (falso) y se vuelve #t (verdadero) cuando encontramos la estación de inicio.
;;Esto nos permite saber si debemos sumar las distancias de las secciones subsecuentes o no.
;;La función helper se llama inicialmente con la lista de secciones de la línea y #f para indicar que todavía no estamos acumulando la distancia.
(define (line-section-length line start-station-name end-station-name)
  (define (calculate-distance sections acum)
    (cond
      ((null? sections) 0)  ; No hay más secciones para procesar.
      ((string=? (get-station-name (get-section-point2 (first sections))) end-station-name) ;; Encontró la estación de fin, sumar esta distancia si está acumulando.
       (if acum (get-section-distance (first sections)) 0))
      ((string=? (get-station-name (get-section-point1 (first sections))) start-station-name) ;; Encontró la estación de inicio, sumar esta distancia y las subsiguientes.
       (+ (get-section-distance (first sections)) 
          (calculate-distance (rest-of-list sections) #t)))
      (acum
       ;; Si ya se ha comenzado a acumular, sumar esta distancia.
       (+ (get-section-distance (first sections)) 
          (calculate-distance (rest-of-list sections) #t)))
      (else
       ;; Si no se ha encontrado ninguna estación relevante, o aún no se acumula, continuar con la siguiente sección.
       (calculate-distance (rest-of-list sections) acum))))
  (calculate-distance (get-line-sections line) #f))

;line-cost
(define (line-cost line)
  (define (calculate-total-cost sections)
    (if (null? sections)
        0  ; No hay más secciones, no hay costo para calcular.
        (+ (get-section-cost (first sections))  ; Sumar el costo de la sección actual.
           (calculate-total-cost (rest-of-list sections)))))  ; Recursivamente sumar el costo de las secciones restantes.
  (calculate-total-cost (get-line-sections line)))

;;;line-section-cost
(define (line-section-cost line start-station-name end-station-name)
  (define (calculate-cost sections accumulated-cost start-found)
    (if (null? sections)
        accumulated-cost  ; Retorna el costo acumulado, finalizando la recursión.
        (cond
          ((string=? (get-station-name (get-section-point2 (first sections))) end-station-name)
           (if start-found
               (+ accumulated-cost (get-section-cost (first sections)))  ; Agrega el costo final y retorna.
               (calculate-cost (rest-of-list sections) accumulated-cost start-found)))  ; No ha comenzado a acumular, sigue buscando.
          ((string=? (get-station-name (get-section-point1 (first sections))) start-station-name)
           (calculate-cost (rest-of-list sections) (+ accumulated-cost (get-section-cost (first sections))) #t))  ; Comienza a acumular.
          (start-found
           (calculate-cost (rest-of-list sections) (+ accumulated-cost (get-section-cost (first sections))) start-found))  ; Continúa acumulando.
          (else
           (calculate-cost (rest-of-list sections) accumulated-cost start-found)))))  ; No se acumula aún, sigue buscando.
  (calculate-cost (get-line-sections line) 0 #f))



;;line-add-section
(define crearline
  (lambda (id name rail-type sections)
    (if (and (integer? id)
             (string? name)
             (string? rail-type)
             ;;(is-section? sections)
             )  ; Asegura que todos los elementos en 'sections' son secciones válidas.
        (list id name rail-type sections)  ; Envuelve 'sections' en una lista para mantener una estructura consistente.
        (raise "Invalid line parameters crear line"))))

(define (line-add-section line section)
  (define (section-exists? sections section)
    (cond ((null? sections) #f)  ; No hay más secciones, la sección no está duplicada.
          ((equal? (car sections) section) #t)  ; Se encontró una sección duplicada.
          (else (section-exists? (cdr sections) section))))  ; Recursión para verificar el resto.

  (define (add-section sections section)
    (if (section-exists? sections section)
        sections  ; Si la sección está duplicada, retorna las secciones sin cambios.
        (append sections (list section))))  ; Añade la sección al final.

  (if (is-section? section)
      (crearline (get-line-id line)
            (get-line-name line)
            (get-line-rail-type line)
            (add-section (get-line-sections line) section))
      (error "Invalid section to add")))

;;Comprobar si es linea
(define (line? line)
  (and (list? line)  ; Debe ser una lista.
       (= (length line) 4)  ; Debe tener exactamente cuatro elementos.
       (integer? (get-line-id line))  ; El ID debe ser un entero.
       (string? (get-line-name line))  ; El nombre debe ser una cadena de caracteres.
       (string? (get-line-rail-type line))  ; El tipo de riel debe ser una cadena de caracteres.
       (list? (get-line-sections line))  ; Las secciones deben ser una lista.
       (not (null? (get-line-sections line)))  ; Asegura que haya al menos una sección.
       (or (line-is-circular? line)  ; La línea debe ser circular,
           (any-station-is-type-t (get-line-sections line)))  ; o debe tener al menos una estación tipo 't'.
       (all-sections-valid? (get-line-sections line))))  ; Todas las secciones deben ser válidas.

(define (any-station-is-type-t sections)
  (define (station-type-t? station)
    (equal? (get-station-type station) t))
  (ormap (lambda (section)
           (or (station-type-t? (get-section-point1 section))
               (station-type-t? (get-section-point2 section))))
         sections))

(define (all-sections-valid? sections)
  (if (null? sections)
      #t
      (and (is-section? (first sections))
           (all-sections-valid? (rest-of-list sections)))))






