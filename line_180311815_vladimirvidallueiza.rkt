#lang racket
(provide (all-defined-out))

(require "station_180311815_vladimirvidallueiza.rkt")
(require "section_180311815_vladimirvidallueiza.rkt")

; TDA Line.
; Representada por una lista con
; id (int) X name (string) X rail-type(string)Xsection* (* señala que se pueden agregar 0 o más tramos)

;---------------------Constructor---------------------
; Crea una línea de metro con un identificador, nombre, tipo de riel y una lista opcional de secciones.
; Dominio: iid (int) X name (string) X rail-type(string) X section  * (* señala que se pueden agregar 0 o más tramos)
; Recorrido: line
(define line
  (lambda (id name rail-type . sections)
    (if (and (integer? id)  ; Verifica que el id sea un entero.
             (string? name)  ; Verifica que name sea una cadena de texto.
             (string? rail-type)  ; Verifica que rail-type sea una cadena de texto.
             (andmap section? sections))  ; Asegura que todos los elementos en 'sections' son secciones válidas.
        (list id name rail-type sections)  ; Construye la línea con las secciones envueltas en una lista.
        (raise "Invalid line parameters"))))

;---------------------Pertenencia---------------------
; Verifica si un objeto es una línea válida.
; Dominio: line
; Recorrido: boolean
(define (line? line)
  (and (list? line)  ; Debe ser una lista.
       (= (length line) 4)  ; Debe tener exactamente cuatro elementos.
       (integer? (get-line-id line))  ; El ID debe ser un entero.
       (string? (get-line-name line))  ; El nombre debe ser una cadena de caracteres.
       (string? (get-line-rail-type line))  ; El tipo de riel debe ser una cadena de caracteres.
       (list? (get-line-sections line))  ; Las secciones deben ser una lista.
       (not (null? (get-line-sections line)))  ; Asegura que haya al menos una sección.
       (or (line-is-circular? line)  ; La línea debe ser circular,
           (any-station-is-type-t (get-line-sections line)))  ; o debe tener al menos una estación tipo t.
       (all-sections-valid? (get-line-sections line))))  ; Todas las secciones deben ser válidas.

;; Define una función para verificar si al menos una estación en las secciones de una línea es del tipo t.
(define (any-station-is-type-t sections)
  (define (station-type-t? station) ;; Define una función auxiliar que verifica si el tipo de una estación es t
    (equal? (get-station-type station) t)) ;; Compara el tipo de la estación con t.
  (ormap (lambda (section) ;; Aplica ormap para procesar cada sección y devuelve #t si alguna estación es del tipo t.
           (or (station-type-t? (get-section-point1 section)) ;; Usa un or para verificar si alguno de los puntos de la sección es del tipo t.
               (station-type-t? (get-section-point2 section))))
         sections))
;; Define una función para verificar que todas las secciones de una lista son válidas.
(define (all-sections-valid? sections)
  (if (null? sections)
      #t
      (and (section? (first sections))
           (all-sections-valid? (rest-of-list sections)))))

;---------------------Selectores---------------------

; get-line-id: Devuelve el ID de la línea.
; Dominio: line
; Recorrido: int
(define get-line-id
  (lambda (line)
    (first-of-list line)))

; get-line-name: Devuelve el nombre de la línea.
; Dominio: line
; Recorrido: string
(define get-line-name
  (lambda (line)
    (second-of-list line)))

; get-line-rail-type: Devuelve el tipo de riel de la línea.
; Dominio: line
; Recorrido: string
(define get-line-rail-type
  (lambda (line)
    (third-of-list line)))

; get-line-sections: Devuelve las secciones de la línea, o una lista vacía si no hay secciones.
; Dominio: line
; Recorrido: list of section
(define get-line-sections
  (lambda (line)
    (if (>= (length line) 4)
        (fourth-of-list line)
        '())))

;---------------------Otras Funciones---------------------
; line-length: Calcula la longitud total de una línea, diferenciando entre líneas circulares y no circulares.
; No se usa recursividad directamente uso de apply para hacerlo declarativo
; Dominio: line
; Recorrido: positive-number
(define (line-length line)
  (if (line-is-circular? line)
      (apply + (map get-section-distance (get-line-sections line)))
      (apply + (map get-section-distance (get-line-sections line)))))

; Función para verificar si una línea es circular.
; Verifica que la primera estación de la primera sección y la última estación de la última sección sean las mismas.
; Dominio: line
; Recorrido: boolean
(define (line-is-circular? line)
  (and (not (null? (get-line-sections line)))
       (equal? (get-station-id (get-section-point1 (first (get-line-sections line))))
               (get-station-id (get-section-point2 (last (get-line-sections line)))))))


;------------------------------------------

; line-section-length: calcular la longitud de la sección de una línea entre dos estaciones dadas.
; Dominio: line (line) X station1-name (String) X station2-name (String)
; Recorrido: positive-number
; Uso de recursión
(define (line-section-length line start-station-name end-station-name)
  ;; Función recursiva interna para calcular la distancia.
  ;; sections es la lista de secciones de la línea, y acum es un booleano para indicar si estamos sumando distancias.
  (define (calculate-distance sections acum)
    ;; Caso base: si la lista de secciones está vacía, retorna 0.
    (if (null? sections)
        0
        ;; Evalúa la primera sección de la lista.
        (cond
          ;; Si la segunda estación de la sección actual es la estación final, verifica si estamos acumulando.
          ((string=? (get-station-name (get-section-point2 (first-of-list sections))) end-station-name)
           ;; Si acum es verdadero, suma la distancia de esta sección; si no, retorna 0.
           (if acum
               (get-section-distance (first-of-list sections))
               0))
          ;; Si la primera estación de la sección actual es la estación de inicio o ya estamos acumulando,
          ;; suma la distancia de esta sección y continua con las siguientes secciones.
          ((or (string=? (get-station-name (get-section-point1 (first-of-list sections))) start-station-name)
               acum)
           (+ (get-section-distance (first-of-list sections))
              (calculate-distance (rest-of-list sections) #t)))
          ;; En cualquier otro caso, simplemente continúa con las siguientes secciones sin acumular.
          (else
           (calculate-distance (rest-of-list sections) acum)))))
  ;; Inicia la función recursiva con la lista completa de secciones de la línea y `#f` indicando que aún no se acumula distancia.
  (calculate-distance (get-line-sections line) #f))

;------------------------------------------
; line-cost : Calcula el costo total de todas las secciones de una línea.
; Dominio: line
; Recorrido: number
; Recursión natural porque estan pendientes en cada iteración.
(define (line-cost line)
  (define (calculate-total-cost sections)
    (if (null? sections)
        0
        (+ (get-section-cost (first sections))
           (calculate-total-cost (rest-of-list sections)))))
  (calculate-total-cost (get-line-sections line)))

;------------------------------------------

; line-section-cost: Función para calcular el costo de una sección específica entre dos estaciones dentro de una línea.
; Dominio: line X start-station-name(string) X end-station-name(string)
; Recorrido: positive-number U {0}
; Uso de recursíon de cola ya que no deja estados pendiente en cada llamado
;; Define una función para calcular el costo acumulado de viajar entre dos estaciones en una línea.
(define (line-section-cost line start-station-name end-station-name)
  ;; Función interna recursiva de cola para calcular el costo.
  ;; Utiliza acum-cost para mantener el costo acumulado y start-found para indicar si la estación de inicio ha sido encontrada.
  (define (calculate-cost sections acum start-found)
    (if (null? sections)
        acum  ; Si no hay más secciones, retorna el costo acumulado.
        (cond
          ;; Si se encuentra la estación final y la estación de inicio ha sido encontrada.
          ((and start-found (string=? (get-station-name (get-section-point2 (first-of-list sections))) end-station-name))
           (+ acum (get-section-cost (first sections))))  ; Suma y retorna el costo de esta última sección.
          ;; Si se encuentra la estación de inicio o si ya se ha comenzado a acumular.
          ((or (string=? (get-station-name (get-section-point1 (first-of-list sections))) start-station-name)
               start-found)
           (calculate-cost (rest-of-list sections) (+ acum (get-section-cost (first sections))) #t))  ; Continúa con el costo actualizado.
          ;; Continúa sin actualizar el costo acumulado si no se han encontrado condiciones relevantes.
          (else
           (calculate-cost (rest-of-list sections) acum start-found)))))
  ;; Inicia la función recursiva con la lista completa de secciones de la línea, un costo acumulado de 0 y sin haber encontrado la estación de inicio.
  (calculate-cost (get-line-sections line) 0 #f))


;------------------------------------------

; Función line-add-section: Añade una sección a una línea si no está ya presente.
; Dominio: line X section
; Recorrido: line
; Recursión natural
(define (line-add-section line section)
  ;; Primero verifica si el argumento section es de tipo válido usando una función section? no definida aquí.
  (if (not (section? section))
      ;; Si section no es una sección válida, lanza un error.
      (raise "Invalid section to add")
      ;; Si es válida, procede a construir una nueva lista que represente la línea actualizada.
      (list (get-line-id line)           ;; Obtiene el identificador de la línea.
            (get-line-name line)         ;; Obtiene el nombre de la línea.
            (get-line-rail-type line)    ;; Obtiene el tipo de riel de la línea.
            ;; Usa una función interna add-if-not-exists para agregar la sección si no existe previamente.
            (let add-if-not-exists ((sections (get-line-sections line)))
              (cond ((null? sections) (list section))    ;; Si no hay secciones, crea una lista con la nueva sección.
                    ((equal? (first-of-list sections) section) sections) ;; Si la primera sección es igual a la nueva, no hace cambios.
                    (else (cons (first-of-list sections)  ;; De lo contrario, conserva la primera sección y revisa el resto.
                                (add-if-not-exists (rest-of-list sections)))))))))


(define (crearline id name rail-type sections)
  (list id name rail-type sections))

