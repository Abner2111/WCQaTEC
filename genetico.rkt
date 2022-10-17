#lang racket/gui

;;POBLACION INICIAL
;;----------------------------------------------------------------------------------------------------
;;se le da una alineacion '(num_defensas, num_medios,num_delanteros)
;;retorna una lista con listas con 3 veces mas la cantidad de los jugadores indicados en la alineacion
;;jugador = (posx, posy, interceptar [0-10], velocidad [0-10], punteria [0-10] )
(define (poblacion_inicial alineacion)
    (list 
        alineacion 
        (jugadores_iniciales (* 3 (car alineacion)) 1 '()) 
        (jugadores_iniciales (* 3 (cadr alineacion)) 2 '()) 
        (jugadores_iniciales (* 3 (caddr alineacion)) 3 '())
    )
)

(define (jugadores_iniciales cantidad pos jugadores)
    (
        cond 
        (
            (zero? cantidad)
            jugadores
        )
        (
            (jugadores_iniciales 
                (- cantidad 1) 
                pos 
                (cons
                    (list 
                        (car (random-coords pos)) 
                        (cadr (random-coords pos)) 
                        (quotient (random 101) 10)
                        (quotient (random 101) 10) 
                        (quotient (random 101) 10)
                    )
                    jugadores 

                ) 
            )
        )
    )
)
;;supone cancha de 80px de largo, iniciando en pixel 22 y  55px de ancho
;;limites horizontales: [0-19], [20-59], [60-79] 
;;limite vertical [0-54]
(define (random-coords position)
    (
        cond

        (
            ;;defensas
            (equal? position 1)
            (list (+ 22 (* 5 (random (/ 20 5)))) (* 5 (random (/ 55 5))))
        )
        
        (
            ;;mediocampistas
            (equal? position 2)
            (list (+ 42 (* 5 (random (/ 40 5)))) (* 5 (random (/ 55 5))))
        )
        (
            ;;delanteros
            (equal? position 3)
            (list (+ 82 (* 5 (random (/ 20 5)))) (* 5 (random (/ 55 5))))
        )
    )
   
)

;;----------------------------------------------------------------------------------------------------


;;FITNESS 
;;Recibe lista de jugadores (defensas, medios, delanteros) y la alineacion
;;----------------------------------------------------------------------------------------------------

(define (fitness poblacion alineacion)
    (list 
        alineacion
        (select-cuantity (car alineacion) (quicksort  (car poblacion) 1) '())
        (select-cuantity (cadr alineacion) (quicksort  (cadr poblacion) 2) '())
        (select-cuantity (caddr alineacion) (quicksort  (cadr poblacion) 3) '())
    )
)

(define (select-cuantity cantidad poblacion jugadores-seleccionados)
    (
        cond
        (
            (zero? cantidad)
            jugadores-seleccionados
        )
        (
            else
            (select-cuantity (- cantidad 1 ) (cdr poblacion) (cons (car poblacion) jugadores-seleccionados))
        )
    )
)

(define (quicksort lista posicion)
  (cond ( (null? lista)
          '())
        (else
           (append 
           (quicksort (cadr (smaller_grater (car lista) (cdr lista) '() '() posicion)) posicion)
           (list (car lista)) 
           (quicksort (car (smaller_grater (car lista) (cdr lista) '() '() posicion)) posicion) 
           
           )
        )
  )
)
;; delanteros: ((interceptar [0-10] + velocidad [0-10] +  punteria [0-10])/ 8) + punteria
;; medios: ((interceptar [0-10] + velocidad [0-10] +  punteria [0-10]) / 8) + velocidad
;; defensas: ((interceptar [0-10] + velocidad [0-10] + punteria [0-10]) / ) + interceptar
(define (smaller_grater pivot lista menores mayores posicion)
  (cond ( (null? lista)
          (list menores mayores))
            
        ( (<= 
            ( + 
                ( / 
                    (+ 
                        (n-esimo (+ 2 1) (car lista) )
                        (n-esimo (+ 2 2) (car lista) ) 
                        (n-esimo (+ 2 3) (car lista) )
                    ) 
                    3
                ) 
                (n-esimo  (+ 2 posicion) (car lista))
            )
            (n-esimo  (+ 2 posicion) pivot) 
          )
          
          (smaller_grater pivot (cdr lista) (cons (car lista) menores) mayores posicion))
        ( else
          (smaller_grater pivot (cdr lista) menores (cons (car lista) mayores) posicion)
        )
    )
)
(define (largo lista)
  (
    cond 
    (
      (null? lista)
      0
    )
    (
      else
      (largo-aux 0 lista)
    )
  )
)

(define (largo-aux n-elementos lista)
  (
    cond 
    (
      (null? lista)
      n-elementos
    )
    (
      else
      (largo-aux (+ 1 n-elementos) (cdr lista))
    )
  )
)

(define (n-esimo n lista)
  (
    cond 
    (
      (> n (largo lista))
      #f
    )
    (
      else
      (n-esimo-aux 1 n lista)
    )
  )
)

(define (n-esimo-aux indice n lista)
  (
    cond
    (
      (equal? indice n)
      (car lista)
    )
    (
      else
      (n-esimo-aux (+ 1 indice) n (cdr lista))
    )
  )
)

;;'((4 4 2) ((22 25 4 7 9) (27 30 0 9 9) (32 35 3 8 2) (27 10 5 3 1) (22 10 2 9 7) (22 15 4 6 4) (22 35 7 0 4) (32 50 5 2 7) (27 5 1 8 3) (32 35 2 1 0) (22 30 10 4 4) (27 10 3 0 8)) ((42 15 3 4 1) (72 40 9 9 2) (42 0 0 7 0) (77 35 6 8 6) (62 50 8 1 1) (52 45 7 1 9) (57 40 5 7 9) (42 25 5 8 5) (42 0 4 6 1) (52 15 0 2 4) (57 10 6 6 2) (72 20 1 2 1)) ((92 45 4 5 6) (82 30 6 10 0) (82 30 9 2 9) (87 50 4 5 2) (97 25 2 0 0) (82 5 3 9 0)))

#|
(fitness (cdr '((4 4 2) ((22 25 4 7 9) (27 30 0 9 9) (32 35 3 8 2) (27 10 5 3 1) (22 10 2 9 7) (22 15 4 6 4) (22 35 7 0 4) (32 50 5 2 7) (27 5 1 8 3) (32 35 2 1 0) (22 30 10 4 4) (27 10 3 0 8)) ((42 15 3 4 1) (72 40 9 9 2) (42 0 0 7 0) (77 35 6 8 6) (62 50 8 1 1) (52 45 7 1 9) (57 40 5 7 9) (42 25 5 8 5) (42 0 4 6 1) (52 15 0 2 4) (57 10 6 6 2) (72 20 1 2 1)) ((92 45 4 5 6) (82 30 6 10 0) (82 30 9 2 9) (87 50 4 5 2) (97 25 2 0 0) (82 5 3 9 0))))
(car '((4 4 2) ((22 25 4 7 9) (27 30 0 9 9) (32 35 3 8 2) (27 10 5 3 1) (22 10 2 9 7) (22 15 4 6 4) (22 35 7 0 4) (32 50 5 2 7) (27 5 1 8 3) (32 35 2 1 0) (22 30 10 4 4) (27 10 3 0 8)) ((42 15 3 4 1) (72 40 9 9 2) (42 0 0 7 0) (77 35 6 8 6) (62 50 8 1 1) (52 45 7 1 9) (57 40 5 7 9) (42 25 5 8 5) (42 0 4 6 1) (52 15 0 2 4) (57 10 6 6 2) (72 20 1 2 1)) ((92 45 4 5 6) (82 30 6 10 0) (82 30 9 2 9) (87 50 4 5 2) (97 25 2 0 0) (82 5 3 9 0)))))
|#

;;----------------------------------------------------------------------------------------------------

;;CROSSOVER
;;----------------------------------------------------------------------------------------------------
(define (crossover population)
  (list 
    (crossover-perposition (car population) 1) 
    (crossover-perposition (cadr population) 2) 
    (crossover-perposition (caddr population) 3)
  )
)

(define (crossover-perposition jugadores posicion)
  (crossover-aux jugadores (car jugadores) posicion)
)

(define (crossover-per-position-aux jugadores primer-jugador posicion)
  (
    cond
    (
      (null? jugadores)
      '()
    )
    (
      (null? (cadr jugadores))
      (cons (cross-parent-genes (car jugadores) primer-jugador posicion) (crossover-per-position-aux (cdr jugadores) primer-jugador posicion))
    )
    (
      else
      (cons (cross-parent-genes (car jugadores) (cadr jugadores) posicion) (crossover-per-position-aux (cdr jugadores) primer posicion))
    )
  )
)

(define (cross-parent-genes jugador1 jugador2 posicion)
  (cross-genes-aux1 jugador1 jugador2 (+ 2 posicion))
)

(define (cross-genes-aux1 jugador1 jugador2 punto-corte) 
  (
    cond
    (
      (null? jugador2)
      '()
    )
    (
      (<= punto-corte 1)
      (cons (car jugador2) (cross-genes-aux1 jugador1 (cdr jugador2) punto-corte) )
    )
    (
      else
      (cons (car jugador1) (cross-genes-aux1 (cdr jugador1) (cdr jugador2) (- punto-corte 1)))
    )
  )
)

(cross-parent-genes '(45 34 4 7 3) '(78 90 10 1 6) 1)
;;----------------------------------------------------------------------------------------------------


;;MUTATION
;;----------------------------------------------------------------------------------------------------


;;----------------------------------------------------------------------------------------------------