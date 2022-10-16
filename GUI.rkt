#lang racket/gui

(require racket/gui/base)

(define scoreTeam1 0)
(define scoreTeam2 0)

(define CR1x 320)
(define CR1y 250)
(define CR2x 320)
(define CR2y 250)


; Creates window frame
(define frame (new frame%
                   [label "World Cup QaTec"]
                   [width 640]
                   [height 500]))

; Creates a canvas into the mainframe
(define canvas
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)

                ; coloca la imagen
                (send dc draw-bitmap(read-bitmap "Resources/field.jpg") 0 35 )
                (send dc draw-bitmap(read-bitmap "Resources/CRC1.png") xRec yRec )
                
                ; coloca el texto del marcador
                (send dc draw-text (~a "CRC  " scoreTeam1 " BRA  " scoreTeam2) 10 5)

                ; dibuja un ractangulo de prueba
                ;(send dc draw-rectangle xRec yRec 30 30)
                ;(send dc draw-rectangle CR1x CR1y 30 30)
                ;(send dc draw-rectangle CR2x CR2y 30 30)
                
                ;(animation CRC1 320 250 dc)
                ;(posiniciales jugadores coords dc)
                ;(send CRC1 move 200 250 dc)
                
                )
              ]
             )
  )

; Make a button in the frame
(new button% [parent frame]
             [label "prueba"]
             [callback (lambda (button event)
                         (set! scoreTeam1 (+ scoreTeam1 1))
                         (animation CRC1 320 250)
                         ;(send CRC1 set-xpos (random 500))
                         ;(send CRC1 set-ypos (random 500))
                         ;(send CRC1 set-draw dc)
                         ;(set! yRec (random 500))
                         ;(send canvas refresh-now)
                         )
                       ]
             )

; Class for players

(define player%
  (class object%

    ; atributos iniciales
    (init x y)

    ; Se definen los atributos
    (define xPos x)
    (define yPos y)
    (define aim 0)
    (define speed 0)
    (define block 0)
    (define rectangulo 0) 

    (super-new)

    ; Se definen los metodos
    
    (define/public (get-xpos) xPos)
    (define/public (get-ypos) yPos)
    (define/public (get-aim) aim)
    (define/public (get-speed) speed)
    (define/public (get-block) block)
    (define/public (get-rec) rectangulo)

    (define/public (set-xpos newX)
      (set! xPos newX))
    (define/public (set-ypos newY)
      (set! yPos newY))
    (define/public (set-aim newAim)
      (set! aim newAim))
    (define/public (set-speed newSpeed)
      (set! speed newSpeed))
    (define/public (set-block newBlock)
      (set! block newBlock))
    (define/public (set-draw dc)
      (set! rectangulo (send dc draw-rectangle xPos yPos 30 30)))
    (define/public (move x y dc)
      (send dc translate x y))
  ))

;Class for ball
(define ball%
  (class object%

    ;atributos iniciales
    (init xi yi xf yf)

    ;Definicion de atributos
    (define xIni xi)
    (define yIni yi)
    (define xFin xf)
    (define yFin yf)

    (super-new) ;No sé si es necesario ponerlo

    ;Getters
    (define/public (get-xini) xIni)
    (define/public (get-yini) yIni)
    (define/public (get-xfin) xFin)
    (define/public (get-yfin) yFin)

    ;Setters
    (define/public (set-xini newXini)
      (set! xIni newXini))
    (define/public (set-yini newYini)
      (set! yIni newYini))
    (define/public (set-xfin newXfin)
      (set! xFin newXfin))
    (define/public (set-yfin newYfin)
      (set! yFin newYfin))
    (define/public (ballpass newXfin newYfin)
      (set! xIni newXfin)
      (set! yIni newYfin))
    ))
    

    

; Crea las posiciones iniciales de todos los jugadores 
(define (posiniciales listajugs listacoord canvas)
  (cond ((or (null? listacoord) (null? listajugs))
         '())
        (else
         (send (car listajugs) set-xpos (car (car listacoord)))
         (send (car listajugs) set-ypos (car (cdr (car listacoord))))
         (send (car listajugs) set-draw canvas)
         (posiniciales (cdr listajugs) (cdr listacoord) canvas))))

;;Metodo para realizar animaciones de todos los jugadores
(define (newpos listajugs listacoord)
  (cond ((or (null? listacoord) (null? listajugs))
         '())
        (else
         (animation (car listajugs) (car (car listacoord)) (car(cdr (car listacoord))))
         (newpos (cdr listajugs) (cdr listacoord)))))
        
        
  
; Cambia las coordenadas para la animacion FALTA CONECTARLO CON LA INTERFAZ

(define (animation player xNewPos yNewPos)
  (cond
    ((and (>= (send player get-xpos)  xNewPos) (>= (send player get-ypos) yNewPos)) 0)
    (else
     (send player set-xpos (+ (send player get-xpos) 5))
     (send player set-ypos (+ (send player get-ypos) 5))
     (set! xRec (send player get-xpos))
     (set! yRec (send player get-ypos))
     (sleep 0.1)
     (send canvas refresh-now)
     (animation player xNewPos yNewPos)
    )
    )
  )


; pruebas ----------------------------------------------------------
(define CRC1 (new player% (x 25) (y 25)))
(define jug2 (new player% (x 8) (y 8)))
(define jugadores (list CRC1 jug2))
(define coords (list (list 100 100) (list 50 50)))

(define xRec (send CRC1 get-xpos))
(define yRec (send CRC1 get-ypos))

; pruebas ----------------------------------------------------------

;;prueba
;;(print (car (car coords)))
;(send (car jugadores) get-xpos)
;(send (car jugadores) get-ypos)
;(send jug2 get-xpos)
;(send jug2 get-ypos)

(send frame show #t)

 

