#lang racket/gui

(require racket/gui/base)
(require 2htdp/universe)

(define scoreTeam1 0)
(define scoreTeam2 0)

(define CR1x 320)
(define CR1y 250)
(define CR2x 320)
(define CR2y 250)
(define CR3x 320)
(define CR3y 250)


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
                (send dc draw-bitmap(read-bitmap "Resources/CRC2.png") CR1x CR1y )
                
                ; coloca el texto del marcador
                (send dc draw-text (~a "CRC  " scoreTeam1 " BRA  " scoreTeam2) 10 5)
                
                )
              ]
             )
  )

; Make a button in the frame
(new button% [parent frame]
             [label "prueba"]
             [callback (lambda (button event)
                         (set! scoreTeam1 (+ scoreTeam1 1))
                         (send canvas refresh-now)
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

(define (animation player xNewPos yNewPos X Y)
  (cond
    ((and (>= (send player get-xpos)  xNewPos) (>= (send player get-ypos) yNewPos)) 0)
    (else
     (send player set-xpos (+ (send player get-xpos) 5))
     (send player set-ypos (+ (send player get-ypos) 5))
     (set! X (send player get-xpos))
     (set! Y (send player get-ypos))
     (sleep 0.1)
     (send canvas refresh-now)
     (animation player xNewPos yNewPos X Y)
    )
    )
  )


; pruebas ----------------------------------------------------------
(define CRC1 (new player% (x 25) (y 25)))
(define jug2 (new player% (x 8) (y 8)))
(define jug3 (new player% (x 8) (y 8)))
(define jugadores (list CRC1 jug2))
(define coords (list (list 100 100) (list 50 50)))

(define xRec 100)
(define yRec 100)

; pruebas ----------------------------------------------------------

(send frame show #t)

 

