#lang racket/gui

(require racket/gui/base)

(define scoreTeam1 0)
(define scoreTeam2 0)
(define xRec 10)
(define yRec 10)
(define CR1x 20)
(define CR1y 20)
(define CR2x 30)
(define CR2y 30)

(define playerList (list '("CR1" CR1x CR1y) '("CR2" CR2x CR2y)))

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
                (send dc draw-bitmap(read-bitmap "Resources/field.jpg") 0 35 )
                (send dc draw-text (~a "CRC  " scoreTeam1 " BRA  " scoreTeam2) 10 5)
                (send dc draw-rectangle xRec yRec 30 30)
                )
              ]
             )
  )

; Make a button in the frame
(new button% [parent frame]
             [label "prueba"]
             [callback (lambda (button event)
                         (set! scoreTeam1 (+ scoreTeam1 1))
                         ;(animation "CR1" 320 250)
                         )
                       ]
             )

; Class for players

(define player%
  (class object%
    (init x y)

    ; Se definen los atributos
    (define xPos x)
    (define yPos y)
    (define aim 0)
    (define speed 0)
    (define block 0)

    (super-new)

    ; Se definen los metodos
    
    (define/public (get-xpos) xPos)
    (define/public (get-ypos) yPos)
    (define/public (get-aim) aim)
    (define/public (get-speed) speed)
    (define/public (get-block) block)
    (define/public (get-ypos) yPos)
    )
  )

(define CRC1 (new player% (x 10) (y 20)))
(send CRC1 get-xpos)
(send CRC1 get-ypos)
#|
(define (animation listPos xNewPos yNewPos)
  ;(define xRec (~a player "x")) ; CR1x
  ;(define yRec (~a player "y")) ; CR1y
  (define tempList (list-ref playerList (- listPos 1)))
  (cond
    ((and (>= (list-ref tempList 1)  xNewPos) (>= (list-ref tempList 2) yNewPos)) 0)
    (else
     (set! (eval (list-ref tempList 1)) (+ (eval (list-ref tempList 1)) 5))
     (set! (eval (list-ref tempList 2)) (+ (eval (list-ref tempList 2)) 5))
     (print (~a xRec "  " yRec))
     (sleep 0.1)
     (send canvas refresh-now)
     (animation listPos xNewPos yNewPos)
    )
  )
  )
|#

(send frame show #t)