#lang racket/gui

(require racket/gui/base)

(define frame (new frame%
                   [label "Example"]
                   [width 900]
                   [height 500]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "prueba" 0 0))])
(send frame show #t)