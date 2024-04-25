;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname BATTLEGAME) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require picturing-programs)
(require "BATTLEGAME-msg.rkt")

;structure to send
(define-struct table (id tries attack boat))

;players
(define player1 (make-table 1 3 0 1))
(define player2 (make-table 2 3 0 2))
;------------------chose mark---------------------------
(define mark
  (circle 5 "solid" "red"))

;------------------2x2 grid-------------------------------
(define Vstack (above (rectangle 50 50 "outline" "black")(rectangle 50 50 "outline" "black")))
(define grid (beside Vstack Vstack))
;background
(define background
  (place-image grid 55 70 (rectangle 110 140 "solid" "white")))
;-------------------------Draw-h helper-----------------------------
(define (backworks model)
  (overlay/align "right" "top" (beside (text "tries:" 18 "red")(text (number->string(table-tries model)) 18 "black"))
                 (overlay/align  "left" "top"
                                 (beside (text "p:" 18 "black")(text (number->string(table-id model)) 18 "blue"))
                                 background)))
;;--------------------D-handler-----------------------------------------
(define (draw-h model)
  (cond [(> (table-boat model) 0)(backworks model)]
        [else (overlay/align "center" "center" (text "GAME OVER" 20 "red") (backworks model))]))
;=====================helper functions==================================

(define (minus1 model)(make-table (table-id model)(- (table-tries model) 1) (table-attack model) (table-boat model)))
;=======================================================================
;--------------------recieve handler------------------------------------
(define (receive-h model msg)
  (cond [(= (table-msg-attack msg) (table-boat model))(make-table (table-id model)(table-tries model)(table-attack model)0)] 
        [else model]))

#|-----------------NEW MOUSE HANDLER-----------------------|#
(define (mouse-h model x y event)
  (cond 
    [(and (>= x 2.5) (<= x 52.5) (>= y 20) (<= y 70) (mouse=? event "button-down"))
          (make-package model (make-table-msg (table-id model) (- (table-tries model) 1) 1 (table-boat model)))] 
    [(and (>= x 52.5) (<= x 105) (>= y 20) (<= y 70) (mouse=? event "button-down"))
     (make-package model (make-table-msg (table-id model) (- (table-tries model) 1) 2 (table-boat model)))]
    [(and (>= x 2.5) (<= x 52.5) (>= y 70) (<= y 120) (mouse=? event "button-down"))
     (make-package model (make-table-msg (table-id model) (- (table-tries model) 1) 3 (table-boat model)))]
    [(and (>= x 52.5) (<= x 105) (>= y 70) (<= y 120) (mouse=? event "button-down"))
     (make-package model (make-table-msg (table-id model) (- (table-tries model) 1) 4 (table-boat model)))]
    [else model]))
;-----------------------------stop-when-----------------------
(define (game-over? model msg)
  (or (= (table-tries player1) 0)     
      (= (table-tries player2) 0)
      (= (table-boat player1) 0)
      (= (table-boat player2) 0)))
;-------------------------------------------------------------
(define (start-bang title initial)
   (big-bang initial
      (name title)
      (register LOCALHOST)
     (to-draw draw-h)
      (on-mouse mouse-h)
      (on-receive receive-h)
     ))

(launch-many-worlds
   (start-bang "first" player1)
   (start-bang "second" player2))