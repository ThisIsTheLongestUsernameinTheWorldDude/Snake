;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname test) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define-struct world (worm food score))
(define-struct worm (head segments))
(define-struct head (posn dir))
(define-struct segment (posn id))
(define (add-to-list l x)
  (cond
    [(empty? l) (cons x empty)]
    [else (cons (first l) (add-to-list (rest l) x))]))
(define (get-segment l id)
  (cond
    [(empty? l) "error"]
    [(= (segment-id (first l)) id) (first l)]
    [else (get-segment (rest l)id)]))

(define (move-segments l w) 
(cond
      [(empty? l) w]
      [(= (segment-id (first l)) 1) (move-segments (rest l)w)]
      [else (cond
      [(empty? w) (move-segments (rest l) (make-segment (segment-posn (get-segment l (sub1 (segment-id (first l))))) (segment-id (first l))))]
      [else (move-segments (rest l) (add-to-list w (make-segment (segment-posn (get-segment l (sub1 (segment-id (first l))))) (segment-id (first l)))))])]))
(move-segments (cons (make-segment "a" 1) (cons (make-segment "b" 2) (cons (make-segment "c" 3) (cons (make-segment "d" 4) empty)))) empty)