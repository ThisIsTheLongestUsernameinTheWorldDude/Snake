;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (move-segments l w)
    
    (cond
      [(empty? l) w]
      [else (cond
              [(first l)
      [(empty? w) (move-segments (rest l) (make-segment (segment-posn (get-segment l (sub1 (segment-id (first l))))) (segment-id (first l))))]
      [else (move-segments (rest l) (add-to-list w (make-segment (segment-posn (get-segment l (sub1 (segment-id (first l))))) (segment-id (first l)))))]))
    
(require 2htdp/image)
(require 2htdp/universe)
(define SCALE 10)
(define SCENE  (place-image (square 400 "solid" "black") 0 0 (empty-scene 400 400))))
(define SEGMENT (circle 5 "solid" "red"))
(define FOOD (circle 10 "solid" "blue"))
(define-struct world (worm food score))
(define-struct worm (head segments))
(define-struct head (posn dir))
(define-struct segment (posn id))

(define (sub5 x)
  (- x 5))

(define (add5 x)
  (+ x 5))

(define (add-to-list l x)
  (cond
    [(empty? l) (cons x empty)]
    [else (cons (first l) (add-to-list (rest l) x))]))

(define (controller w cmd)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)]
         )
    (cond
      [(key=? cmd "w") (make-world (make-worm (make-head (head-posn head) "u") (worm-segments worm)) "food" "score")]
      [(key=? cmd "a") (make-world (make-worm (make-head (head-posn head) "l") (worm-segments worm)) "food" "score")]
      [(key=? cmd "s") (make-world (make-worm (make-head (head-posn head) "d") (worm-segments worm)) "food" "score")]
      [(key=? cmd "d") (make-world (make-worm (make-head (head-posn head) "r") (worm-segments worm)) "food" "score")])))
(define (get-segment l id)
  (cond
    [(empty? l) "error"]
    [(= (segment-id (first l)) id) (first l)]
    [else (get-segment (rest l)id)]))

(define (move w)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)]
         )
    (cond
      [(string=? dir "u") (make-world (make-worm (make-head (make-posn x  (sub5 y))  "u") (worm-segments worm)) "food" "score")]
      [(string=? dir "l") (make-world (make-worm (make-head (make-posn  (sub5 x) y) "l") (worm-segments worm)) "food" "score")]
      [(string=? dir "d") (make-world (make-worm (make-head (make-posn  x  (add5 y)) "d") (worm-segments worm)) "food" "score")]
      [(string=? dir "r") (make-world (make-worm (make-head (make-posn  (add5 x) y) "r") (worm-segments worm)) "food" "score")])))

(define (render w)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)]
         ) (place-image SEGMENT x y SCENE)))

(big-bang (move (make-world (make-worm (make-head (make-posn 205 205) "u") "segments") "food" "score"))
          (on-key controller)
          (on-tick move)
          (to-draw render))


