;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
(define SCALE 10)
(define SCENE  (place-image (square 400 "solid" "black") 204 204 (place-image (square 410 "solid" "red") 204 204 (empty-scene 408 408))))
(define SEGMENT (circle 10 "solid" "red"))
(define FOOD (circle 10 "solid" "blue"))
(define-struct world (worm food score))
(define-struct worm (head segments))
(define-struct head (posn dir))
(define-struct segment (pos id))

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


