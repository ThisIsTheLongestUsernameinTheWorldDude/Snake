;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

    
   
    
(require 2htdp/image)
(require 2htdp/universe)
(define SCALE 10)
(define SCENE  (place-image (square 1600 "solid" "black") 0 0 (empty-scene 800 800)))
(define SEGMENT (circle 5 "solid" "red"))
(define FOOD (circle 5 "solid" "blue"))
(define-struct world (worm food score))
(define-struct worm (head segments))
(define-struct head (posn dir))
(define-struct segment (posn id))

(define (sub10 x)
  (- x 10))

(define (add10 x)
  (+ x 10))

(define (add-to-list l x)
  (cond
    [(empty? l) (cons x empty)]
    [else (cons (first l) (add-to-list (rest l) x))]))
(define (get-segment l id)
  (cond
    [(empty? l) "error"]
    [(= (segment-id (first l)) id) (first l)]
    [else (get-segment (rest l)id)]))

(define (move-worm l)
  (cons (make-segment (head-posn (worm-head (world-worm l))) 1)(move-segments (worm-segments (world-worm l)) empty (worm-segments (world-worm l)))))
(define (move-segments l w stor) 
(cond
      [(empty? l) w]
      [(= (segment-id (first l)) 1) (move-segments (rest l)w stor)]
      [else (cond
      [(empty? w) (move-segments (rest l) (cons (make-segment (segment-posn (get-segment stor (sub1 (segment-id (first l))))) (segment-id (first l)) )empty)stor)]
      [else (move-segments (rest l) (add-to-list w (make-segment (segment-posn (get-segment stor (sub1 (segment-id (first l))))) (segment-id (first l))))stor)])]))

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
      [(key=? cmd "d") (make-world (make-worm (make-head (head-posn head) "r") (worm-segments worm)) "food" "score")]
      [else w])))


(define (move w)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)]
         [segments (worm-segments (world-worm w))]
         )
    (cond
      [(string=? dir "u") (make-world (make-worm (make-head (make-posn x  (sub10 y))  "u") (move-worm w)) "food" "score")]
      [(string=? dir "l") (make-world (make-worm (make-head (make-posn  (sub10 x) y) "l") (move-worm w)) "food" "score")]
      [(string=? dir "d") (make-world (make-worm (make-head (make-posn  x  (add10 y)) "d") (move-worm w)) "food" "score")]
      [(string=? dir "r") (make-world (make-worm (make-head (make-posn  (add10 x) y) "r") (move-worm w)) "food" "score")])))


(define (render-segments l)
  (cond
    [(empty? l) SCENE]
    [else (place-image SEGMENT (posn-x (segment-posn (first l))) (posn-y (segment-posn (first l))) (render-segments (rest l)))]))

(define (render w)
  (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         [dir (head-dir (worm-head (world-worm w)))]
         [head (worm-head (world-worm w))]
         [worm (world-worm w)])
         (place-image SEGMENT x y (render-segments (worm-segments (world-worm w))) )))
(define (stop w)
   (let* ([x (posn-x (head-posn (worm-head (world-worm w))))]
         [y (posn-y (head-posn (worm-head (world-worm w))))]
         )
     (cond
      [(>= x 800) #t]
      [(>= y 800) #t]
       [(<= x 0) #t]
      [(<= y 0) #t]
       [else false])))

(big-bang (move (make-world (make-worm (make-head (make-posn 205 205) "u") (cons (make-segment (make-posn 205 200)  1) (cons (make-segment (make-posn 205 195) 2) (cons (make-segment (make-posn 205 190) 3) (cons (make-segment (make-posn 205 185) 4) empty))))) "food" "score"))
          (on-key controller)
          (on-tick move)
          (to-draw render)
          (stop-when stop))


