;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
(define SCALE 10)
(define segment (circle 10 "solid" "red"))
(define SCENE (place-image (rectangle 1 200 "solid" "yellow") 199 100 (place-image (square 400 "solid" "black") 0 0 (empty-scene 200 200))))
