{-# OPTIONS -F -pgmF lisk #-}

(module demo
  "A demo program!")

(import data.char)
(import system.io)

(= main (foo "Hello, World!"))

(= foo x (put-str-ln (show (fib (read x)))))

(= fib 0 0)
(= fib 1 1)
(= fib n (+ (fib (- n 1))
            (fib (- n 2))))
