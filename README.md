Lisk
----

S-expression-based syntax alternative for Haskell.

This software is incomplete. Don't try to use it.

Latest working example:

    {-# OPTIONS -F -pgmF lisk #-}

    (module demo
      "A demo program!")

    (import data.char system.io)

    (:: main ('io ()))
    (= main (do (<- line get-line)
                (let (= the-line line))
                (foo the-line)))

    (:: foo (-> 'string ('io ())))
    (= foo x (put-str-ln (show (fib (read x)))))

    (= fib 0 0)
    (= fib 1 1)
    (= fib n (+ (fib (- n 1))
                (fib (- n 2))))

    (:: from-just (-> ('maybe a) a a))
    (= from-just (('just a) b) a)
    (= from-just ('nothing b) b)
