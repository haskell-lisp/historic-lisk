Lisk
----

S-expression-based syntax alternative for Haskell.

This software is incomplete. Don't try to use it.

Latest working example:

````common-lisp
    {-# OPTIONS -F -pgmF lisk #-}

    (module demo
      "A demo program!")

    (import data.char system.io (data.map :as map))

    (:: main ('io ()))
    (= main (do (<- line get-line)
                (let (= the-line line)
                     (= some-map map.empty))
                (case :do get-line
                  ("" (put-str-ln "* * Empty!"))
                  (x (put-str-ln x)))
                (foo the-line)
                (return ())))

    (= demo (do (<- line get-line)
                (case :of line
                  ("apples" (return "yum"))
                  (anything (return "...")))))

    (:: foo (-> 'string ('io ())))
    (= foo x (put-str-ln (show (fib (read x)))))

    (= fib 0 0)
    (= fib 1 1)
    (= fib n (+ (fib (- n 1))
                (fib (- n 2))))

    (= fib2
       (case
         (0 0)
         (1 1)
         (n (+ (fib (- n 1))
               (fib (- n 2))))))

    (:: from-just (-> ('maybe a) a a))
    (= from-just (('just a) b) a)
    (= from-just ('nothing b) b)
```
