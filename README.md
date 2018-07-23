Arithmetics.
============

The definition of T.
--------------------

* [T, F, Z]        in T.
* [S t, P t, E t]  in T.
* I t t t          in T.

How many elements does T have?
------------------------------

A polynomial, recursively applied.

There are t = 3 terminals, u = 3 unary constructors and i = 1 ternary one _(`if`)_. So:

    f = t + u × x + i × (3 → t)

    (f . f . f) 0 == 59439  -- The number of elements of depth <= 3.
