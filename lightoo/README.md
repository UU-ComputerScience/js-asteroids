LightOO
=======

A lightweight Object-Oriented programming library for Haskell.

The library is based on the ["Mutable Objects, with tail-polymorphism"][1] approach initially described by the authors of OOHaskell and it extends it with:

* Generic up and downcasts using dynamic typing
* An inheritance combinator [W. Cook][2]
* Parameterized classes
* CPP macros for deriving parts of the boilerplate

The OO programming techniques offered by the library were used to implement a subset of the [wxWidgets design in Haskell][3]. 
Please look inside the `src/Examples` directory for additional examples. 

Usage GHC
-------

    make ghci 

or 

    make ghci MAIN=src/Examples/{One of the examples}

Usage UHC
---------

Make sure the UHC environment variable points to your local UHC installation.

    make 

or

    make MAIN=src/Examples/{One of the examples}

[1]: http://homepages.cwi.nl/~ralf/OOHaskell/
[2]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.11.8792
[3]: https://github.com/rubendg/msc-thesis
