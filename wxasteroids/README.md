A port of wxAsteroids to the web browser
===========

The [wxHaskell paper][1] demonstrates its library design through a clone of the classic asteroids game coined: wxAsteroids.
Using the Utrecht Haskell Compiler (UHC) we have ported a subset of wxHaskell to the web browser. This has enabled us
to run a feature-light version of wxAsteroids in the browser. See [my thesis][2] for more information, for the demo [click here](http://uu-computerscience.github.com/js-asteroids/build/Asteroids.html).

![wxAsteroids in the browser](https://raw.github.com/UU-ComputerScience/js-asteroids/master/msc-thesis/resources/browser_wxasteroids.png)

Building wxAsteroids
--------------------

In order to build wxAsteroids you will need:

* A recent version of [UHC](https://github.com/UU-ComputerScience/uhc) (tested with ehc-1.1.4, revision js@658578eae9)  
* [cpphs](http://projects.haskell.org/cpphs/)

Make sure that you have an environment variable (UHC) setup to refer to your UHC binary:
    
    export UHC = "your UHC binary location here"

Running make will compile the sources and output a single HTML file that links everything together.

    make

To run the application open up the HTML file in your browser of choice:

    chromium-browser src/Asteroids.html

Warning: only tested in Chrome on Ubuntu 12.10

[1]: http://dl.acm.org/citation.cfm?id=1017472.1017483 
[2]: https://github.com/UU-ComputerScience/js-asteroids/raw/master/msc-thesis/thesis.pdf
