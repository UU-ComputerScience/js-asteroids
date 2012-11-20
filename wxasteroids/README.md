A port of wxAsteroids to the web browser
===========

The [wxHaskell paper][1] explained the library design through a clone of the classic asteroids game coined: wxAsteroids.
Using the Utrecht Haskell Compiler (UHC) we have ported a subset of wxHaskell to the web browser. This has enabled us
to run a feature-light version of wxAsteroids in the browser. See [my thesis][2] for more information.

![wxAsteroids in the browser](https://raw.github.com/rubendg/msc-thesis/master/resources/browser_wxasteroids.png)

In order to compile wxAsteroids make sure that UHC is available under the UHC environment variable and then run:

    make install

    make

Open up wxAsteroids in your browser of choice:

    chromium-browser src/Asteroids.html


Warning: I've only tested it in chrome.


[1]: http://dl.acm.org/citation.cfm?id=1017472.1017483 
[2]: https://raw.github.com/rubendg/msc-thesis/master/thesis.pdf
