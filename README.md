
# Dblp-Crawler - DBLP crawler

cf. http://dblp.uni-trier.de/faq/Am+I+allowed+to+crawl+the+dblp+website

## Installation

1. Install [Roswell](https://github.com/roswell/roswell) --- on Mac, `brew install roswell`
2. `ros install guicho271828/dblp-crawler`
3. `PATH=~/.roswell/bin:$PATH`
4. `crawl-jp-researcher -j [journals...] -c [conference+year ids...]`

+ Journal ID: `http://dblp.uni-trier.de/db/journals/[ID]/` e.g. [jair](http://dblp.uni-trier.de/db/journals/jair/)
+ Conference ID / Conference ID + year : `http://dblp.uni-trier.de/db/conf/[conf/conf+year].html` e.g. [aaai/aaai2016](http://dblp.uni-trier.de/db/conf/aaai/aaai2016.html)

Example usage (on bash):

```
crawl-jp-researcher -j jair aamas jmlr \
                    -c aaai/aaai{2012..2016} ijcai/ijcai{2012..2016} \
                       ecai/ecai{2012..2016} atal/aamas{2012..2016} \
                       nips/nips{2012..2016} kdd/kdd{2012..2016} \
                       icaps/icaps{2012..2016} \
                       > csv
```

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


