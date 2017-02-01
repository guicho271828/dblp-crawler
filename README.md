
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

## Example output

```csv
"研究者氏名"                     "所属"                                                        "職名"                    "jmlr"  "aamas"  "jair"  "icaps"  "kdd"  "nips"  "atal"  "ecai"  "ijcai"  "aaai"
"姜　波"                         "川崎医療福祉大学"                                            "教授"                    1       0        0       0        1      0       0       0       1        1
"韓　博"                         "旧所属　和歌山県立医科大学　大学院医学研究科"                NIL                       0       0        1       0        0      2       1       3       0        1
"陳　波"                         NIL                                                           NIL                       1       0        0       0        3      3       0       0       0        1
"高　建斌"                       "旧所属　福井大学　工学研究科　大学院学生(博士課程)"          "大学院学生（博士課程）"  0       0        0       0        3      0       0       0       0        2
"陳　彬"                         "旧所属　電気通信大学　大学院電気通信学研究科　電子工学専攻"  "大学院学生（博士課程）"  0       0        0       0        4      0       3       0       1        0
"金　範埈"                       "東京大学"                                                    "教授"                    0       0        0       0        0      1       0       0       0        0
"五十嵐　歩"                     "東京大学"                                                    "講師"                    0       0        0       0        0      0       1       0       0        0
"中村篤祥"                       "北海道大学"                                                  "准教授"                  1       0        0       0        0      0       0       0       0        0
"岩崎　篤"                       "群馬大学"                                                    "准教授"                  0       4        0       0        0      0       20      0       4        6
"NGUYEN　TUAN　ANH"              "神戸大学"                                                    "学術研究員"              0       0        0       0        0      1       0       0       0        0
"Nguyen　Quynh"                  "独立行政法人産業技術総合研究所"                              "その他"                  0       0        0       0        0      2       2       0       0        0
"FUKUNAGA　ALEX　"               "東京大学"                                                    "准教授"                  0       0        3       0        0      0       0       1       1        6
"武田　昭子"                     "昭和女子大学"                                                "教授"                    3       0        0       0        0      1       0       0       0        0
"岸本　章宏"                     NIL                                                           NIL                       0       0        0       0        1      1       1       2       3        6
"AHMED　Mosad　Ibrahim　Ghneim"  "旧所属　愛媛大学　大学院医学系研究科　博士課程"              "その他"                  0       0        0       0        0      1       0       0       0        0
"JATOWT，Adam　Wladyslaw"        "京都大学"                                                    NIL                       0       0        0       0        1      0       0       0       0        0
```

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


