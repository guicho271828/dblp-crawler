#|
  This file is a part of dblp-crawler project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :dblp-crawler.test
  (:use :cl
        :dblp-crawler
        :fiveam
        :dexador :plump :trivia :alexandria :iterate))
(in-package :dblp-crawler.test)



(def-suite :dblp-crawler)
(in-suite :dblp-crawler)

;; run test with (run! test-name) 

(test dblp-crawler

  )



