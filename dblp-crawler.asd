#|
  This file is a part of dblp-crawler project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  DBLP crawler

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage dblp-crawler-asd
  (:use :cl :asdf))
(in-package :dblp-crawler-asd)


(defsystem dblp-crawler
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :bug-tracker "https://github.com/guicho271828/dblp-crawler/issues"
  :source-control (:git "https://github.com/guicho271828/dblp-crawler.git")
  :license "LLGPL"
  :depends-on (:dexador :plump :trivia :alexandria :iterate)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "DBLP crawler"
  :in-order-to ((test-op (test-op :dblp-crawler.test))))
