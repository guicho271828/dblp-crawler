#|
  This file is a part of dblp-crawler project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage dblp-crawler.test-asd
  (:use :cl :asdf))
(in-package :dblp-crawler.test-asd)


(defsystem dblp-crawler.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of dblp-crawler"
  :license "LLGPL"
  :depends-on (:dblp-crawler
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(let ((res (5am:run :dblp-crawler)))
     (explain! res)
     (every #'fiveam::TEST-PASSED-P res))"))
))
