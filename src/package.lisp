#|
  This file is a part of dblp-crawler project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage dblp-crawler
  (:use :cl :trivia :trivia.ppcre :alexandria :iterate :plump))
(in-package :dblp-crawler)

(cl-syntax:use-syntax :cl-interpol)

;;; DBLP journals

(define-constant +dblp+ "http://dblp.uni-trier.de" :test 'equal)

(defparameter *journals*
              `("jair")
  #?"List of journals to search the authors for.
http://dblp.uni-trier.de/db/journals/[id]/")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpattern element (&rest args)
    `(and (type element)
          ,@(let ((*print-case* :downcase))
              (mapcar (lambda-match
                        ((cons attr subpattern)
                         (let ((attrname (princ-to-string attr)))
                           (with-gensyms (tmp)
                             `(guard1 ,tmp
                                      (has-attribute ,tmp ,attrname)
                                      (attribute ,tmp ,attrname)
                                      ,subpattern)))))
                      (plist-alist args))))))

(defun journal-volumes (journal)
  (iter (for x in-vector
             (clss:select
              "li > a"
              (parse
               (dex:get #?"${+dblp+}/db/journals/${journal}/"))))
        (match x
          ((element :href
                    (and href
                         (ppcre
                          (#?"${+dblp+}/db/journals/${journal}/${journal}[0-9]*\.html"))))
           (collect href)))))

;; ${+dblp+}/rec/xml/journals/jair/Wellman93.xml
(defun volume-papers (volume)
  (ematch volume
    ((ppcre (#?"${+dblp+}/db/journals/([^/]*)/.*\.html")
            journal)
     (iter (for x in-vector (clss:select "li > a" (parse (dex:get volume))))
           (match x
             ((element :href (and xml
                                  (ppcre
                                   (#?"${+dblp+}/rec/xml/journals/${journal}/.*\.xml"))))
              (collect xml)))))))

(defun paper-authors (paper)
  (map 'list #'text (clss:select "author" (parse (dex:get paper)))))


(defun crawl-journal-authors ()
  #+nil
  (remove-duplicates
   (uni mappend #'paper-authors
        (mappend #'volume-papers
                 (mappend #'journal-volumes *journals*)))
   :test #'equal)
  (let ((hash (make-hash-table :test 'equal)))
    (handler-case
        (iter (for journal in *journals*)
              (iter (for volume in (journal-volumes journal))
                    (iter (for paper in (volume-papers volume))
                          (iter (for author in (paper-authors paper))
                                (setf (gethash author hash) t)))))
      (sb-sys:interactive-interrupt ()
        ))
    (hash-table-keys hash)))

(defparameter *conferences*
              `("aaai/aaai2016")
  "List of conferences to search the authors for.
${+dblp+}/db/conf/[id/idyear]")

;; http://dblp.uni-trier.de/db/conf/aaai/aaai2016.html

(defun conf-papers (conf-year)
  (let ((address #?"${+dblp+}/db/conf/${conf-year}.html"))
    (match conf-year
      ((ppcre ("([^/]*)/.*") conf)
       (iter (for x in-vector (clss:select "li > a" (parse (dex:get address))))
             (match x
               ((element :href (and xml
                                    (ppcre
                                     (#?"${+dblp+}/rec/xml/conf/${conf}/.*\.xml"))))
                (collect xml))))))))

(defun crawl-conf-authors ()
  (let ((hash (make-hash-table :test 'equal)))
    (handler-case
        (iter (for conf in *conferences*)
              (iter (for paper in (conf-papers conf))
                    (iter (for author in (paper-authors paper))
                          (print author)
                          (setf (gethash author hash) t))))
      (sb-sys:interactive-interrupt ()
        ))
    (hash-table-keys hash)))

;; case-insensitive prefix search: default
;; e.g., sig matches "SIGIR" as well as "signal"
;; exact word search: append dollar sign ($) to word
;; e.g., graph$ matches "graph", but not "graphics"
;; phrase search: connect words by a dot (.)
;; e.g., inform.retriev.tech
;; boolean and: separate words by space
;; e.g., codd model
;; boolean or: connect words by pipe symbol (|)
;; e.g., graph|network
;; boolean not: prepend word by minus sign (-)
;; e.g., knuth -don

;;; researchmap

;; http://researchmap.jp/search/?user_name=Alex%20Fukunaga&op=search


(defun author-page (author)
  (let ((elements
         (clss:select
          ".snsview_card_div a"
          (parse
           (dex:get
            #?"http://researchmap.jp/search/?user_name=${(quri:url-encode author)}&op=search")))))
    (assert (= 1 (length elements)))
    (attribute (elt elements 0) "href")))

(defun author-metadata (author)
  (let ((root (clss:select
               ".cv_basic_item"
               (parse (dex:get (author-page author))))))
    (map 'list
         (lambda (th td)
           (cons (text th) (text td)))
         (print (clss:select "th" root))
         (print (clss:select "td" root)))))

