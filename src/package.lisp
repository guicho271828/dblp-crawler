#|
  This file is a part of dblp-crawler project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage dblp-crawler
  (:use :cl :trivia :trivia.ppcre :alexandria :iterate :plump
        :arrow-macros)
  (:shadowing-import-from :arrow-macros :<>)
  (:import-from :clss :select)
  (:export
   #:run-all
   #:*conferences*
   #:*journals*
   #:journal-ids))
(in-package :dblp-crawler)

(cl-syntax:use-syntax :cl-interpol)

;;; DBLP journals

(define-constant +dblp+ "http://dblp.uni-trier.de" :test 'equal)

(defparameter *journals*
              `()                       ;"jair"
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

(defmacro ignore-errors-and-return ((name &optional value) &body body)
  `(handler-case
       (progn ,@body)
     (error ()
       (return-from ,name ,@(when value `(,value))))
     (USOCKET:NS-TRY-AGAIN-CONDITION ()
       (return-from ,name ,@(when value `(,value))))))

(defun journal-volumes (journal)
  (iter (for x in-vector
             (select
              "li > a"
              (parse
               (ignore-errors-and-return (journal-volumes)
                 (dex:get #?"${+dblp+}/db/journals/${journal}/")))))
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
     (iter (for x in-vector (select "li > a" (parse (ignore-errors-and-return
                                                        (volume-papers)
                                                      (dex:get volume)))))
           (match x
             ((element :href (and xml
                                  (ppcre
                                   (#?"${+dblp+}/rec/xml/journals/${journal}/.*\.xml"))))
              (collect xml)))))))

(defun paper-authors (paper)
  (map 'list #'text (select "author" (parse (ignore-errors-and-return
                                                (paper-authors)
                                              (dex:get paper))))))

(defun printn (thing)
  (write thing :escape nil)
  (terpri))

(defun crawl-journal-authors ()
  #+nil
  (remove-duplicates
   (uni mappend #'paper-authors
        (mappend #'volume-papers
                 (mappend #'journal-volumes *journals*)))
   :test #'equal)
  (let ((hash (make-hash-table :test 'equal))
        (*standard-output* *error-output*))
    (handler-case
        (iter (initially (printn "Crawling journal volumes..."))
              (for journal in *journals*)
              (iter (initially (printn #?"Crawling ${journal}..."))
                    (for volume in (journal-volumes journal))
                    (for i from 1)
                    (iter (initially (printn #?" Crawling ${journal}/${journal}${volume}"))
                          (for paper in (volume-papers volume))
                          (iter (initially (printn #?"  Crawling ${paper}"))
                                (for author in (paper-authors paper))
                                (setf (gethash author hash) t)))))
      (sb-sys:interactive-interrupt ()
        ))
    (hash-table-keys hash)))

(defparameter *conferences*
              `()                       ;"aaai/aaai2016"
  "List of conferences to search the authors for.
${+dblp+}/db/conf/[id/idyear]")

;; http://dblp.uni-trier.de/db/conf/aaai/aaai2016.html

(defun conf-papers (conf-year)
  (let ((address #?"${+dblp+}/db/conf/${conf-year}.html"))
    (match conf-year
      ((ppcre ("([^/]*)/.*") conf)
       (iter (for x in-vector (select "li > a" (parse (ignore-errors-and-return
                                                          (conf-papers)
                                                        (dex:get address)))))
             (match x
               ((element :href (and xml
                                    (ppcre
                                     (#?"${+dblp+}/rec/xml/conf/${conf}/.*\.xml"))))
                (collect xml))))))))

(defun crawl-conf-authors ()
  (let ((hash (make-hash-table :test 'equal))
        (*standard-output* *error-output*))
    (handler-case
        (iter (initially (printn "Crawling conferences..."))
              (for conf in *conferences*)
              (iter (initially (printn #?"Crawling conference ${conf}..."))
                    (for paper in (conf-papers conf))
                    (iter (initially (printn #?" Crawling ${paper}"))
                          (for author in (paper-authors paper))
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

;;; researchmap author data

;; http://researchmap.jp/search/?user_name=Alex%20Fukunaga&op=search

(defun author-page (author)
  (let ((elements
         (select
          ".snsview_card_div a"
          (parse
           (ignore-errors-and-return
               (author-page)
             (dex:get
            #?"http://researchmap.jp/search/?lang=english&user_name=${(quri:url-encode author)}&op=search"))))))
    (match elements
      ((vector)
       nil)
      ((vector e)
       (attribute e "href"))
      ((vector* e)
       (warn "Perhaps there are multiple ~a; Using the first candidate only" author)
       (attribute e "href")))))

(defun author-metadata (author)
  (let ((address (author-page author)))
    (when address
      (let ((root (select
                   ".cv_basic_item"
                   (parse (ignore-errors-and-return
                              (author-metadata)
                            (dex:get address))))))
        (map 'list
             (lambda (th td)
               (cons (text th) (text td)))
             (select "th" root)
             (select "td" root))))))

;;; dblp author data

;; http://dblp.uni-trier.de/search/author?author=Schek

(defun author-dblpkeys (author)
  (ignore-errors-and-return (author-dblpkeys)
    (-<> (quri:url-encode author)
      (dex:get #?"${+dblp+}/search/author?xauthor=${<>}")
      (parse)
      (select "author" <>)
      (map 'list (lambda (x) (attribute x "urlpt")) <>))))

(defun author-venues (author)
  (ignore-errors-and-return (author-venues)
    (flatten
     (mapcar
      (lambda (x)
        (-<> (dex:get #?"${+dblp+}/rec/pers/${x}/xk")
          (parse)
          (select "dblpkey" <>)
          (map 'list (lambda (e)
                       (match (text e)
                         ((split* "/" _ venue _) venue)))
               <>)))
      (author-dblpkeys author)))))

;;; combine

(defun journal-ids ()
  (append *journals*
          (remove-duplicates
           (mapcar (lambda-match
                     ((ppcre ("([^/]*)/.*") conf)
                      conf))
                   *conferences*)
           :test #'equal)))

(defun venue-histogram (author)
  (mapcar (lambda (cj)
            (count cj (author-venues author) :test #'equal))
          (journal-ids)))

(defun run-all ()
  (printn #?"Start Crawling. C-c to interrupt at any time")
  (let ((acc nil)
        (authors (sort (union (crawl-journal-authors)
                              (crawl-conf-authors)
                              :test 'equal)
                       #'string<))
        (*standard-output* *error-output*))
    (handler-case
        (iter (initially (printn #?"Searching authors from researchmap.jp..."))
              (for author in authors)
              (princ #?" Searching ${author}")
              (for jp-metadata = (author-metadata author))
              (if jp-metadata
                  (let ((name
                         (assoc "研究者氏名" jp-metadata :test #'equal))
                        (affiliation
                         (assoc "所属" jp-metadata :test #'equal))
                        (status
                         (assoc "職名" jp-metadata :test #'equal)))
                    (printn " --- found!")
                    (push
                     (list* (cdr name)
                            (cdr affiliation)
                            (cdr status)
                            (venue-histogram author))
                     acc))
                  (terpri)))
      (sb-sys:interactive-interrupt ()))
    acc))
