(defvar *insert-to-init-lisp*
  "(ql:quickload :dexador :silent t)
(defpackage quicklisp-https
  (:use :cl)
  (:export :setup))

(in-package :quicklisp-https)

(defun fetch-via-dexador (url file &key (follow-redirects t) quietly (maximum-redirects 10))
  \"Request URL and write the body of the response to FILE.\"
  (declare (ignorable follow-redirects quietly maximum-redirects))
  (dex:fetch (ql-http::urlstring (ql-http:url url)) file
             :if-exists :supersede)
  (values (make-instance 'ql-http::header :status 200)
          (probe-file file)))

(defun setup (&key overwrite (method \"https\"))
  (when (or (not (find method ql-http:*fetch-scheme-functions* :test 'equal :key 'first))
              overwrite)
      (setf ql-http:*fetch-scheme-functions*
            (acons method 'fetch-via-dexador
                   (remove method ql-http:*fetch-scheme-functions* :key 'first :test 'equal)))))
(setup)")

(or
 #+quicklisp
 (if (assoc "https" ql-http:*fetch-scheme-functions* :test 'equal)
     (progn
       (format t "this quicklisp already support https protocol.nothing to be done.~%")
       t)
     (let ((path (ql:qmerge (format nil "local-init/https.lisp"))))
       (with-open-file (o (ensure-directories-exist
                           (ql:qmerge (format nil "local-init/https.lisp")))
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
         (write-line *insert-to-init-lisp* o)
         (format t "patch applied to ~A~%" path)
         t)))
 (format t "no quicklisp installed.install quicklisp first!~%"))

(uiop:quit)


