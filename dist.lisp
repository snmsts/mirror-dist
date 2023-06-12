(ql:quickload '(:cl-toml :quickdist :dexador) :silent t)

(in-package :cl-user)

(handler-bind ((style-warning #'muffle-warning))
  (quickdist::def-print-method ((obj quickdist:system-info))
    "~A ~A ~A~{ ~A~}"
    (quickdist:get-project-name obj)
    (quickdist:get-filename obj)
    (quickdist:get-name obj)
    (quickdist:get-dependencies obj)))

(defvar *project-directory* nil)
(defvar *project-name*  nil)
(defvar *version* nil)
(defvar *sentinel* nil)
(defvar *distinfo* nil)

(defun tgz-file ()
  (merge-pathnames (format nil "~A-~A.tgz" *project-name* *version*) *project-directory*))

(defun sentinel-file ()
  (merge-pathnames (format nil "~A.sentinel" *project-name*) *project-directory*))

(defun old-sentinel ()
  (merge-pathnames (format nil "~A.old-sentinel" *project-name*) *project-directory*))

(defun project-subdir ()
  (merge-pathnames (format nil "~A/" *project-name*) *project-directory*)  )

(defun system-file ()
  (make-pathname :name *project-name* :type "system" :defaults *project-directory*))

(defun release-file ()
  (make-pathname :name *project-name* :type "release" :defaults *project-directory*))

(defun hash (keys &optional hash)
  (loop for k in keys
        do (setf hash (gethash k hash)))
  hash)

(defun ensure-sentinel-release (sentinel)
  (uiop:run-program (format nil "sh -c \"gh release create ~A --notes sentinel\""
                            sentinel)
                    :output :interactive
                    :error-output :interactive
                    :ignore-error-status t))

(defun ensure-version-release (version)
  (assert version)
  (uiop:run-program (format nil "gh release create ~A --notes ~A"
                            version version)
                    :output :interactive
                    :error-output :interactive
                    :ignore-error-status t))

(defun upload-files (version path-list)
  (assert version)
  (assert path-list)
  (uiop:run-program (format nil "gh release upload ~A ~{~A ~}--clobber"
                            version path-list)))

(defun map-toml (function)
  (loop for path in (uiop:directory-files *project-directory*)
        do (when (equal (pathname-type path) "toml")
             (let ((toml (cl-toml:parse-file path))
                   (*project-name* (pathname-name path)))
               (funcall function
                        :toml-path path
                        :toml toml
                        :disabled (not (not (hash '("disable") toml))))))))

(defun download-sentinels (config)
  (assert (hash '("upload" "github") config))
  (map-toml
   (lambda (&key disabled &allow-other-keys)
     (unless disabled
       (let* ((base-uri (format nil "https://github.com/~A/releases/download/~A/"
                                (hash '("upload" "github") config)
                                *sentinel*))
              (result (ignore-errors
                        (dex:fetch (format nil "~A~A.sentinel" base-uri *project-name*)
                                   (old-sentinel)
                                   :if-exists :supersede))))
         (log:info "download old sentinel ~A: ~A" 
                   *project-name* result))))))
                              
(defvar *checkout-handlers*
  '(("github" . github-checkout)))

(defun github-checkout (path toml)
  (assert (hash '("checkout" "where") toml))
  (log:info "checkout ~A" path)
  (uiop:run-program (format nil
                            "git clone --depth 1 https://github.com/~A ~A"
                            (hash '("checkout" "where") toml)
                            (uiop:native-namestring (project-subdir)))))

(defun assoc-call (assoc toml-path toml)
  (let ((function (cdr (assoc (hash '("checkout" "from") toml) assoc
                              :test 'equal))))
    (assert function)
    (funcall function toml-path toml)))

(defun check-updates (config)
  (declare (ignorable config))
  (map-toml 
   (lambda (&key toml-path toml disabled &allow-other-keys)
     (assert (hash '("checkout" "from") toml))
     (unless disabled
       (assoc-call *checkout-handlers* toml-path toml)))))

(defun create-systems ()
  (let ((registry-path (uiop:native-namestring *project-directory*)))
    (asdf:initialize-source-registry registry-path))
  (map-toml 
   (lambda (&key toml disabled &allow-other-keys)
     (let ((quickdist:*project-path* (project-subdir)))
       (unless disabled
         (log:info "Processing create-dist" *project-name*)
         (with-open-file (system-index (system-file)
                                       :direction :output :if-exists :supersede)
           (with-simple-restart (skip-project "Skip project ~S, continue with the next."
                                              quickdist:*project-path*)
             (let* ((systems-info (quickdist:make-systems-info quickdist:*project-path*
                                                               :ignore-filename-p (if (ignore-errors (hash '("system" "ignore-filename") toml))
                                                                                      (eval (read-from-string (hash '("system" "ignore-filename") toml)))
                                                                                      quickdist:*default-ignore-filename-p*)
                                                               :ignore-dependency-p quickdist:*default-ignore-dependency-p*)))
               (loop for system-info in systems-info
                     do (write system-info
                               :stream system-index
                               :pretty nil))))))))))

(defvar *commit-handlers*
  '(("github" . git-commit)))

(defun git-commit (toml-path toml)
  (declare (ignore toml toml-path))
  (uiop:run-program
   (format nil "sh -c \"cd ~A;git rev-parse HEAD\""
           (uiop:native-namestring (project-subdir)))
   :output '(:string :stripped t)
   :ignore-error-status t))

(defun create-sentinels ()
  (map-toml
   (lambda (&key toml-path toml disabled &allow-other-keys)
     (unless disabled
       (let ((commit (assoc-call *commit-handlers* toml-path toml)))
         (with-open-file (o (sentinel-file)
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
           (let ((hash (make-hash-table :test 'equal)))
             (setf (gethash "name" hash) *project-name*)
             (setf (gethash "commit" hash) commit)
             (setf (gethash "version" hash) *version*)
             (cl-toml:encode hash o))))))))

(defun create-archives ()
  (map-toml
   (lambda (&key disabled &allow-other-keys)
     (unless disabled
       (when (or (not (uiop:file-exists-p (old-sentinel)))
                 (not (equal (hash '("commit") (cl-toml:parse-file (old-sentinel)))
                             (hash '("commit") (cl-toml:parse-file (sentinel-file))))))
         (uiop:run-program
          (format nil "sh -c \"cd ~A;git archive HEAD --format=tar.gz --prefix=~A/ > ~A\""
                  (uiop:native-namestring (project-subdir))
                  (format nil "~A-~A" *project-name* *version*)
                  (uiop:native-namestring (tgz-file)))
          :ignore-error-status t))))))

(defun create-releases ()
  (map-toml
   (lambda (&key disabled &allow-other-keys)
     (unless disabled
       (when (uiop:file-exists-p (tgz-file))
         (let ((h (make-hash-table :test 'equal)))
           (with-open-file (o (release-file)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
             (setf (gethash "project-name" h) *project-name*
                   (gethash "version" h) *version*
                   (gethash "archive-path" h) (file-namestring (tgz-file))
                   (gethash "file-size" h) (ql::file-size (tgz-file))
                   (gethash "md5sum" h) (quickdist::md5sum (tgz-file))
                   (gethash "content-sha1" h) (quickdist::tar-content-sha1 (tgz-file)))
             (cl-toml:encode h o))))))))

(defun upload-archives ()
  (map-toml 
   (lambda (&key disabled &allow-other-keys)
     (unless disabled
       (let* ((system (system-file))
              (tgz (tgz-file)))
         (log:info "uploading" tgz (uiop:file-exists-p tgz))
         (when (uiop:file-exists-p tgz)
           (ensure-version-release *version*)
           (upload-files *version* (list tgz (sentinel-file) (release-file) system))
           (ensure-sentinel-release *sentinel*)
           (upload-files *sentinel*  (list (sentinel-file)))))))))

(defun version (config &key version set file)
  (when (uiop:file-exists-p file)
    (setf version
          (hash '("version") (cl-toml:parse-file file))))
  (let ((result
          (or version
              *version*
              (uiop:getenv "VERSION")
              (funcall (if (hash '("projects" "version") config)
                           (eval (read-from-string (hash '("projects" "version") config)))
                           (lambda (universal-time)
                             (let* ((time (multiple-value-list (decode-universal-time universal-time)))
                                    (timestamp (reverse (subseq time 0 6))))
                               (format nil "~{~2,'0d~}" timestamp))))
                       (get-universal-time)))))
    (if set
        (setf *version* result)
        result)))

(defun call-with-env (function)
  (let* ((config (cl-toml:parse-file "./config.toml"))
         (*version* (version config :set t))
         (*sentinel* (or (ignore-errors (hash '("upload" "sentinel-release") config)) "sentinels"))
         (*distinfo* (or (ignore-errors (hash '("upload" "distinfo-release") config)) "distinfo"))
         (*project-directory* 
           (merge-pathnames (or (hash '("projects" "dir") config)
                                "projects/") (uiop:getcwd))))
    (funcall function config)))

(defun mirror ()
  (call-with-env
   (lambda (config)
     (download-sentinels config)
     (check-updates config)
     (create-systems)
     (create-sentinels)
     (create-archives)
     (create-releases)
     )))

(defun upload ()
  (call-with-env
   (lambda (config)
     (declare (ignore config))
     (upload-archives))))

(defun clean ()
  (call-with-env
   (lambda (config)
     (declare (ignorable config))
     (map-toml
      (lambda (&key toml disabled &allow-other-keys)
        (declare (ignorable toml disabled))
        (ignore-errors
          (uiop:delete-directory-tree (project-subdir) :validate t))
        (mapc 'uiop:delete-file-if-exists (list (sentinel-file) (old-sentinel) (system-file)))))
     (mapc 'uiop:delete-file-if-exists (uiop:directory-files *project-directory* #P"*.tgz")))))

