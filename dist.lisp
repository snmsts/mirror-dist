(ql:quickload '(:cl-toml :quickdist :dexador) :silent t)

(in-package :cl-user)

(defvar *hash* nil)
(defvar *project-directory* nil)
(defvar *version* nil)
(defvar *sentinel* nil)

(defun hash (keys &optional (hash *hash*))
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
                    :error-output :interactive))

(defun upload-files (version path-list)
  (assert version)
  (assert path-list)
  (uiop:run-program (format nil "gh release upload ~A ~{~A ~}--clobber"
                            version path-list)))

(defun map-toml (function)
  (loop for path in (uiop:directory-files *project-directory*)
        do (when (equal (pathname-type path) "toml")
             (let ((toml (cl-toml:parse-file path)))
               (funcall function path
                        :toml toml
                        :disabled (not (not (hash '("disable") toml))))))))

(defun download-sentinels (config)
  (assert (hash '("upload" "github") config))
  (map-toml
   (lambda (toml-path &key disabled &allow-other-keys)
     (unless disabled
       (let* ((base-uri (format nil "https://github.com/~A/releases/download/~A/"
                                (hash '("upload" "github") config)
                                *sentinel*))
              (project-name (pathname-name toml-path))
              (result (ignore-errors
                        (dex:fetch (format nil "~A~A.sentinel" base-uri (pathname-name toml-path))
                                   (merge-pathnames (format nil "~A.old-sentinel" (pathname-name toml-path)) *project-directory*)
                                   :if-exists :supersede))))
         (log:info "download old sentinel ~A: ~A" 
                   project-name result))))))
                              
(defvar *checkout-handlers*
  '(("github" . github-checkout)))

(defun github-checkout (path toml)
  (assert (hash '("checkout" "where") toml))
  (log:info "checkout ~A" path)
  (uiop:run-program (format nil
                            "git clone --depth 1 https://github.com/~A ~A~A"
                            (hash '("checkout" "where") toml)
                            (uiop:native-namestring *project-directory*)
                            (pathname-name path))))

(defun assoc-call (assoc toml-path toml)
  (let ((function (cdr (assoc (hash '("checkout" "from") toml) assoc
                              :test 'equal))))
    (assert function)
    (funcall function toml-path toml)))

(defun check-updates (config)
  (declare (ignorable config))
  (map-toml 
   (lambda (toml-path &key toml disabled &allow-other-keys)
     (assert (hash '("checkout" "from") toml))
     (unless disabled
       (assoc-call *checkout-handlers* toml-path toml)))))

(defun create-systems ()
  (let ((registry-path (uiop:native-namestring *project-directory*)))
    (asdf:initialize-source-registry registry-path))
  (map-toml 
   (lambda (toml-path &key toml disabled &allow-other-keys)
     (let* ((project-name (pathname-name toml-path))
            (quickdist:*project-path* (merge-pathnames (format nil "~A/" project-name) *project-directory*)))
       (unless disabled
         (log:info "Processing create-dist" project-name)
         (with-open-file (system-index (make-pathname :name project-name :type "system" :defaults *project-directory*)
                                       :direction :output :if-exists :supersede)
           (write-line "# project system-file system-name [dependency1..dependencyN]" system-index)
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
  (declare (ignore toml))
  (let ((project-name (pathname-name toml-path)))
    (uiop:run-program
     (format nil "sh -c \"cd ~A;git rev-parse HEAD\""
             (uiop:native-namestring (merge-pathnames (format nil "~A/" project-name) *project-directory*)))
     :output '(:string :stripped t)
     :ignore-error-status t)))

(defun create-sentinels ()
  (map-toml
   (lambda (toml-path &key toml disabled &allow-other-keys)
     (let ((project-name (pathname-name toml-path)))
       (unless disabled
         (let ((commit (assoc-call *commit-handlers* toml-path toml)))
           (with-open-file (o (merge-pathnames 
                               (format nil "~A.sentinel" project-name)
                               *project-directory*)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
             (let ((hash (make-hash-table :test 'equal)))
               (setf (gethash "name" hash) project-name)
               (setf (gethash "commit" hash) commit)
               (setf (gethash "version" hash) *version*)
               (cl-toml:encode hash o)))))))))

(defun create-archives ()
  (map-toml
   (lambda (toml-path &key disabled &allow-other-keys)
     (let* ((project-name (pathname-name toml-path))
            (old-sentinel (merge-pathnames (format nil "~A.old-sentinel" project-name) *project-directory*))
            (sentinel (merge-pathnames (format nil "~A.sentinel" project-name) *project-directory*)))
       (unless disabled
         (when (or (not (uiop:file-exists-p old-sentinel))
                   (not (equal (hash '("commit") (cl-toml:parse-file old-sentinel))
                               (hash '("commit") (cl-toml:parse-file sentinel)))))
           (uiop:run-program
            (format nil "sh -c \"cd ~A;git archive HEAD --format=tar.gz --prefix=~A/ > ../~A.tgz\""
                    (uiop:native-namestring (merge-pathnames (format nil "~A/" project-name) *project-directory*))
                    (format nil "~A-~A" project-name *version*)
                    project-name)
            :ignore-error-status t)))))))

(defun upload-archives ()
  (map-toml 
   (lambda (toml-path &key disabled &allow-other-keys)
     (unless disabled
       (let* ((project-name (pathname-name toml-path))
              (tgz (merge-pathnames (format nil "~A.tgz" project-name) *project-directory*))
              (sentinel (merge-pathnames (format nil "~A.sentinel" project-name) *project-directory*))
              (system (merge-pathnames (format nil "~A.system" project-name) *project-directory*))
              (version (hash '("version") (cl-toml:parse-file sentinel))))
         (log:info "uploading" tgz (uiop:file-exists-p tgz))
         (when (uiop:file-exists-p tgz)
           (ensure-version-release version)
           (upload-files version (list tgz sentinel system))
           (ensure-sentinel-release *sentinel*)
           (upload-files *sentinel*  (list sentinel))))))))

(defun call-with-env (function)
  (let* ((config (cl-toml:parse-file "./config.toml"))
         (*version* (funcall (if (hash '("projects" "version") config)
                                 (eval (read-from-string (hash '("projects" "version") config)))
                                 (lambda (universal-time)
                                   (let* ((time (multiple-value-list (decode-universal-time universal-time)))
                                          (timestamp (reverse (subseq time 0 6))))
                                     (format nil "~{~2,'0d~}" timestamp))))
                             (get-universal-time)))
         (*sentinel* (or (ignore-errors (hash '("upload" "sentinel-release") config)) "sentinels"))
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
     (upload-archives))))

(defun clean ()
  (call-with-env
   (lambda (config)
     (declare (ignorable config))
     (map-toml 
      (lambda (toml-path &key toml disabled &allow-other-keys)
        (declare (ignorable toml disabled))
        (let* ((project-name (pathname-name toml-path))
               (dir (merge-pathnames (format nil "~A/" project-name) *project-directory*)))
          (ignore-errors(uiop:delete-directory-tree dir :validate t))
          (loop for i in '("~A.old-sentinel"
                           "~A.system"
                           "~A.sentinel"
                           "~A.tgz")
                do (uiop:delete-file-if-exists
                    (merge-pathnames (format nil i project-name) *project-directory*)))))))))
