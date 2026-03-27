(require :asdf)

#-ocicl
(let ((p (merge-pathnames ".local/share/ocicl/ocicl-runtime.lisp"
                          (user-homedir-pathname))))
  (when (probe-file p) (load p)))

(asdf:initialize-source-registry
 (list :source-registry
       (list :directory (uiop:getcwd))
       (list :directory (merge-pathnames "vendor/cl-gserver/" (uiop:getcwd)))
       :inherit-configuration))
