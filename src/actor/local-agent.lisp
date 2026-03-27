;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Local agent.  Eval in hemlock's own image on the shared dispatcher.

(in-package :hemlock.actor)

(defvar *local-agent* nil
  "The local eval agent actor.")

(defun start-local-agent ()
  "Create a local agent for evaluating code in hemlock's own image."
  (setf *local-agent*
        (ac:actor-of *actor-system*
          :name "local-agent"
          :receive
          (lambda (msg)
            (case (car msg)
              (:eval
               (let ((form-string (cadr msg)))
                 (handler-case
                     (let* ((form (read-from-string form-string))
                            (values (multiple-value-list (eval form)))
                            (result (format nil "~{~S~^~%~}" values)))
                       (reply (list :ok result)))
                   (error (c)
                     (reply (list :error (princ-to-string c)))))))

              (:compile
               (let ((text (cadr msg))
                     (package-name (caddr msg)))
                 (handler-case
                     (let ((*package* (or (and package-name
                                              (find-package package-name))
                                         *package*)))
                       (eval (read-from-string text))
                       (reply (list :ok "compiled")))
                   (error (c)
                     (reply (list :error (princ-to-string c)))))))

              (:set-package
               (let* ((name (cadr msg))
                      (pkg (find-package name)))
                 (if pkg
                     (progn (setf *package* pkg)
                            (reply (list :ok (package-name pkg))))
                     (reply (list :error
                                  (format nil "No package ~S" name))))))

              (:ping (reply :pong))
              (:shutdown (reply :ok))
              (t (reply (list :error :unknown-message (car msg))))))))

  (register-agent "local" :local *local-agent*))
