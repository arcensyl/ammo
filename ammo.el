;;; ammo.el - Automatic function runner for Emacs -*- lexical-binding: t; -*-

;; Author: Arcensyl <dev@arcensyl.me>
;; URL: https://github.com/arcensyl/ammo
;; Created: November 15, 2025
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;;; Commentary:

;;; Code:

;; TODO: Write documentation.

(require 'cl-lib)

(defgroup ammo nil
  ""
  :group 'convenience
  :prefix "ammo-")

(defvar ammo--triggers (make-hash-table :test #'eq))

(cl-defstruct ammo--trigger
  (active-state nil :type boolean)
  (logic 'and :type symbol)
  (conditions '() :type list)
  (activate-fn nil :type function)
  (deactivate-fn nil :type function))

(cl-defun ammo--check-conditions (trigger)
  (unless (ammo--trigger-conditions trigger)
    (cl-return t))
  
  (cond ((eq (ammo--trigger-logic trigger) 'and)
         (cl-loop for condition in (ammo--trigger-conditions trigger)
                  when (null (ignore-errors (funcall condition)))
                  return nil
                  finally return t))
        ((eq (ammo--trigger-logic trigger) 'or)
         (cl-loop for condition in (ammo--trigger-conditions trigger)
                  when (ignore-errors (funcall condition))
                  return t
                  finally return nil))
        (t nil)))

;;;###autoload
(defun ammo-check-trigger (trigger)
  (interactive (list (completing-read "Check trigger: "
                                      (hash-table-keys ammo--triggers)
                                      nil t)))
  (when (called-interactively-p)
    (setq trigger (intern trigger)))
  
  (when-let ((actual-trigger (gethash trigger ammo--triggers)))
    (if (ammo--check-conditions actual-trigger)
        (message "The conditions of trigger '%s' are met!" trigger)
      (message "The conditions of trigger '%s' are not met!" trigger))))

(provide 'ammo)
;;; ammo.el ends here
