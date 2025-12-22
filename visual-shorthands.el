;;; visual-shorthands.el --- Visual abbreviations for symbol prefixes -*- lexical-binding: t -*-

;; Author: Gino Cornejo
;; Mantainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/visual-shorthands
;; Keywords: hypermedia vc

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs  "29.1") (compat "30.1") (magit "4.3") (org "9.7") (orgit "2.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Replace long prefixes with short ones visually using overlays.
;; Example: "application_config_manager_" -> "acm_"
;;
;; Basic usage:
;;
;;     (visual-shorthands-add "application_config_manager_" "acm_")
;;     (visual-shorthands-mode 1)
;;
;; Abbreviates PREFIXES only, not whole symbols.

;;; Code:

(defgroup visual-shorthands nil
  "Visual shorthand overlays for improved code readability."
  :group 'convenience
  :prefix "visual-shorthands-")

(defface visual-shorthands-face
  '((((class color) (min-colors 88) (background light))
     :inherit elisp-shorthand-font-lock-face
     :background "#e0e0e0"
     :extend nil)
    (((class color) (min-colors 88) (background dark))
     :inherit elisp-shorthand-font-lock-face
     :background "#404040"
     :extend nil)
    (t :inherit elisp-shorthand-font-lock-face
       :box (:line-width -1)))
  "Face for visual shorthand overlays.
Inherits from `elisp-shorthand-font-lock-face' but adds a subtle
background to distinguish visual shorthands from real shorthands.
Only applied to the shorthand prefix portion of symbols."
  :group 'visual-shorthands)

;;;; Internal variables

(defvar-local visual-shorthands-alist nil
  "Alist of (LONGHAND-PREFIX . SHORTHAND-PREFIX) pairs.
Automatically sorted by prefix length (longest first).")

(defvar-local visual-shorthands--prev-symbol nil
  "Previous symbol bounds that surrounded the cursor.")

(defvar-local visual-shorthands--timer nil
  "Current active timer for delayed reveal.")

(defvar-local visual-shorthands--do-reveal nil
  "Non-nil when mode is notified to start revealing.")

(defvar-local visual-shorthands--symbol-revealed nil
  "Non-nil if the last encountered symbol has been revealed.")

;;;; Core Implementation
(defconst visual-shorthands--symbol-regexp
  "\\_<\\([[:alpha:]][[:alnum:]-_]*\\)\\_>"
  "Regexp matching Emacs Lisp symbol names.")

;; (defun visual-shorthands--symbol-regexp ()
;;   "Return regexp matching symbols in current syntax table."
;;   "\\(?:\\sw\\|\\s_\\)+")

(defun visual-shorthands--face-at (pos)
  "Return face property at POS, handling both atoms and lists."
  (let ((face (get-text-property pos 'face)))
    (if (listp face) face (list face))))

(defun visual-shorthands--in-string-or-comment-p (pos)
  "Return non-nil if POS is inside string or comment."
  (let ((faces (visual-shorthands--face-at pos)))
    (or (memq 'font-lock-string-face faces)
        (memq 'font-lock-comment-face faces)
        (memq 'font-lock-doc-face faces))))

(defun visual-shorthands--create-overlay (beg end longhand shorthand)
  "Create overlay from BEG to END hiding LONGHAND and showing SHORTHAND.
Uses invisible property on the longhand prefix and before-string for shorthand."
  (let* ((longhand-len (length longhand))
         (shorthand-len (length shorthand))
         (overlay (make-overlay beg (+ beg longhand-len) nil t nil))
         (shorthand-string (propertize shorthand
                                       'face 'visual-shorthands-face)))
    (overlay-put overlay 'invisible 'visual-shorthands)
    (overlay-put overlay 'before-string shorthand-string)
    (overlay-put overlay 'visual-shorthand t)
    (overlay-put overlay 'visual-shorthand-data (cons longhand shorthand))
    (overlay-put overlay 'evaporate t)
    overlay))

(defun visual-shorthands--apply ()
  "Apply abbreviations to all symbols in buffer.

Skips strings and comments by checking `font-lock-face' property.
Requires `font-lock-ensure' to have run first."
  (when visual-shorthands-alist
    (save-excursion
      (save-restriction
        (widen)
        (font-lock-ensure (point-min) (point-max))
        (add-to-invisibility-spec 'visual-shorthands)
        (goto-char (point-min))
        (let ((case-fold-search nil)
              (regexp (visual-shorthands--symbol-regexp)))
          (while (re-search-forward regexp nil t)
            (let ((start (match-beginning 0))
                  (name (match-string-no-properties 0)))
              (unless (visual-shorthands--in-string-or-comment-p start)
                (catch 'done
                  (dolist (pair visual-shorthands-alist)
                    (when (string-prefix-p (car pair) name)
                      (visual-shorthands--create-overlay
                       start (car pair) (cdr pair))
                      (throw 'done t))))))))))))

;;;; Commands

;;;###autoload
(defun visual-shorthands-add (longhand shorthand)
  "Add mapping from LONGHAND prefix to SHORTHAND prefix.

Example: (visual-shorthands-add \"long_prefix_\" \"lp_\")

Abbreviates PREFIXES only:
  long_prefix_function -> lp_function  (works)
  new_name -> nn  (does not work)"
  (interactive "sLonghand prefix: \nsShorthand: ")
  (setq-local visual-shorthands-alist
              (cons (cons longhand shorthand)
                    (assoc-delete-all longhand visual-shorthands-alist)))
  ;; Sort by length descending so longest prefixes match first
  (setq-local visual-shorthands-alist
              (sort visual-shorthands-alist
                    (lambda (a b) (> (length (car a)) (length (car b))))))
  (when visual-shorthands-mode
    (visual-shorthands-mode -1)
    (visual-shorthands-mode 1))
  (message "Added: %s → %s" longhand shorthand))

;;;###autoload
(define-minor-mode visual-shorthands-mode
  "Visually abbreviate symbol prefixes.

Abbreviates prefixes defined in `visual-shorthands-alist'.
Buffer content remains unchanged - this is display-only."
  :lighter " VS"
  (if visual-shorthands-mode
      (visual-shorthands--apply)
    (remove-overlays (point-min) (point-max) 'vs-shorthand t)
    (remove-from-invisibility-spec 'visual-shorthands)))

(provide 'visual-shorthands)
;;; visual-shorthands.el ends here
