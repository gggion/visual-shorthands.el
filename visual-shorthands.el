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

(defvar-local visual-shorthands-alist nil
  "Alist of (LONGHAND-PREFIX . SHORTHAND-PREFIX) pairs.
Automatically sorted by prefix length (longest first).")

;;;; Core Implementation

(defun visual-shorthands--symbol-regexp ()
  "Return regexp matching symbols in current syntax table."
  "\\(?:\\sw\\|\\s_\\)+")

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

(defun visual-shorthands--create-overlay (start longhand shorthand)
  "Create overlay at START hiding LONGHAND, showing SHORTHAND.

Uses `invisible' property instead of `display' to allow cursor
navigation through actual text.  See Info node `(elisp)Invisible Text'."
  (let ((ov (make-overlay start (+ start (length longhand)) nil t nil)))
    (overlay-put ov 'invisible 'visual-shorthands)
    (overlay-put ov 'before-string shorthand)
    (overlay-put ov 'vs-shorthand t)
    (overlay-put ov 'vs-data (cons longhand shorthand))
    (overlay-put ov 'evaporate t)
    ov))

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
