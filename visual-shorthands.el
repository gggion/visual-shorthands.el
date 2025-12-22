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
(require 'cl-lib)

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

(defcustom visual-shorthands-trigger 'always
  "Method of triggering element toggling.
`always' means that symbols are revealed when cursor enters them.
`on-change' means symbols are revealed only when buffer is modified.
`manual' means toggling starts on `visual-shorthands-manual-start'."
  :type '(choice (const :tag "Always reveal on cursor" always)
          (const :tag "Only on buffer change" on-change)
          (const :tag "Manual control" manual))
  :group 'visual-shorthands)

(defcustom visual-shorthands-delay 0.0
  "Seconds of delay before revealing a symbol under cursor."
  :type 'number
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

(defun visual-shorthands--current-symbol ()
  "Return bounds of symbol at point if it has a visual shorthand overlay.
Returns (START . END) or nil."
  (when-let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
              (start (car symbol-bounds))
              (end (cdr symbol-bounds))
              (overlays (overlays-in start end)))
    (when (cl-some (lambda (ov) (overlay-get ov 'visual-shorthand)) overlays)
      symbol-bounds)))

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

(defun visual-shorthands--apply-to-region (start end)
  "Apply visual shorthands to symbols between START and END."
  (when visual-shorthands-alist
    (save-excursion
      (goto-char start)
      (let ((case-fold-search nil)
            (limit (min end (point-max))))
        (while (re-search-forward visual-shorthands--symbol-regexp limit t)
          (let ((symbol-start (match-beginning 1))
                (symbol-end (match-end 1))
                (symbol-name (match-string-no-properties 1))
                (face-at-point (get-text-property (match-beginning 1) 'face)))
            (unless (or (eq face-at-point 'font-lock-string-face)
                        (eq face-at-point 'font-lock-comment-face)
                        (eq face-at-point 'font-lock-doc-face))
              (catch 'matched
                (dolist (mapping visual-shorthands-alist)
                  (let ((longhand (car mapping))
                        (shorthand (cdr mapping)))
                    (when (string-prefix-p longhand symbol-name)
                      (visual-shorthands--create-overlay
                       symbol-start symbol-end longhand shorthand)
                      (throw 'matched t))))))))))))

(defun visual-shorthands--apply-to-buffer ()
  "Apply visual shorthands to all matching symbols in current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (font-lock-ensure (point-min) (point-max))
      (add-to-invisibility-spec 'visual-shorthands)
      (visual-shorthands--apply-to-region (point-min) (point-max)))))

(defun visual-shorthands--reveal-symbol (symbol-bounds)
  "Reveal the symbol at SYMBOL-BOUNDS by removing invisibility."
  (let ((start (car symbol-bounds))
        (end (cdr symbol-bounds)))
    (dolist (ov (overlays-in start end))
      (when (overlay-get ov 'visual-shorthand)
        (overlay-put ov 'invisible nil)
        (overlay-put ov 'before-string nil)))))

(defun visual-shorthands--hide-symbol (symbol-bounds)
  "Hide the symbol at SYMBOL-BOUNDS by reapplying invisibility."
  (let ((start (car symbol-bounds))
        (end (cdr symbol-bounds)))
    (dolist (ov (overlays-in start end))
      (when-let ((data (overlay-get ov 'visual-shorthand-data)))
        (let ((shorthand (cdr data)))
          (overlay-put ov 'invisible 'visual-shorthands)
          (overlay-put ov 'before-string
                       (propertize shorthand 'face 'visual-shorthands-face)))))))

(defun visual-shorthands--reveal-with-lock (symbol-bounds &optional renew)
  "Reveal symbol at SYMBOL-BOUNDS.
When RENEW is non-nil, obtain symbol bounds at point instead."
  (when renew
    (setq symbol-bounds (visual-shorthands--current-symbol))
    (setq visual-shorthands--prev-symbol symbol-bounds)
    (setq visual-shorthands--timer nil))

  (when symbol-bounds
    (let ((start (car symbol-bounds))
          (end (cdr symbol-bounds)))
      (font-lock-ensure start end)
      (visual-shorthands--reveal-symbol symbol-bounds))))

(defun visual-shorthands--post-cmd ()
  "Handle cursor movement for auto-reveal functionality."
  (let* ((prev-symbol visual-shorthands--prev-symbol)
         (current-symbol (visual-shorthands--current-symbol)))

    ;; After leaving a symbol
    (when (and prev-symbol
               visual-shorthands--symbol-revealed
               (not (equal prev-symbol current-symbol)))

      (setq visual-shorthands--symbol-revealed nil)

      (if (not visual-shorthands--timer)
          (visual-shorthands--hide-symbol prev-symbol)
        (cancel-timer visual-shorthands--timer)
        (setq visual-shorthands--timer nil)))

    ;; Inside a symbol
    (when (and current-symbol
               (or (eq visual-shorthands-trigger 'always)
                   visual-shorthands--do-reveal
                   visual-shorthands--symbol-revealed))

      (setq visual-shorthands--symbol-revealed t)

      ;; New symbol, delay first reveal
      (when (and (eq visual-shorthands-trigger 'always)
                 (> visual-shorthands-delay 0)
                 (not (equal prev-symbol current-symbol)))
        (setq visual-shorthands--timer
              (run-with-idle-timer visual-shorthands-delay
                                   nil
                                   #'visual-shorthands--reveal-with-lock
                                   current-symbol
                                   t)))

      ;; Not a new symbol or no delay
      (when (not visual-shorthands--timer)
        (visual-shorthands--reveal-with-lock current-symbol)))

    (setq visual-shorthands--prev-symbol current-symbol)
    (when (not (eq visual-shorthands-trigger 'manual))
      (setq visual-shorthands--do-reveal nil))))

(defun visual-shorthands--after-change (&rest _args)
  "Signal that symbols in current buffer should be revealed on change."
  (setq visual-shorthands--do-reveal t))

;;;; Commands

;;;###autoload
(defun visual-shorthands-add-mapping (longhand shorthand)
  "Add visual shorthand mapping from LONGHAND to SHORTHAND."
  (interactive "sLonghand prefix: \nsShorthand replacement: ")
  (setq visual-shorthands-alist
        (cons (cons longhand shorthand)
              (assoc-delete-all longhand visual-shorthands-alist)))
  (setq visual-shorthands-alist
        (sort visual-shorthands-alist
              (lambda (a b) (> (length (car a)) (length (car b))))))
  (when visual-shorthands-mode
    (remove-overlays (point-min) (point-max) 'visual-shorthand t)
    (visual-shorthands--apply-to-buffer))
  (message "Added mapping: %s → %s" longhand shorthand))


;;;###autoload
(defun visual-shorthands-manual-start ()
  "Signal that symbols in current buffer should be revealed."
  (interactive)
  (setq visual-shorthands--do-reveal t))

;;;###autoload
(defun visual-shorthands-manual-stop ()
  "Signal that symbols should no longer be auto-revealed."
  (interactive)
  (when-let ((current-symbol (visual-shorthands--current-symbol)))
    (visual-shorthands--hide-symbol current-symbol)
    (setq visual-shorthands--symbol-revealed nil))
  (setq visual-shorthands--do-reveal nil))

;;;###autoload
(define-minor-mode visual-shorthands-mode
  "Toggle visual shorthand overlays with auto-reveal in current buffer.

When enabled, long symbol prefixes defined in
`visual-shorthands-alist' are visually replaced with shorter
alternatives using overlays with the `invisible' property.

Symbols are automatically revealed when the cursor enters them,
allowing normal character-by-character navigation through the
actual buffer text."
  :init-value nil
  :lighter " VS"
  :group 'visual-shorthands

  (cond
   (visual-shorthands-mode
    ;; Enable mode
    (visual-shorthands--apply-to-buffer)
    (add-hook 'post-command-hook #'visual-shorthands--post-cmd nil t)
    (when (eq visual-shorthands-trigger 'on-change)
      (add-hook 'after-change-functions #'visual-shorthands--after-change nil t)))

   (t
    ;; Disable mode - clean up
    (when-let ((current-symbol (visual-shorthands--current-symbol)))
      (visual-shorthands--reveal-symbol current-symbol)
      (when visual-shorthands--timer
        (cancel-timer visual-shorthands--timer)
        (setq visual-shorthands--timer nil)))
    (remove-overlays (point-min) (point-max) 'visual-shorthand t)
    (remove-from-invisibility-spec 'visual-shorthands)
    (remove-hook 'post-command-hook #'visual-shorthands--post-cmd t)
    (when (eq visual-shorthands-trigger 'on-change)
      (remove-hook 'after-change-functions #'visual-shorthands--after-change t))
    (setq visual-shorthands--prev-symbol nil
          visual-shorthands--symbol-revealed nil
          visual-shorthands--do-reveal nil))))

(provide 'visual-shorthands)
;;; visual-shorthands.el ends here
