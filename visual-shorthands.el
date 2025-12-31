;;; visual-shorthands.el --- Visual abbreviations for symbol prefixes -*- lexical-binding: t -*-

;; Author: Gino Cornejo
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; Homepage: https://github.com/gggion/visual-shorthands.el
;; Keywords: convenience

;; Package-Version: 0.3.0
;; Package-Requires: ((emacs "29.1"))

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
;;     (visual-shorthands-add-mapping "application_config_manager_" "acm_")
;;     (visual-shorthands-mode 1)
;;
;; Abbreviates PREFIXES only, not whole symbols.

;;; Code:
(require 'cl-lib)

(defvar visual-shorthands-mode)  ; Forward declaration for byte-compiler

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
`always' means symbols are revealed when cursor enters them.
`on-change' means symbols are revealed only when buffer is modified."
  :type '(choice (const :tag "Always reveal on cursor" always)
          (const :tag "Only on buffer change" on-change))
  :group 'visual-shorthands)

(defcustom visual-shorthands-delay 0.0
  "Seconds of delay before revealing a symbol under cursor."
  :type 'number
  :group 'visual-shorthands)

(defvar-local visual-shorthands-alist nil
  "Buffer-local alist of visual shorthand mappings.
Each element has the form (LONGHAND . SHORTHAND) where LONGHAND
is the full prefix to replace and SHORTHAND is the visual replacement.
Longer prefixes should come first for correct matching.")

;; Survive buffer revert and major mode changes
(put 'visual-shorthands-alist 'permanent-local t)

(defvar-local visual-shorthands--prev-symbol nil
  "Previous symbol bounds that surrounded the cursor.")

(defvar-local visual-shorthands--timer nil
  "Current active timer for delayed reveal.")

(defvar-local visual-shorthands--do-reveal nil
  "Non-nil when mode is notified to start revealing.")

(defvar-local visual-shorthands--symbol-revealed nil
  "Non-nil if the last encountered symbol has been revealed.")

(defconst visual-shorthands--symbol-regexp
  "\\_<\\([[:alpha:]][[:alnum:]-_]*\\)\\_>"
  "Regexp matching Emacs Lisp symbol names.")

(defvar-local visual-shorthands--preview-overlays nil
  "List of overlays showing preview of shorthand mapping.
Used during interactive mapping creation to highlight matches.
Managed by `visual-shorthands--preview-cleanup' and
`visual-shorthands--preview-refresh'.")

(defface visual-shorthands-preview-face
  '((((class color) (min-colors 88) (background light))
     :background "#ffff00" :foreground "#000000")
    (((class color) (min-colors 88) (background dark))
     :background "#cdcd00" :foreground "#000000")
    (t :inverse-video t))
  "Face for preview overlays during interactive mapping creation.
Applied to longhand prefixes that will be hidden if mapping is confirmed."
  :group 'visual-shorthands)


(defun visual-shorthands--create-overlay (beg _end longhand shorthand)
  "Create overlay from BEG hiding LONGHAND and showing SHORTHAND.
Argument END is ignored; overlay covers only the LONGHAND prefix portion.
Uses invisible property on the longhand prefix and before-string for shorthand."
  (let* ((longhand-len (length longhand))
         ;; Overlay covers only the prefix that needs to be hidden
         (overlay (make-overlay beg (+ beg longhand-len) nil t nil))
         ;; Create propertized shorthand string with face
         (shorthand-string (propertize shorthand
                                       'face 'visual-shorthands-face)))
    ;; Make the longhand prefix invisible
    (overlay-put overlay 'invisible 'visual-shorthands)
    ;; Show the shorthand before the invisible text
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
            ;; Skip if in string or comment
            (unless (or (eq face-at-point 'font-lock-string-face)
                        (eq face-at-point 'font-lock-comment-face)
                        (eq face-at-point 'font-lock-doc-face))
              ;; Try each mapping
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
      ;; Add to invisibility spec
      (add-to-invisibility-spec 'visual-shorthands)
      (visual-shorthands--apply-to-region (point-min) (point-max)))))

(defun visual-shorthands--current-symbol ()
  "Return bounds of symbol at point if it has a visual shorthand overlay.
Returns (START . END) or nil."
  (when-let* ((symbol-bounds (bounds-of-thing-at-point 'symbol))
              (start (car symbol-bounds))
              (end (cdr symbol-bounds))
              (overlays (overlays-in start end)))
    (when (cl-some (lambda (ov) (overlay-get ov 'visual-shorthand)) overlays)
      symbol-bounds)))

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

      ;; If timer fired and expired
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
    (setq visual-shorthands--do-reveal nil)))

(defun visual-shorthands--after-change (&rest _args)
  "Signal that symbols in current buffer should be revealed on change."
  (setq visual-shorthands--do-reveal t))

(defun visual-shorthands--before-revert ()
  "Remove overlays before buffer revert.
Installed on `before-revert-hook' when mode is active."
  (when visual-shorthands-mode
    (remove-overlays (point-min) (point-max) 'visual-shorthand t)))

(defun visual-shorthands--after-revert ()
  "Reapply overlays after buffer revert.
Installed on `after-revert-hook' when mode is active."
  (when visual-shorthands-mode
    (visual-shorthands--apply-to-buffer)))

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
    (add-hook 'before-revert-hook #'visual-shorthands--before-revert nil t)
    (add-hook 'after-revert-hook #'visual-shorthands--after-revert nil t)
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
    (remove-hook 'before-revert-hook #'visual-shorthands--before-revert t)
    (remove-hook 'after-revert-hook #'visual-shorthands--after-revert t)
    (when (eq visual-shorthands-trigger 'on-change)
      (remove-hook 'after-change-functions #'visual-shorthands--after-change t))
    (setq visual-shorthands--prev-symbol nil
          visual-shorthands--symbol-revealed nil
          visual-shorthands--do-reveal nil))))

;;;; Preview Highlights
(defun visual-shorthands--preview-cleanup ()
  "Remove all preview overlays.
Called when exiting interactive mapping creation."
  (while visual-shorthands--preview-overlays
    (delete-overlay (car visual-shorthands--preview-overlays))
    (setq visual-shorthands--preview-overlays
          (cdr visual-shorthands--preview-overlays))))

(defun visual-shorthands--preview-refresh (longhand)
  "Refresh preview overlays for LONGHAND prefix.
Removes existing preview overlays and creates new ones for all
symbols in buffer starting with LONGHAND.

Returns count of matched symbols."
  (visual-shorthands--preview-cleanup)
  (when (and longhand (> (length longhand) 0))
    (let ((case-fold-search nil)
          (count 0)
          (re (concat "\\_<" (regexp-quote longhand) "\\(?:\\sw\\|\\s_\\)+")))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward re nil t)
            (let* ((symbol-start (match-beginning 0))
                   (face-at-point (get-text-property symbol-start 'face))
                   (longhand-end (+ symbol-start (length longhand))))
              ;; Skip strings and comments
              (unless (or (eq face-at-point 'font-lock-string-face)
                          (eq face-at-point 'font-lock-comment-face)
                          (eq face-at-point 'font-lock-doc-face))
                (let ((ov (make-overlay symbol-start longhand-end)))
                  (overlay-put ov 'face 'visual-shorthands-preview-face)
                  (overlay-put ov 'priority 1001)
                  (overlay-put ov 'visual-shorthands-preview t)
                  (push ov visual-shorthands--preview-overlays)
                  (setq count (1+ count))))))))
      count)))

(defun visual-shorthands--preview-minibuffer-setup ()
  "Set up preview overlay refresh on minibuffer input.
Installed on `minibuffer-setup-hook' during interactive mapping."
  (when (minibufferp)
    (add-hook 'after-change-functions
              #'visual-shorthands--preview-after-change
              nil t)))

(defun visual-shorthands--preview-after-change (&rest _)
  "Refresh preview overlays after minibuffer content changes.
Installed on buffer-local `after-change-functions' during interactive
mapping creation."
  (when (minibufferp)
    (let ((longhand (minibuffer-contents-no-properties)))
      (with-selected-window (or (minibuffer-selected-window)
                                (selected-window))
        (visual-shorthands--preview-refresh longhand)))))

;;;; Interactive functions

;;;###autoload
(defun visual-shorthands-add-mapping (longhand shorthand)
  "Add visual shorthand mapping from LONGHAND to SHORTHAND.

When called interactively, dynamically highlights symbols in buffer
matching LONGHAND as you type.  Highlighting uses
`visual-shorthands-preview-face' to show which prefix portions will
be hidden.

LONGHAND is the full prefix to replace.
SHORTHAND is the visual replacement.

Updates `visual-shorthands-alist' by adding or replacing mapping.
Mappings are sorted by descending LONGHAND length for correct matching.

If `visual-shorthands-mode' is active, immediately applies overlays
to all matching symbols.

Also see `visual-shorthands-remove-mapping' `visual-shorthands-clear-mappings'."
  (interactive
   (progn
     (add-hook 'minibuffer-setup-hook
               #'visual-shorthands--preview-minibuffer-setup)
     (unwind-protect
         (let* ((longhand-input
                 (read-string "Longhand prefix: "))
                (shorthand-input
                 (progn
                   (visual-shorthands--preview-cleanup)
                   (read-string (format "Shorthand for '%s': " longhand-input)))))
           (list longhand-input shorthand-input))
       (visual-shorthands--preview-cleanup)
       (remove-hook 'minibuffer-setup-hook
                    #'visual-shorthands--preview-minibuffer-setup))))

  ;; Update alist, maintaining sort order
  (setq visual-shorthands-alist
        (cons (cons longhand shorthand)
              (assoc-delete-all longhand visual-shorthands-alist)))
  (setq visual-shorthands-alist
        (sort visual-shorthands-alist
              (lambda (a b) (> (length (car a)) (length (car b))))))

  ;; Refresh if mode active
  (when visual-shorthands-mode
    (remove-overlays (point-min) (point-max) 'visual-shorthand t)
    (visual-shorthands--apply-to-buffer))

  (message "Added mapping: %s → %s" longhand shorthand))

;;;###autoload
(defun visual-shorthands-remove-mapping (longhand)
  "Remove visual shorthand mapping for LONGHAND."
  (interactive
   (list (completing-read "Remove mapping for: "
                          (mapcar #'car visual-shorthands-alist)
                          nil t)))
  (setq visual-shorthands-alist
        (assoc-delete-all longhand visual-shorthands-alist))
  (when visual-shorthands-mode
    (remove-overlays (point-min) (point-max) 'visual-shorthand t)
    (visual-shorthands--apply-to-buffer))
  (message "Removed mapping for: %s" longhand))

;;;###autoload
(defun visual-shorthands-clear-mappings ()
  "Clear all visual shorthand mappings."
  (interactive)
  (setq visual-shorthands-alist nil)
  (when visual-shorthands-mode
    (remove-overlays (point-min) (point-max) 'visual-shorthand t)
    (remove-from-invisibility-spec 'visual-shorthands))
  (message "Cleared all visual shorthand mappings"))

;;;###autoload
(defun visual-shorthands-setup (mappings)
  "Set up visual shorthands with MAPPINGS and enable mode."
  (setq visual-shorthands-alist
        (sort (copy-sequence mappings)
              (lambda (a b) (> (length (car a)) (length (car b))))))
  (visual-shorthands-mode 1))

(provide 'visual-shorthands)
;;; visual-shorthands.el ends here
