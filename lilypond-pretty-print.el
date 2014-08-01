;;; lilypond-pretty-print.el --- visualize durations in source code

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer

;;; Code:

(defun lilypond-pretty-print ()
  (interactive)
  (save-excursion
    (indent-according-to-mode)
    (back-to-indentation)
    (when (re-search-forward "[^ ]\\([| ]*$\\)" (line-end-position) t)
      (replace-match " |" nil nil nil 1)
      (back-to-indentation))
    (let ((time-passed nil)
          (indentation-column (current-column)))
      (while (re-search-forward " +" (line-end-position) t)
        (goto-char (match-end 0))
        (replace-match " ")
        (setq time-passed (/ (* 33 (car (get-beat)))
                             (cadr (get-beat))))
        (when time-passed
          (while (> time-passed (- (current-column) indentation-column))
            (insert " "))))))
  ;; make-string LENGTH INIT
  (forward-line))

(defun lilypond-beat-remove ()
  (dolist (ov (lilypond-beat--active-overlays))
    (delete-overlay ov)))

(defun lilypond-beat--active-overlays ()
  (delq nil (mapcar
             (lambda (ov)
               (and (eq (overlay-get ov 'category)
                        'lily-pretty)
                    ov))
             (overlays-in (point-min) (point-max)))))

(defun lilypond-beat-show ()
  (unless (active-minibuffer-window)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward "|" (window-end) t)
        (save-excursion
          (let ((line (line-number-at-pos))
                (local-count 0)
                (increment 4))
            (back-to-indentation)
            (while (and (eq line (line-number-at-pos))
                        (< (+ increment (point)) (window-end)))
              (cond ((= (mod local-count 4) 0)
                     (lilypond--make-overlay (point) "|"))
                    ((= (mod local-count 4) 2)
                     (lilypond--make-overlay (point) "+"))
                    ((= (mod local-count 4) 1)
                     (lilypond--make-overlay (point) "."))
                    ((= (mod local-count 4) 3)
                     (lilypond--make-overlay (point) ".")))
              (setq local-count (+ 1 local-count))
              (forward-char increment))))))))

(defun lilypond--make-overlay (line symbol)
  "draw line at (line, col)"
  (let ((original-pos (point)) ov prop)
    (save-excursion
      (goto-char line)
      (when (looking-at " ")
        (setq prop 'display
              ov (make-overlay (point) (+ 1 (point))))
        (when ov
          (overlay-put
           ov 'category 'lily-pretty)
          (overlay-put
           ov prop
           (propertize symbol 'face 'warning)))))))

(define-minor-mode lilypond-pretty-beat-mode
  :init-value nil
  :lighter ".+.|.+."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-q")
              'lilypond-pretty-print)
            map)
  :global nil
  (if lilypond-pretty-beat-mode
      (progn
        (add-hook 'pre-command-hook 'lilypond-beat-remove nil t)
        (add-hook 'post-command-hook 'lilypond-beat-show nil t))
    (remove-hook 'pre-command-hook 'lilypond-beat-remove t)
    (remove-hook 'post-command-hook 'lilypond-beat-show t)))

(provide 'lilypond-pretty-print)

;;; lilypond-pretty-print.el ends here
