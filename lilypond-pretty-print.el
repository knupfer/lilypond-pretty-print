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

(defun lilypond-string (begin times size)
  (let ((result nil))
    (while (> times 0)
      (cond ((= (mod begin size) 0)
             (setq result (concat result "|")))
            ((= (mod begin size) (/ size 4))
             (setq result (concat result ".")))
            ((= (mod begin size) (/ size 2))
             (setq result (concat result "+")))
            ((= (mod begin size) (/ (* size 3) 4))
             (setq result (concat result ".")))
            (t
             (setq result (concat result " "))))
      (setq begin (+ 1 begin)
            times (- times 1)))
    result))

(defun lilypond-beat-show ()
  (interactive)
  (unless (active-minibuffer-window)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward "| *$" (window-end) t)
        (save-excursion
          (let ((line (line-number-at-pos))
                (local-count 0)
                (increment 4))
            (back-to-indentation)
            (let ((time-passed nil)
                  (indentation-column (current-column))
                  (ov-count 0)
                  (size 16))
              (while (re-search-forward " +" (line-end-position) t)
                (setq time-passed (/ (* size 4 (car (get-beat)))
                                     (cadr (get-beat))))
                (when (and time-passed
                           (< 0 (+ indentation-column
                                   (- time-passed
                                      ov-count
                                      (current-column)))))
                  (lilypond--make-overlay
                   (+ -1 (point))
                   (lilypond-string (- (+ ov-count (current-column))
                                       indentation-column)
                                    (+ indentation-column
                                       (- time-passed
                                          ov-count
                                          (current-column)))
                                    size))
                  (setq ov-count
                        (+ indentation-column
                           (- time-passed (current-column)))))))))))))

(defun lilypond--make-overlay (line symbol)
  "draw line at (line, col)"
  (let ((original-pos (point)) ov prop)
    (save-excursion
      (goto-char line)
      (when (looking-at " ")
        (setq prop 'after-string
              ov (make-overlay (point) (+ 1 (point))))
        (when ov
          (overlay-put
           ov 'category 'lily-pretty)
          (overlay-put
           ov prop
           (propertize symbol 'face 'font-lock-comment-delimiter-face)))))))

(define-minor-mode lilypond-pretty-beat-mode
  :init-value nil
  :lighter "| . |"
  :global nil
  (if lilypond-pretty-beat-mode
      (progn
        (add-hook 'pre-command-hook 'lilypond-beat-remove nil t)
        (add-hook 'post-command-hook 'lilypond-beat-show nil t))
    (remove-hook 'pre-command-hook 'lilypond-beat-remove t)
    (remove-hook 'post-command-hook 'lilypond-beat-show t)))

(provide 'lilypond-pretty-print)

;;; lilypond-pretty-print.el ends here
