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


(defvar lilypond-pretty-print-size 16)
(defvar lilypond-fill-column 72)

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

(defun lilypond-string (begin times)
  (let ((result nil))
    (while (> times 0)
      (let ((col (lilypond-make-hex (elt times-used
                                         (min (- (length times-used) 1)
                                              begin)))))
        (setq result (concat result
                             (eval `(propertize "|"
                                                'face '(:foreground ,col))))))
      (setq begin (+ 1 begin)
            times (- times 1)))
    result))

(defun lilypond-analyse-metrum ()
  (interactive)
  (setq times-used (make-vector 128 0))
  (setq lilypond-measure-length nil)
  (let ((time-passed 0)
        (size lilypond-pretty-print-size))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "| *$" nil t)
      (save-excursion
        (while (re-search-forward "| *$" nil t)
          (forward-char -1)
          (setq lilypond-measure-length
                (cons (/ (* 144 (car (get-beat)))
                         (cadr (get-beat))) lilypond-measure-length))
          (forward-char 1)))
      (sort lilypond-measure-length '<)
      (setq debug123 lilypond-measure-length)
      (let ((old-length (car lilypond-measure-length))
            (local-count 0)
            (result-count 0)
            (result))
        (while lilypond-measure-length
          (if (and (cadr lilypond-measure-length)
                   (eq old-length (car lilypond-measure-length)))
              (setq local-count (+ 1 local-count))
            (when (> local-count result-count)
              (setq result-count local-count
                    result old-length)
              (setq old-length (car lilypond-measure-length))
              (setq local-count 1)))
          (pop lilypond-measure-length))
        (setq lilypond-measure-length result))
      (setq lilypond-pretty-print-size (/ (* lilypond-fill-column 144) lilypond-measure-length)
            size lilypond-pretty-print-size)
      (while (re-search-forward "| *$" nil t)
        (save-excursion
          (back-to-indentation)
          (while (re-search-forward " +" (line-end-position) t)
            (when (and (not (looking-at "r"))
                       (not (= time-passed (/ (* size (car (get-beat)))
                                              (cadr (get-beat)))))
                       (< (/ (* size (car (get-beat)))
                             (cadr (get-beat))) 128))
              (setq time-passed (/ (* size (car (get-beat)))
                                   (cadr (get-beat))))
              (aset times-used time-passed (+ 1 (elt times-used time-passed)))))
          (aset times-used time-passed (+ -1 (elt times-used time-passed)))))))
  (let ((beat-max 1))
    (mapc (lambda (x) (setq beat-max (max beat-max (* x x)))) times-used)
    (setq times-used
          (map 'vector (lambda (y) (max 0 (/ (* 255 (* y y)) beat-max))) times-used))))

(defun lilypond-beat-show ()
  (interactive)
  (unless (active-minibuffer-window)
    (save-excursion
      (let ((win-min (window-start)))
        (goto-char (point-min))
        (re-search-forward "| *$" nil t)
        (if (< (point) win-min)
            (goto-char win-min)
          (if (> (point) (window-end))
              (error "no bar")
            (forward-line))))
      (while (re-search-forward "| *$" (window-end) t)
        (save-excursion
          (let ((line (line-number-at-pos))
                (local-count 0)
                (increment 4))
            (back-to-indentation)
            (let ((time-passed nil)
                  (indentation-column (current-column))
                  (ov-count 0)
                  (size lilypond-pretty-print-size)
                  (current-pos nil))
              (while (re-search-forward " +" (line-end-position) t)
                (setq time-passed (/ (* size (car (get-beat)))
                                     (cadr (get-beat)))
                      current-pos (point))
                (while (and (re-search-forward " +" (line-end-position) t)
                            (= time-passed (/ (* size (car (get-beat)))
                                              (cadr (get-beat)))))
                  (setq current-pos (point)))
                (goto-char current-pos)
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
                                          (current-column)))))
                  (setq ov-count
                        (+ indentation-column
                           (- time-passed (current-column)))))))))))))

(defun lilypond-make-hex (col)
  (format "#%02X%02X%02X" (round col) (round (/ col 2)) (round col)))

(defun lilypond--make-overlay (line symbol)
  "draw line at (line, col)"
  (setq foo-test symbol)
  (let ((original-pos (point))
        (color (lilypond-make-hex (random 200)))
        ov prop)

    (save-excursion
      (goto-char line)
      (when (looking-at " ")
        (setq prop 'after-string
              ov (make-overlay (point) (+ 1 (point))))
        (when ov
          (overlay-put
           ov 'category 'lily-pretty)
          (overlay-put ov prop symbol))))))

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
