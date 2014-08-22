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

(defvar lilypond-timer-1 nil)
(defvar lilypond-timer-2 nil)
(defvar lilypond-timer-3 nil)
(defvar lilypond-timer-4 nil)
(defvar lilypond-draw-pos 1)
(defvar lilypond-analyse-pos 1)
(defvar lilypond-measure-length nil)
(defvar lilypond-measure-length-list nil)
(defvar lilypond-mode-line " ♫")
(defvar lilypond-fill-column 64)
(defvar lilypond-pretty-print-size 144)
(defvar lilypond-scan-position nil)
(defvar times-used)
(defvar times-used-tmp)

(defun lilypond-beat-remove (&optional begin-of-change end-of-change
                                       zonk is-measure)
  (dolist (ov (lilypond-beat--active-overlays begin-of-change end-of-change
                                              is-measure))
    (delete-overlay ov)))

(defun lilypond-beat--active-overlays (&optional begin-of-change end-of-change
                                                 is-measure)
  (let ((del-from (point-min))
        (del-to (point-max)))
    (when (and begin-of-change end-of-change)
      (save-excursion
        (goto-char begin-of-change)
        (beginning-of-line)
        (setq del-from (point))
        (goto-char end-of-change)
        (end-of-line)
        (setq del-to (point))))
    (delq nil (mapcar (lambda (ov)
                        (and (eq (overlay-get ov 'category)
                                 (if is-measure 'lily-measure 'lily-pretty))
                             ov))
                      (overlays-in del-from del-to)))))

(defun lilypond-string (begin times)
  (let ((result nil))
    (while (> times 0)
      (let ((col (lilypond-make-hex (elt times-used
                                         (min (- (length times-used) 1)
                                              begin)))))
        (setq result (concat result
                             (eval `(propertize "│"
                                                'face '(:foreground ,col))))))
      (setq begin (+ 1 begin)
            times (- times 1)))
    result))

(defun lilypond-analyse-metrum ()
  (if (not lilypond-pretty-beat-mode)
      (progn
        (cancel-timer lilypond-timer-3)
        (setq lilypond-timer-3 nil)
        (setq lilypond-analyse-pos 1))
    (while-no-input
      (save-excursion
        (goto-char lilypond-analyse-pos)
        (re-search-forward "| *\\(%.*\\)*$" nil t)
        (save-excursion
          (while (re-search-forward "| *\\(%.*\\)*$" nil t)
            (save-excursion
              (goto-char (match-beginning 0))
              (setq-local lilypond-measure-length-list
                          (cons (/ (* 144 (car (get-beat)))
                                   (cadr (get-beat))) lilypond-measure-length-list)))
            (setq-local lilypond-analyse-pos (point))))
        (sort lilypond-measure-length-list '<)
        (let ((old-length (car lilypond-measure-length-list))
              (local-count 0)
              (result-count 0)
              (result 144)
              (inhibit-quit t))
          (while lilypond-measure-length-list
            (if (and (cadr lilypond-measure-length-list)
                     (eq old-length (car lilypond-measure-length-list)))
                (setq local-count (+ 1 local-count))
              (when (> local-count result-count)
                (setq result-count local-count
                      result old-length))
              (setq old-length (car lilypond-measure-length-list))
              (setq local-count 1))
            (pop lilypond-measure-length-list))
          (setq-local lilypond-measure-length result)
          (setq-local lilypond-pretty-print-size
                      (/ (* lilypond-fill-column 144) lilypond-measure-length))
          (setq-local lilypond-analyse-pos 1)
          (cancel-timer lilypond-timer-3)
          (setq lilypond-timer-3 nil))))))

(defun lilypond-scan-for-beats ()
  (if (not lilypond-pretty-beat-mode)
      (progn
        (cancel-timer lilypond-timer-1)
        (setq lilypond-timer-1 nil))
    (save-excursion
      (if lilypond-scan-position
          (goto-char lilypond-scan-position)
        (goto-char (point-min))
        (re-search-forward "| *\\(%.*\\)*$" nil t))
      (while-no-input
        (let ((time-passed 0))
          (while (re-search-forward "| *\\(%.*\\)*$" nil t)
            (let ((inhibit-quit t))
              (save-excursion
                (back-to-indentation)
                (while (re-search-forward "\\(<+[^<>]*>+\\)? +"
                                          (line-end-position) t)
                  (when (and (not (looking-at "r"))
                             (not (= time-passed
                                     (/ (* lilypond-pretty-print-size
                                           (car (get-beat))) (cadr (get-beat)))))
                             (< (/ (* lilypond-pretty-print-size (car (get-beat)))
                                   (cadr (get-beat))) 128))
                    (setq time-passed (/ (* lilypond-pretty-print-size
                                            (car (get-beat))) (cadr (get-beat))))
                    (aset times-used-tmp time-passed (+ 1 (elt times-used-tmp
                                                               time-passed)))))
                (aset times-used-tmp time-passed (+ -1 (elt times-used-tmp
                                                            time-passed))))
              (setq lilypond-scan-position (point))))
          (let ((beat-max 1)
                (inhibit-quit t))
            (setq lilypond-scan-position nil)
            (mapc (lambda (x) (setq beat-max (max beat-max (* x x)))) times-used-tmp)
            (setq-local times-used
                        (map 'vector (lambda (y) (max 0 (/ (* 255 (* y y))
                                                           beat-max))) times-used-tmp))
            (setq-local times-used-tmp (make-vector 128 0))
            (cancel-timer lilypond-timer-1)
            (setq lilypond-timer-1 nil)
            (unless lilypond-timer-2
              (setq lilypond-timer-2
                    (run-with-idle-timer 0.4 t 'lilypond-beat-show 1 (point-max) 2)))))))))

(defun lilypond-timers (&optional foo bar baff)
  (unless lilypond-timer-4
    (setq lilypond-timer-4
          (run-with-idle-timer 0.1 t 'lilypond-get-measure)))
  (unless lilypond-timer-3
    (setq lilypond-timer-3
          (run-with-idle-timer 0.1 t 'lilypond-analyse-metrum)))
  (unless lilypond-timer-1
    (setq lilypond-timer-1
          (run-with-idle-timer 0.25 t 'lilypond-scan-for-beats))))

(defun lilypond-get-measure (&optional begin-of-change end-of-change zonk)
  (if (not lilypond-pretty-beat-mode)
      (progn
        (cancel-timer lilypond-timer-4)
        (setq lilypond-timer-4 nil))
    (while-no-input
      (save-excursion
        (lilypond-beat-remove (point-min) (point-max) nil t)
        (goto-char (point-min))
        (while (re-search-forward "| *%[^0-9]*\\([0-9]*\\)" nil t)
          (save-excursion
            (let ((current-measure (string-to-number (match-string 1)))
                  (current-line (line-number-at-pos)))
              (while (and (re-search-forward "| *\\(%[^0-9\n]*\\)?$" nil t)
                          (= (line-number-at-pos) (+ 1 current-line)))
                (setq current-line (+ 1 current-line)
                      current-measure (+ 1 current-measure))
                (when (not (match-string 1))
                  (lilypond--make-overlay
                   (match-beginning 0)
                   (propertize (concat " " (number-to-string
                                            current-measure))
                               'face '(:foreground "blue"))
                   t)))))))
      (let ((inhibit-quit t))
        (cancel-timer lilypond-timer-4)
        (setq lilypond-timer-4 nil)))))

(defun lilypond-beat-show (&optional begin-of-change end-of-change zonk)
  (if (not lilypond-pretty-beat-mode)
      (progn
        (cancel-timer lilypond-timer-2)
        (setq lilypond-timer-2 nil))
    (unless (active-minibuffer-window)
      (while-no-input
        (save-excursion
          (let ((win-min (window-start))
                (win-max (window-end)))
            (when (and begin-of-change end-of-change)
              (goto-char begin-of-change)
              (beginning-of-line)
              (setq win-min (point))
              (goto-char end-of-change)
              (end-of-line)
              (setq win-max (point)))
            (when (eq zonk 2)
              (setq win-min lilypond-draw-pos))
            (goto-char (point-min))
            (re-search-forward "| *\\(%.*\\)*$" nil t)
            (if (< (point) win-min)
                (goto-char win-min)
              (forward-line))
            (when (> win-max (point))
              (while (re-search-forward "| *\\(%.*\\)*$" win-max t)
                (let ((inhibit-quit t))
                  (if (eq zonk 2)
                      (progn
                        (lilypond-beat-remove (+ 1 lilypond-draw-pos) (point))
                        (setq-local lilypond-draw-pos (point)))
                    (lilypond-beat-remove (point) (point)))
                  (save-excursion
                    (goto-char (match-beginning 0))
                    (if (and (> (cadr (get-beat)) 0)
                             (> (car (get-beat)) 0)
                             (eq lilypond-pretty-print-size
                                 (/ (* 144 lilypond-fill-column)
                                    (/  (* 144 (car (get-beat)))
                                        (cadr (get-beat))))))
                        (let ((line (line-number-at-pos))
                              (local-count 0))
                          (back-to-indentation)
                          (let ((time-passed nil)
                                (indentation-column (current-column))
                                (ov-count 0)
                                (current-pos nil))
                            (while (re-search-forward " +" (line-end-position) t)
                              (setq time-passed (/ (* lilypond-pretty-print-size
                                                      (car (get-beat))) (cadr (get-beat)))
                                    current-pos (point))
                              (while (and (re-search-forward " +" (line-end-position) t)
                                          (= time-passed (/ (* lilypond-pretty-print-size
                                                               (car (get-beat)))
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
                                                     (- time-passed ov-count
                                                        (current-column)))))
                                (setq ov-count
                                      (+ indentation-column
                                         (- time-passed (current-column))))))))
                      (let ((current-pos (point))
                            (current-col (current-column))
                            (err-string nil)
                            (beat-length (/ (* 144 (car (get-beat)))
                                            (cadr (get-beat))))
                            (time-passed (/ (* lilypond-pretty-print-size
                                               lilypond-measure-length) 144)))
                        (if (> beat-length lilypond-measure-length)
                            (setq err-string (propertize
                                              (make-string
                                               (min 7 (max 1 (round (* 5 (- (/ (* 1.0 beat-length)
                                                                               lilypond-measure-length)
                                                                            1)))))
                                               (string-to-char "+"))
                                              'face '(:foreground "green")))
                          (setq err-string (propertize
                                            (make-string
                                             (min 7 (max 1 (round (* 5 (- (/ (* 1.0 lilypond-measure-length)
                                                                             (max 1 beat-length))
                                                                          1)))))
                                             (string-to-char "-"))
                                            'face '(:foreground "red"))))
                        (back-to-indentation)
                        (lilypond--make-overlay
                         (+ -1 current-pos)
                         (concat
                          (lilypond-string (- current-col (current-column))
                                           (+ (current-column)
                                              (- time-passed current-col (length err-string))))
                          err-string)))))))
              (when (eq zonk 2)
                (setq-local lilypond-draw-pos (point-min))
                (cancel-timer lilypond-timer-2)
                (setq lilypond-timer-2 nil)))))))))

(defun lilypond-make-hex (col)
  (format "#%02X%02X%02X" (round col) (round (/ col 2)) (round col)))

(defun lilypond--make-overlay (ov-pos symbol &optional measure-num)
  (save-excursion
    (goto-char ov-pos)
    (let ((ov (make-overlay (+ 1 (point)) (+ 1 (point)))))
      (when (and (or (looking-at "|") (looking-at " ")) ov)
        (overlay-put
         ov 'category (if measure-num 'lily-measure 'lily-pretty))
        (overlay-put ov 'after-string symbol)))))

(define-minor-mode lilypond-pretty-beat-mode ()
  :init-value nil
  :lighter (:eval lilypond-mode-line)
  :global nil
  (when (not (equal (buffer-name) " *temp*"))
    (if lilypond-pretty-beat-mode
        (progn
          (setq-local times-used (make-vector 128 0))
          (setq-local times-used-tmp (make-vector 128 0))
          (lilypond-timers)
          (add-hook 'after-change-functions 'lilypond-beat-show nil t)
          (add-hook 'after-change-functions 'lilypond-timers nil t))
      (lilypond-beat-remove (point-min) (point-max))
      (lilypond-beat-remove (point-min) (point-max) nil t)
      (remove-hook 'after-change-functions 'lilypond-beat-show t)
      (remove-hook 'after-change-functions 'lilypond-timers t))))

(provide 'lilypond-pretty-print)

;;; lilypond-pretty-print.el ends here
