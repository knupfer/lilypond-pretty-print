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
  (forward-line))

(provide 'lilypond-pretty-print)

;;; lilypond-pretty-print.el ends here
