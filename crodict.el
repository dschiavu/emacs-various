;; -*- coding: iso-8859-2 -*-

;; crodict.el -- Pomoć s hrvatsko-engleskim rječnikom.
;; Copyright (C) 2005 Hrvoje Nikšić

;; Author: Hrvoje Nikšić <***@xemacs.org>
;; Keywords: i18n
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;;; Commentary:

;; This program provides dictionary access specific to the Croatian
;; language and is only useful to speakers (or learners) of that
;; language. Because of that the explanation is provided in Croatian.

;; Ovaj program služi kao pomoć pri korištenju hrvatsko-engleskog
;; rječnika Denisa Lackovića. Zapravo se može koristiti bilo koja
;; datoteka s rječnikom, pod uvjetom da je u pitanju tekstualna
;; datoteka s dva stupca, gdje je u prvom stupcu engleski izraz, a u
;; drugom hrvatski. Stupci su odvojeni znakom TAB. Pri učitavanju
;; rječnika koristi se trenutno važeći coding sistem -- oni koji
;; koriste Mule vjerojatno žele izvršiti nešto poput
;; (set-language-environment "Latin-2") da bi vidjeli naše znakove.
;;
;; Program se koristi tako da se stavi u load-path, iskompajlira i u
;; ~/.emacs stavi linija poput:
;;
;; (require 'crodict)
;;
;; Modul će se učitati i automatski bindati tipke `M-C', `M-E' i `M-T'
;; na svoje funkcije koje (tim redosljedom) služe za prevođenje riječi
;; s hrvatskog na engleski, prevođenje s engleskog na hrvatski i
;; prevođenje riječi oko pointa.

;;; Code:

(require 'cl)

(defgroup crodict nil
"Provide access to English-Croatian and Croatian-English translations.")

(defcustom crodict-dictionary-file "~/elisp/Engleski-Hrvatski.txt"
"Location of the dictionary file.
The file must contain non-empty translation lines, in this format:
ENGLISH-WORD<TAB>CROATIAN-WORD"
:type 'string
:group 'crodict)

;; EVIL default, loading libraries shouldn't change global bindings!
(defcustom crodict-bind-keys t
"Whether crodict should bind keys when loaded.
By default, crodict binds `M-C' to `crodict-croatian-to-english',
`M-E'to `crodict-english-to-croatian', and M-T to
`crodict-translate-word-at-point'
NOTE: this variable has no effect after crodict has been loaded."
:type 'boolean
:group 'crodict)

(when crodict-bind-keys
(global-set-key [(meta T)] 'crodict-translate-word-at-point)
(global-set-key [(meta C)] 'crodict-croatian-to-english)
(global-set-key [(meta E)] 'crodict-english-to-croatian))

(defvar crodict-dictionary-file "~/elisp/Engleski-Hrvatski.txt")

(defun crodict-buffer ()
(or (get-buffer " *Crodict Dictionary*")
(with-current-buffer (get-buffer-create " *Crodict Dictionary*")
(unwind-protect
(insert-file-contents crodict-dictionary-file)
(if (= (buffer-size) 0)
;; Don't leave a dangling dictionary buffer if
;; insert-file-contents signals.
(kill-buffer (current-buffer))))
(current-buffer))))

(defun crodict-croatian-string-lessp (w1 w2)
;; like string-lessp, but correctly comparing words with Croatian
;; chars.
(let ((index 0)
(commonsz (min (length w1) (length w2))))
(while (and (< index commonsz)
(eq (aref w1 index) (aref w2 index)))
(incf index))
(if (= index commonsz)
(< (length w1) (length w2))
;; Compare the last non-equal char of W1 and W2, converting each
;; HR char to a floating point number between the two ASCII
;; codes of its adjacent characters. For example changing š to
;; 115.1 makes sure that it sorts after "s" (115), but before
;; "t" (116). Yes, it's an evil hack, but it works.
(let* ((c1 (aref w1 index))
(c2 (aref w2 index))
(alist '((?č 99.1) (?ć 99.2) (?š 115.1) (?ž 122.1) (?đ 100.1)))
(c1repl (assq c1 alist))
(c2repl (assq c2 alist)))
(when c1repl
(setq c1 (cadr c1repl)))
(when c2repl
(setq c2 (cadr c2repl)))
(< c1 c2)))))

(defun crodict-print-translations (word translations)
;; Used to avoid repeating this code in three different functions.
(if translations
(message "%s: %s" word (mapconcat #'identity translations ", "))
(message "No translations for `%s'" word)))

;;;###autoload
(defun crodict-english-to-croatian (word)
"Translate WORD to Croatian."
(interactive "sEnglish: ")
(with-current-buffer (crodict-buffer)
(goto-char (point-min))
(let ((regex (concat "^" (regexp-quote word) "\t"))
(translations ())
(case-fold-search nil))
(while (re-search-forward regex nil t)
(push (buffer-substring (progn (skip-chars-forward "\t")
(point))
(progn (end-of-line)
(point)))
translations))
(setq translations (sort translations #'crodict-croatian-string-lessp))
(and (interactive-p) (crodict-print-translations word translations))
translations)))

(defun crodict-word-to-regex (word)
(setq word (mapconcat
(lambda (char)
(case char
(?c "[cčć]")
(?s "[sš]")
(?z "[zž]")
((?\\ ?\[ ?\] ?\* ?\?) (format "\\%c" char))
(otherwise (list char))))
word ""))
;; Must handle "dj" specially:
(let ((i 0)
(parts ()))
(while (string-match "dj" word i)
(push (substring word i (match-beginning 0)) parts)
(setq i (match-end 0)))
(push (substring word i (length word)) parts)
(mapconcat #'identity (nreverse parts) "[đd]j?")))

;;;###autoload
(defun crodict-croatian-to-english (word)
"Translate WORD to English."
(interactive "sCroatian: ")
(with-current-buffer (crodict-buffer)
(goto-char (point-min))
(let ((regex (concat "\t" (crodict-word-to-regex word) "$"))
(translations ())
(case-fold-search nil))
(while (re-search-forward regex nil t)
(push (buffer-substring (progn (beginning-of-line)
(point))
(progn (skip-chars-forward "^\t")
(point)))
translations)
(end-of-line))
(setq translations (sort translations #'string-lessp))
(and (interactive-p) (crodict-print-translations word translations))
translations)))

;;;###autoload
(defun crodict-translate-word-at-point ()
"Translate WORD at point to either Croatian or English."
(interactive)
(let* ((word (downcase
(or (and (fboundp 'region-active-p)
(region-active-p)
(buffer-substring (region-beginning) (region-end)))
(buffer-substring (save-excursion (skip-syntax-backward "w")
(point))
(save-excursion (skip-syntax-forward "w")
(point))))))
;; Try English first, since it's quicker.
(translations (or (crodict-english-to-croatian word)
(crodict-croatian-to-english word))))
(crodict-print-translations word translations)))

(provide 'crodict)

;;; crodict.el ends here