;; -*- coding: iso-8859-2 -*-

;; crodict.el -- Brz pristup hrvatsko-engleskom rje�niku.
;; Copyright (C) 2005 Hrvoje Nik�i�

;; Author: Hrvoje Nik�i� <hniksic@xemacs.org>
;; Keywords: i18n
;; Version: 1.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;;; Commentary:

;; This program provides dictionary access specific to the Croatian
;; language and is only useful to speakers (or learners) of that
;; language.  Because of that the explanation is provided in Croatian.

;; Ovaj program slu�i kao pomo� pri kori�tenju hrvatsko-engleskog
;; rje�nika s lokalizacija.linux.hr.  Koristi ga se tako da se stavi u
;; direktorij u load-pathu, iskompajlira i u ~/.emacs stavi k�d poput:
;;
;;     (setq crodict-dictionary-file "/staza/do/Engleski-Hrvatski.txt")
;;     (require 'crodict)
;;
;; Modul po u�itatavanju automatski mapira tipke `M-C', `M-E' i `M-T'
;; na svoje funkcije koje, redom, slu�e za prevo�enje rije�i s
;; hrvatskog na engleski, prevo�enje s engleskog na hrvatski i
;; prevo�enje rije�i na kojoj se nalazi point.
;;
;; Rje�nik koji program koristi mo�e se skinuti s adrese:
;;
;;     http://fly.srk.fer.hr/~hniksic/emacs/Engleski-Hrvatski.txt.bz2
;;
;; To je rje�nik s lokalizacija.linux.hr, uz minimalnu izmjenu da je
;; separator izmijenjen iz osam razmaka u TAB, da je prekonvertiran u
;; Latin-2 kodnu stranicu i da je maknut prijevod rije�i "bookmark" na
;; prazan string.  Nije nu�no kori�tenje ba� ovog rje�nika, umjesto
;; njega se mo�e koristiti bilo koja datoteka s rje�ni�kim sadr�ajem,
;; pod uvjetom da zadovoljava vrlo jednostavan format.  Mora biti
;; tekstualna datoteka s dva stupca odvojena znakom TAB; prvi stupac
;; sadr�i engleski izraz, a drugi stupac hrvatski prijevod.
;;
;; Pri u�itavanju rje�nika koristi se defaultni Emacsov encoding.
;; Korisnici Mulea vjerojatno �ele izvr�iti ne�to poput
;; (set-language-environment "Latin-2") da bi u rje�niku dobili na�e
;; znakove.
;;
;; Najnovija verzija ovog programa mo�e se na�i na:
;;
;;     http://fly.srk.fer.hr/~hniksic/emacs/crodict.el
;;
;; Program je pisan tako da se vrlo brzo u�ita, da ne u�itava rje�nik
;; dok se prvi put ne upotrijebi, da ne tro�i vi�e memorije nego �to
;; mora (nema fancy hash tablica sa stotinama tisu�a consanih
;; stringova i vektora, sve informacije su u jednom bufferu), te da
;; lookup radi razumno brzo uz prethodne constraintse.  Pretra�ivanje
;; se vr�i trivijalnim regexpom, �to na mom 1.6GHz stroju i XEmacsu
;; traje ~20ms s rje�nikom od ~80,000 entryja, a u GNU Emacsu je
;; gotovo duplo br�e.
;;
;; Testirao sam program na XEmacsu 21.4.17 i GNU Emacsu 21.4.1, ali bi
;; trebao raditi na bilo kojem recentnijem (X)Emacsu.


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
`crodict-translate-word-at-point'.
NOTE: this variable has no effect after crodict has been loaded."
  :type 'boolean
  :group 'crodict)

(when crodict-bind-keys
  (global-set-key [(meta T)] 'crodict-translate-word-at-point)
  (global-set-key [(meta C)] 'crodict-croatian-to-english)
  (global-set-key [(meta E)] 'crodict-english-to-croatian))

(defvar crodict-dictionary-file "~/elisp/Engleski-Hrvatski.txt")

(defun crodict-buffer ()
  "Return the crodict buffer, preparing it if necessary."
  (or (get-buffer " *crodict*")
      (with-current-buffer (get-buffer-create " *crodict*")
	(unwind-protect
	    (insert-file-contents crodict-dictionary-file)
	  (if (= (buffer-size) 0)
	      ;; Don't leave a dangling dictionary buffer if
	      ;; insert-file-contents signals an error.
	      (kill-buffer (current-buffer))))
	(current-buffer))))

(defun crodict-croatian-string-lessp (w1 w2)
  ;; like string-lessp, but correctly collating words containing
  ;; Croatian chars.
  (let ((index 0)
	(commonsz (min (length w1) (length w2))))
    (while (and (< index commonsz)
		(eq (aref w1 index) (aref w2 index)))
      (incf index))
    (if (= index commonsz)
	(< (length w1) (length w2))
      ;; Compare the last non-equal char of W1 and W2, converting each
      ;; HR char to a floating point number between the two ASCII
      ;; codes of its adjacent characters.  For example changing � to
      ;; 115.1 makes sure that it sorts after "s" (115), but before
      ;; "t" (116).  Yes, it's an evil hack, but it works.
      (let* ((c1 (aref w1 index))
	     (c2 (aref w2 index))
	     (alist '((?� 99.1) (?� 99.2) (?� 115.1) (?� 122.1) (?� 100.1)))
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
  "Translate WORD to Croatian, returning a sorted list of translations.
When called interactively, prompt for WORD and print translations to
echo area."
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

;; Convert WORD to regex suitable for finding the word, including
;; approximations.  Otherwise like regexp-quote.
(defun crodict-word-to-regex (word)
  (setq word (mapconcat
	      (lambda (char)
		(case char
		  (?c "[c��]")
		  (?s "[s�]")
		  (?z "[z�]")
		  ((?\\ ?[ ?] ?* ?+ ?? ?\+ ?. ?$ ?^) (format "\\%c" char))
		  (otherwise (list char))))
	      word ""))
  ;; Must handle "dj" specially:
  (let ((i 0)
	(parts ()))
    (while (string-match "dj" word i)
      (push (substring word i (match-beginning 0)) parts)
      (setq i (match-end 0)))
    (push (substring word i (length word)) parts)
    (mapconcat #'identity (nreverse parts) "[�d]j?")))

;;;###autoload
(defun crodict-croatian-to-english (word &optional exact)
  "Translate WORD to English, returning a sorted list of translations.
When called interactively, prompt for WORD and print translations to
echo area.
If EXACT is non-nil, WORD is not matched loosely with regard to
Croatian characters."
  (interactive "sCroatian: ")
  (with-current-buffer (crodict-buffer)
    (goto-char (point-min))
    (let ((regex (concat "\t" (if exact
				  (regexp-quote word)
				(crodict-word-to-regex word))
			 "$"))
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
  "Translate word at point to either Croatian or English.
If region is active, the contents of region is used as word to translate.
This is the only way to provide multi-word phrases to this function.
The translations are printed in the echo area."
  (interactive)
  (let* ((word (downcase
		(or (and (fboundp 'region-active-p)
			 (region-active-p)
			 (buffer-substring (region-beginning) (region-end)))
		    (buffer-substring (save-excursion (skip-syntax-backward "w")
						      (point))
				      (save-excursion (skip-syntax-forward "w")
						      (point))))))
	 translations)
    (when (zerop (length word))
      (error "No word at point"))
    ;; Try English first, since it's quicker.
    (setq translations (or (crodict-english-to-croatian word)
			   (crodict-croatian-to-english word)))
    (crodict-print-translations word translations)))

(provide 'crodict)

;;; crodict.el ends here
