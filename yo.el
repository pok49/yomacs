;; -*- mode: emacs-lisp; coding: utf-8 -*-
;; (c) Eugene Minkovskii; yo.el; Sat Oct 04 15:53:07 2003
;; <emin@mccme.ru>
;;; http://python.anabar.ru/yo.htm

; Использование
; 
; В XEmacs выполните команду
; M-x load-file <RET> yo.elc
;     Примечание: Не ставьте эту команду в .emacs,
;     так как инициализация скрипта выполняется долго (зато работает он быстро).
;     Откройте ё-фицируемый файл и выполните команду
; M-x yo-spell 
; ----
; С.П.: Сначала полезно сделать
; M-x yo-context

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Здесь надо указать откуда читается база ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar yo-database-file "./yo.t"
  "Where your yo database lives")

(defvar yo-database-codingsystem 'windows-1251-unix
  "Coding system of `yo-database-file'")

(defvar yo-cutting-strings (list "\\-" "\"=" "\"~")
  "Words in the text may be split by some strings:
for example: hy\\-phe\\-na\\-ti\\-on in TeX")

;;; For compatibility with Emacs:
(unless (string-match "xemacs" emacs-version) ; test is it emacs or xemacs
  (defun replace-in-string (str regexp newtext)
    (replace-regexp-in-string regexp newtext str))
  (defun region-active-p () nil))

(defun read-yo-database (file-name &optional encoding)
  "Reading yo database from FILENAME and return cons:
\(only-yo-hash . may-be-yo-hash) where hash mapping word whithout yo
to corresponding yo-form"
  (let ((only-yo (make-hash-table :test 'equal :size 60000))
	(may-be-yo (make-hash-table :test 'equal :size 2000))
	current-word)
      (switch-to-buffer (generate-new-buffer "yo"))
      (let ((coding-system-for-read encoding))
	(insert-file-contents file-name))
    (while (re-search-forward "^\\w+" nil t)
      (setq current-word (match-string 0))
      (puthash (replace-in-string current-word "ё" "е")
	       current-word only-yo))
    (goto-char (point-min))
    (while (re-search-forward "^\\*[ \t]*\\(\\w+\\)" nil t)
      (setq current-word (match-string 1))
      (puthash (replace-in-string current-word "ё" "е")
	       current-word may-be-yo))
    (kill-buffer (current-buffer))
    (cons only-yo may-be-yo)
    ))

(defvar yo-hash
  (read-yo-database yo-database-file yo-database-codingsystem)
  "cons (only-yo-hash . may-be-yo-hash) where hash mapping word
whithout yo to corresponding yo-form")

(defun yo-context () "О чём, о нём, обо всём, всё это"
  (interactive)
  (let ((case-fold-search t))
    (save-restriction
      (save-excursion
        (when (region-active-p)
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min)))
        (while (re-search-forward
                "\\<\\(об?\\|обо\\|на\\|во?\\|при\\) \\([чн]\\|вс\\)ем\\>"
                nil t)
          (replace-match "\\1 \\2ём" nil))
        (goto-char (point-min))
        (while (re-search-forward "\\<\\([Нн]\\)е \\(в\\|о\\) чём\\>" nil t)
          (replace-match "\\1е \\2 чем" nil))
        (goto-char (point-min))
        (while (re-search-forward "\\<\\(по [чн]\\)ем\\>" nil t)
          (replace-match "\\1ём" nil))
        (goto-char (point-min))
        (while (re-search-forward
 "\\<\\(вс\\)е\\(,? что\\| это\\| время\\| больше\\| более\\| меньше\\| менее\\| равно\\| было\\| \\w+ство\\|-таки\\)\\>"
                nil t)
          (replace-match "\\1ё\\2" nil))
))))

(defun yo-spell ()
  "Run yo spell interactively"
  (interactive)
  (save-restriction
    (save-excursion
      (let (current-e-word
	    current-yo-word
	    (cutting (concat "\\(?:"
			     (mapconcat 'regexp-quote
					yo-cutting-strings "\\|")
			     "\\)")))
	(when (region-active-p)
	  (narrow-to-region (region-beginning) (region-end))
	  (goto-char (point-min)))
	(while (re-search-forward
		(concat "\\(?:\\w\\(?:\\w\\|" cutting "\\)*\\)?"
			"\\(?:е\\|Е\\)"
			"\\(?:\\w\\(?:\\w\\|" cutting "\\)*\\)?") nil t)
	  (save-match-data
;; 	    (setq current-word (match-string 0))
	    (setq current-e-word
		  (downcase
		   (replace-in-string (match-string 0) cutting "")))
	    (setq current-yo-word (gethash current-e-word (car yo-hash))))
	  (if current-yo-word
	      (replace-match current-yo-word nil)
	    (setq current-yo-word (gethash current-e-word (cdr yo-hash)))
	    (when current-yo-word
	      (if search-highlight
		  (isearch-highlight (match-beginning 0) (match-end 0)))
	      (when (y-or-n-p (format "Меняем \"%s\" на \"%s\"? "
				      current-e-word
				      current-yo-word))
		(undo-boundary)
		(replace-match current-yo-word nil))
	      (isearch-dehighlight)
	      )))
	))))

;; These are test strings
;; Вс\-е ее Все  ЛЕСС
;; (gethash "все" (cdr yo-hash))
