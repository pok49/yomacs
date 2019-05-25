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
(defvar yo-database-file "~/git/yomacs/yo.t" ; <== FIXME !
  "Where your yo.t database lives")

(defvar yo-cutting-strings (list "\\-" "\"=" "\"~")
  "Words in the text may be split by some strings:
for example: hy\\-phe\\-na\\-ti\\-on in TeX")

(defun read-yo-database (file-name)
  "Reading yo database from FILENAME and return cons:
\(only-yo-hash . may-be-yo-hash) where hash mapping word whithout yo
to corresponding yo-form"
  (let ((only-yo (make-hash-table :test 'equal :size 60000))
	(may-be-yo (make-hash-table :test 'equal :size 2000))
	current-word)
      (switch-to-buffer (generate-new-buffer "yo"))
      (insert-file-contents file-name)
    (while (re-search-forward "^\\w+" nil t)
      (setq current-word (match-string 0))
      (puthash (replace-regexp-in-string "ё" "е" current-word)
	       current-word only-yo))
    (goto-char (point-min))
    (while (re-search-forward "^\\*[ \t]*\\(\\w+\\)" nil t)
      (setq current-word (match-string 1))
      (puthash (replace-regexp-in-string "ё" "е" current-word)
	       current-word may-be-yo))
    (kill-buffer (current-buffer))
    (cons only-yo may-be-yo)))

(defvar yo-hash
  (read-yo-database yo-database-file)
  "cons (only-yo-hash . may-be-yo-hash) where hash mapping word
whithout yo to corresponding yo-form")

(defun yo-context (strict) "О чём, о нём, обо всём, всё это"
  (interactive "P")
  (let ((case-fold-search t))
    (save-restriction
      (save-excursion
        (when mark-active
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min)))

        (while (re-search-forward
                "\\<\\(об?\\|обо\\|на\\|во?\\|при\\) \\([чн]\\|вс\\)ем\\>"
                nil t)
          (unless
              (save-match-data
                (left-word 3) ; не в чем себя упрекнуть
                (prog1 (looking-at "не[[:space:]]+\\(в\\|об?\\|на\\) чем\\>")
                  (right-word 3)))
              (replace-match "\\1 \\2ём" nil)))

        (goto-char (point-min))
        (while (re-search-forward "\\<\\(по [чн]\\)ем\\>" nil t)
          (replace-match "\\1ём" nil))

        (unless strict
          (goto-char (point-min))
          (while (re-search-forward
 "\\<\\(вс\\)е\\(,? что\\| э?то\\| время\\| больше\\| более\\| меньше\\| менее\\| равно\\| было\\| \\w+ство\\|-таки\\)\\>"
                nil t)
            (replace-match "\\1ё\\2" nil)))

        (goto-char (point-min))
        (while (re-search-forward "\\(\\w+лось\\) \\(вс\\)е\\>"  nil t)
          (replace-match "\\1 \\2ё" nil))

        (goto-char (point-min))
        (while (re-search-forward "\\<\\(вс\\)е \\(\\w+лось\\)\\>"  nil t)
          (replace-match "\\1ё \\2" nil))

))))

(defun yo-spell ()
  "Словарная ёфикация с диалоговой заменой проблемных слов (типа {сел|сёл}).
Пробел или английские y, Y подтверждают замену.
DEL, Backspace, n или N замену отменяют.
Кроме того, при ответе заглавной буквой (Y или N) входим
в рекурсивное редактирование"
  (interactive)
  (save-restriction
    (save-excursion
      (let (x
            current-e-word
	    current-yo-word
	    (cutting (concat "\\(?:"
			     (mapconcat 'regexp-quote
					yo-cutting-strings "\\|")
			     "\\)")))
	(when mark-active
	  (narrow-to-region (region-beginning) (region-end))
	  (goto-char (point-min)))
	(while (re-search-forward
		(concat "\\(?:\\w\\(?:\\w\\|" cutting "\\)*\\)?"
			"\\(?:е\\|Е\\)"
			"\\(?:\\w\\(?:\\w\\|" cutting "\\)*\\)?") nil t)
	  (save-match-data
 	    (setq current-e-word
		  (downcase
		   (replace-regexp-in-string cutting "" (match-string 0))))
	    (setq current-yo-word (gethash current-e-word (car yo-hash))))
	  (if current-yo-word (replace-match current-yo-word nil)
	    (setq current-yo-word (gethash current-e-word (cdr yo-hash)))
 	    (when current-yo-word
              (if search-highlight (isearch-highlight (match-beginning 0) (match-end 0)))
              (while
                  (progn
                      (setq x (read-char-exclusive
                              (format
                               "Меняем \"%s\" на \"%s\"? (Да={SPC|y}, Нет={DEL|n}) "
                               current-e-word current-yo-word)))
                    (cond
                     ((or (= x ? ) (= x ?y) (= x ?Y))
                      (undo-boundary)
                      (replace-match current-yo-word nil)
                      nil)
                     ((or (= x ?\d ) (= x ?\b) (= x ?n) (= x ?N))
                      (isearch-dehighlight)
                      nil)
                     (t (ding) t)))) ;; Loop end
              (when (or (= x ?N) (= x ?Y)) ; Uppercase Y|N => recursive edit
                (message "%s"
                         (substitute-command-keys
                          (concat "Exit recursive edit with"
                                  " \\[exit-recursive-edit]")))
                (save-window-excursion (save-excursion (recursive-edit)))))))))))

;; These are test strings
;; Вс\-е ее Все  ЛЕСС
;; (gethash "все" (cdr yo-hash))

(defun yo-rm-entry (word) "Remove word from the may-be-yo hash"
  (interactive "sИгнорировать слово: ")
  (if (string-match "\\([*+]\\)?[ \t]*\\(\\w+\\)$" word)
      (let ((e-word (replace-regexp-in-string "ё" "е" (match-string 2 word))))
        (if (string-match "^\\+" word)
            (puthash e-word (gethash e-word (cdr yo-hash)) (car yo-hash)))
        (remhash e-word (cdr yo-hash)))
    (unless (string-match "^#" word)
      (error "Wrong dict entry"))))

(defun yo-rm-many (filePath) "Remove many words from the may-be-yo hash"
  (interactive "fFile: ")
;   (unless (file-exists-p file)
;     (error "%s does not exist" file)
;     (save-excursion
;     
;       (find-file filePath)
;       (read-line)
; 
  (let* ((coding-system-for-read 'windows-1251-unix)
         (lst (with-temp-buffer
               (insert-file-contents filePath)
               (split-string (buffer-string) "\n" t))))
    (dolist (x lst) (yo-rm-entry x))))
