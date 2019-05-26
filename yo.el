;; -*- mode: emacs-lisp; coding: windows-1251-unix -*-
;; (c) Eugene Minkovskii; yo.el; Sat Oct 04 15:53:07 2003
;; <emin@mccme.ru> http://python.anabar.ru/yo.htm
;; (C) Sergio Pokrovskij 2019-05-25
;; GNU General Public License v. 3.0
;; https://www.gnu.org/licenses/gpl-3.0.en.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ����� ���� ������� ������ �������� ���� ;;
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
      (puthash (replace-regexp-in-string "�" "�" current-word)
	       current-word only-yo))
    (goto-char (point-min))
    (while (re-search-forward "^\\*[ \t]*\\(\\w+\\)" nil t)
      (setq current-word (match-string 1))
      (puthash (replace-regexp-in-string "�" "�" current-word)
	       current-word may-be-yo))
    (kill-buffer (current-buffer))
    (cons only-yo may-be-yo)))

(defvar yo-hash
  (read-yo-database yo-database-file)
  "cons (only-yo-hash . may-be-yo-hash) where hash mapping word
whithout yo to corresponding yo-form")

(defun yo-context (strict) "� ���, � ��, ��� ���, �� ���"
  (interactive "P")
  (let ((case-fold-search t))
    (save-restriction
      (save-excursion
        (when mark-active
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min)))

        (while (re-search-forward
                "\\<\\(��?\\|���\\|��\\|��?\\|���\\) \\([��]\\|��\\)��\\>"
                nil t)
          (unless
              (save-match-data
                (left-word 3) ; �� � ��� ���� ���������
                (prog1 (looking-at "��[[:space:]]+\\(�\\|��?\\|��\\) ���\\>")
                  (right-word 3)))
              (replace-match "\\1 \\2��" nil)))

        (goto-char (point-min))
        (while (re-search-forward "\\<\\(�� [��]\\)��\\>" nil t)
          (replace-match "\\1��" nil))

        (unless strict
          (goto-char (point-min))
          (while (re-search-forward
 "\\<\\(��\\)�\\(,? ���\\| �?��\\| �����\\| ������\\| �����\\| ������\\| �����\\| �����\\| ����\\| \\w+����\\|-����\\)\\>"
                nil t)
            (replace-match "\\1�\\2" nil)))

        (goto-char (point-min))
        (while (re-search-forward "\\(\\w+����\\) \\(��\\)�\\>"  nil t)
          (replace-match "\\1 \\2�" nil))

        (goto-char (point-min))
        (while (re-search-forward "\\<\\(��\\)� \\(\\w+����\\)\\>"  nil t)
          (replace-match "\\1� \\2" nil))

))))

(defun yo-spell ()
  "��������� �������� � ���������� ������� ���������� ���� (���� {���|��}).
������ ��� ���������� y, Y ������������ ������.
DEL, Backspace, n ��� N ������ ��������.
����� ����, ��� ������ ��������� ������ (Y ��� N) ������
� ����������� ��������������"
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
			"\\(?:�\\|�\\)"
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
                               "������ \"%s\" �� \"%s\"? (��={SPC|y}, ���={DEL|n}) "
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
;; ��\-� �� ���  ����
;; (gethash "���" (cdr yo-hash))

(defun yo-rm-entry (word) "Remove word from the may-be-yo hash"
  (interactive "s������������ �����: ")
  (if (string-match "\\([*+]\\)?[ \t]*\\(\\w+\\)$" word)
      (let ((e-word (replace-regexp-in-string "�" "�" (match-string 2 word))))
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
  (let* ( ; (coding-system-for-read 'windows-1251-unix)
         (lst (with-temp-buffer
               (insert-file-contents filePath)
               (split-string (buffer-string) "\n" t))))
    (dolist (x lst) (yo-rm-entry x))))
