
(defun run-cmd (cmd &rest args)
  (sb-ext:run-program cmd args :input t :output t :wait t))

(defun browse (url)
  (run-cmd *browser-cmd*
	   "-accept_all_cookies "
	   url))

(defun url-output (url)
  (let ((s (drakma:http-request url :want-stream t)))
    (json:decode-json-from-string (read-line s))))

(defun short-url (item)
  (let* ((without-front (ppcre:regex-replace
			 "http[s]*://(www.)*"
			 (hn-item-url item)
			 ""))
	 (short-url (ppcre:regex-replace
		     "/.*"
		     without-front
		     "")))
    short-url))

(defun clean-html-str (comment-str)
  (let* ((clean (ppcre:regex-replace "<[a-zA-Z]+.*?>" comment-str " "))
	 (clean (ppcre:regex-replace "</[a-zA-Z]+>" clean " ")))
    ;; a dirty hack to avoid the following fatal error
    ;; %n in writable segment detected
    (let ((pattern "%\\s*n"))
      (loop for match in (ppcre:all-matches-as-strings pattern clean)
	 for whitespace = (repeat-char #\space (- (length match) 2))
	 do
	   (setf clean (ppcre:regex-replace-all
			(format nil "%~an" whitespace)
			clean
			(format nil "%_~an" whitespace)))))
    (if (not (equal clean comment-str))
	(clean-html-str clean)
	(string-trim " " clean))))

(defun repeat-char (char n)
  (coerce  (loop repeat n
	      collect char) 'string))

(defun flatten-alist (alist)
  (mapcan (lambda (x) (list (car x) (cdr x))) alist))

(defun integerlistp (list)
  (every #'digit-char-p list))

(defun text-to-str (text)
  (if text
      (map 'string #'code-char (reverse text))
      ""))