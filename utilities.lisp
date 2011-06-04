
(defun run-cmd (cmd &rest args)
  (sb-ext:run-program cmd args :input t :output t :wait t))

(defun browse (url)
  (run-cmd *browser-cmd*
	   "-accept_all_cookies "
	   url))

(defun insensitive-equal (str1 str2)
  (equal (string-downcase str1)
	 (string-downcase str2)))

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
	 (clean (ppcre:regex-replace "</[a-zA-Z]+>" clean " "))
	 ;; a dirty hack to avoid the following fatal error
	 ;; %n in writable segment detected
	 (clean (ppcre:regex-replace "% n" clean "%_ n"))
	 )
    (if (not (equal clean comment-str))
	(clean-html-str clean)
	(string-trim " " clean))))

(defun flatten-alist (alst)
  (mapcan (lambda (x) (list (car x) (cdr x))) alst))

(defun integerlistp (lst)
  (notany #'alpha-char-p lst))

(defun text-to-str (text)
  (if text
      (coerce (mapcar #'code-char (reverse text)) 'string)
      ""))