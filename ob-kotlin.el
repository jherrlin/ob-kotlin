(require 'ob)

(defvar org-babel-tangle-lang-exts)

(add-to-list 'org-babel-tangle-lang-exts '("kotlin" . "kotlin"))

(defgroup ob-kotlin nil
  "org-babel functions for kotlin evaluation"
  :group 'org)

(defcustom ob-kotlin:kotlinc "kotlinc"
  "kotlin compiler"
  :group 'ob-kotlin
  :type 'string)

(defcustom org-babel-kotlin-command "java"
  "Name of the kotlin command.
May be either a command in the path, like kotlin or an absolute
path name, like /usr/local/bin/kotlinc."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'string)

(defcustom org-babel-kotlin-compiler "kotlinc"
  "Name of the kotlin compiler.
May be either a command in the path, like kotlinc or an absolute
path name, like /usr/local/bin/kotlinc."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'string)

(defconst org-babel-kotlin--main-fun-re
  (rx line-start (0+ space) "fun"
      (1+ space) "main()")
  "Regexp for the main method declaration.")

(defconst org-babel-kotlin--package-re (rx line-start (0+ space) "package"
					 (1+ space) (group (1+ (in alnum ?_ ?.))) ; capture the package name
					 (0+ space) ?\; line-end)
  "Regexp for the package statement.")

(defconst org-babel-kotlin--package-re (rx line-start (0+ space) "package"
					 (1+ space) (group (1+ (in alnum ?_ ?.))) ; capture the package name
					 (0+ space) ?\; line-end)
  "Regexp for the package statement.")

(defconst org-babel-kotlin--imports-re (rx line-start (0+ space) "import"
                                         (opt (1+ space) "static")
					 (1+ space) (group (1+ (in alnum ?_ ?. ?*))) ; capture the fully qualified class name
					 (0+ space) ?\; line-end)
  "Regexp for import statements.")

(defun org-babel-kotlin--move-past (re)
  "Move point past the first occurrence of the given regexp RE."
  (while (re-search-forward re nil t)
    (goto-char (1+ (match-end 0)))))

(defun org-babel-expand-body:kotlin (body params)
  "Expand BODY with PARAMS.
BODY could be a few statements, or could include a full class
definition specifying package, imports, and class.  Because we
allow this flexibility in what the source block can contain, it
is simplest to expand the code block from the inside out."
  (let* ((imports-val (assq :imports params))
         (imports (if imports-val
                      (split-string (org-babel-read (cdr imports-val) nil) " ")
                    nil)))
    (with-temp-buffer
      (insert body)

      ;; wrap main.  If there are methods defined, but no main method
      ;; and no class, wrap everything in a generic main method.
      (goto-char (point-min))
      (when (not (re-search-forward org-babel-kotlin--main-fun-re nil t))
        (org-babel-kotlin--move-past org-babel-kotlin--package-re) ; if package is defined, move past it
        (org-babel-kotlin--move-past org-babel-kotlin--imports-re) ; if imports are defined, move past them
        (insert "fun main() {\n")
        (indent-code-rigidly (point) (point-max) 4)
        (goto-char (point-max))
        (insert "\n}"))

      ;; add imports from source block headers
      (when imports
        (goto-char (point-min))
        (org-babel-kotlin--move-past org-babel-kotlin--package-re) ; if package is defined, move past it
        (insert (mapconcat (lambda (package) (concat "import " package ";")) imports "\n") "\n"))

      ;; return expanded body
      (buffer-string))))

(defun org-babel-execute:kotlin (body params)
  "Execute a kotlin source block with BODY code and PARAMS params."
  (message body)
  (let* (;; header args for result processing
         (result-type (cdr (assq :result-type params)))
         (result-params (cdr (assq :result-params params)))
         (classpath (cdr (assq :classpath params)))
         (filename "main.kt")
         (full-body (org-babel-expand-body:kotlin body params))
         (compile-command
          (concat ob-kotlin:kotlinc " " filename " "
                  (when classpath
                    (concat "-classpath " classpath " "))
                  "-include-runtime -d main.jar"))
         (run-command
          (concat org-babel-kotlin-command
                  " -classpath main.jar"
                  (when classpath
                    (concat ":" classpath))
                  " MainKt"))
         (cmd (concat
               compile-command
               " && "
               run-command)))
    (message filename)
    (message cmd)
    (with-temp-file filename (insert full-body))
    ;; compile, run, process result
    (org-babel-reassemble-table
     (org-babel-java-evaluate cmd result-type result-params nil)
     (org-babel-pick-name nil nil)
     (org-babel-pick-name nil nil))))

(provide 'ob-kotlin)

;;; ob-kotlin.el ends here
