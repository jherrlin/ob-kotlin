;;; ob-kotlin.el --- org-babel functions for kotlin evaluation -*- lexical-binding: t -*-

;;; Commentary:

;; Org-Babel support for evaluating kotlin source code.
;; Major parts of this code is reuse from ob-java

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)

(add-to-list 'org-babel-tangle-lang-exts '("kotlin" . "kotlin"))

(defgroup ob-kotlin nil
  "org-babel functions for kotlin evaluation"
  :group 'org)

(defcustom org-babel-kotlin-command "java"
  "Name of the kotlin (java) command.
May be either a command in the path, like java or an absolute
path name, like /usr/bin/java."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'string)

(defcustom org-babel-kotlin-compiler "kotlinc"
  "Name of the kotlin compiler.
May be either a command in the path, like kotlinc or an absolute
path name, like /opt/homebrew/bin/kotlinc."
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

(defconst org-babel-kotlin--imports-re (rx line-start (0+ space) "import"
                                         (opt (1+ space) "static")
					 (1+ space) (group (1+ (in alnum ?_ ?. ?*))) ; capture the fully qualified class name
					 (0+ space) (opt ?\;) line-end)
  "Regexp for import statements.")

(defun org-babel-kotlin--move-past (re)
  "Move point past the first occurrence of the given regexp RE."
  (while (re-search-forward re nil t)
    (goto-char (1+ (match-end 0)))))

(defun org-babel-expand-body:kotlin (body params)
  "Expand BODY with PARAMS."
  (let* ((imports-val (assq :imports params))
         (imports (if imports-val
                      (split-string (org-babel-read (cdr imports-val) nil) " ")
                    nil)))
    (with-temp-buffer
      (insert body)

      ;; wrap main if it doesn't exist
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
  (let* (;; header args for result processing
         (result-type (cdr (assq :result-type params)))
         (result-params (cdr (assq :result-params params)))
         (classpath (cdr (assq :classpath params)))
         (filename "main.kt")
         (full-body (org-babel-expand-body:kotlin body params))
         (compile-command
          (concat org-babel-kotlin-compiler " " filename " "
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
    (with-temp-file filename (insert full-body))
    ;; compile, run, process result
    (org-babel-reassemble-table
     (org-babel-java-evaluate cmd result-type result-params nil)
     (org-babel-pick-name nil nil)
     (org-babel-pick-name nil nil))))

(provide 'ob-kotlin)

;;; ob-kotlin.el ends here
