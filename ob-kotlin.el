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

(defun org-babel-execute:kotlin (body params)
  "Execute a kotlin source block with BODY code and PARAMS params."
  (message body)
  (let* (;; header args for result processing
         (result-type (cdr (assq :result-type params)))
         (result-params (cdr (assq :result-params params)))
         (filename "main.kt")
         (full-body body)
         (compile-command "kotlinc main.kt -classpath kotlinx-coroutines-core-jvm-1.6.4.jar -include-runtime -d main.jar")
         (run-command "java -cp kotlinx-coroutines-core-jvm-1.6.4.jar:main.jar MainKt")
         (cmd (concat
               compile-command
               " && "
               run-command)))
    (message "Inserting code into:" filename)
    (message "Running:" cmd)
    (with-temp-file filename (insert full-body))
    ;; compile, run, process result
    (org-babel-reassemble-table
     (org-babel-java-evaluate cmd result-type result-params nil)
     (org-babel-pick-name nil nil)
     (org-babel-pick-name nil nil))))
