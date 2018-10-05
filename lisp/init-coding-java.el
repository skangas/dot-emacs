;; Java

(use-package jde
  :config
  (setq-default jde-enable-abbrev-mode t)
  (setq jde-web-browser "conkeror")
  (setq jde-doc-dir "/usr/share/doc/openjdk-6-doc/")
  (setq jde-jdk-doc-url "/usr/share/doc/openjdk-6-jdk/api/index.html")
  (setq jde-jdk-registry '(("1.6" . "/usr/lib/jvm/java-6-openjdk/")))
  (setq jde-jdk '("1.6"))
  (setq jde-global-classpath '("/usr/share/java"
                               "~/src/jfsaccounting/administration/target/classes"))
  (setq jde-sourcepath '("~/src/jfsaccounting/administration/src/"))

  (defun my-java-mode-customizations ()
    (my-coding-keys java-mode-map)        

    ;; Compile
    (unless (file-exists-p "Makefile")
      (set (make-local-variable 'compile-command)
           (let ((file (file-name-nondirectory buffer-file-name)))
             (concat "javac " file))))

    ;;  (set (make-local-variable 'require-final-newline) nil)
    (setq c-basic-offset 4
          tab-width 4
          indent-tabs-mode nil))
  (add-hook 'jde-mode-hook 'my-java-mode-customizations)

  (after 'jde-javadoc
    (jde-javadoc-define-template 'jde-javadoc-author-tag-template "\" * @author \" user-full-name")
    (jde-javadoc-define-template 'jde-javadoc-version-tag-template "\"* @version $Id$\"")
    (jde-javadoc-define-template 'jde-javadoc-describe-class-template nil)
    (jde-javadoc-define-template 'jde-javadoc-describe-constructor-template "\"Constructs a new \" name \" object.\"")
    (jde-javadoc-define-template 'jde-javadoc-describe-method-template nil)
    (jde-javadoc-define-template 'jde-javadoc-describe-field-template "\"* \"")
    (jde-javadoc-define-template 'jde-javadoc-param-tag-template "\"* @param \" name")
    (jde-javadoc-define-template 'jde-javadoc-return-tag-template "\"* @return \"")))

(provide 'init-coding-java)

;; init-coding-java.el ends here
