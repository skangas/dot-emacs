;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cedet

;;(semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

;;;;; BEGIN UGLY HACK
;; UGLY HACK to shut semantic up a bit
;; see http://thread.gmane.org/gmane.emacs.semantic/2434
;; This version is from semantic trunk Wed Dec 15 18:29:24 CET 2010
;; (load-file "~/.emacs.d/lisp/semantic-lex-spp.el")

;; This is a patched version of standard Emacs 23.2
(load-file "~/.emacs.d/lisp/lex-spp.el")
;;;;; END UGLY HACK

(semantic-mode)
(global-ede-mode t)
(global-semanticdb-minor-mode t)
(setq semanticdb-default-save-directory "~/.emacs.d/cache/semanticdb")

;; Setup Semantic C/C++ parsing based on GCC output.
(semantic-gcc-setup)

;; Setup projects
;; (let ((project-root-file "~/wip/freedroid/README"))
;;   (when (file-exists-p project-root-file)
;;     (ede-cpp-root-project ""
;;                           :name "Lumiera"
;;                           :file project-root-file
;;                           :include-path '("/src"
;;                                           "/tests"
;;                                           )
;;                           :system-include-path '("/usr/include/boost/"
;;                                                  "/usr/include/cairomm-1.0/cairomm/"
;;                                                  "/usr/include/gtkmm-2.4/"
;;                                                  "/usr/include/valgrind/")
;;                           :local-variables '((c-default-style . "gnu")
;;                                              (c-basic-offset . 2)
;;                                              (tab-width . 2)
;;                                              (indent-tabs-mode . nil))
;;                           :spp-table '(("isUnix" . "")
;;                                        ("BOOST_TEST_DYN_LINK" . "")))))

(let ((project-root-file "~/wip/lumiera/README"))
  (when (file-exists-p project-root-file)
    (ede-cpp-root-project "Lumiera"
                          :name "Lumiera"
                          :file project-root-file
                          :include-path '("/src"
                                          "/tests"
                                          )
                          :system-include-path '("/usr/include/boost/"
                                                 "/usr/include/cairomm-1.0/cairomm/"
                                                 "/usr/include/gtkmm-2.4/"
                                                 "/usr/include/valgrind/")
                          :local-variables '((c-default-style . "gnu")
                                             (c-basic-offset . 2)
                                             (tab-width . 2)
                                             (indent-tabs-mode . nil))
                          :spp-table '(("isUnix" . "")
                                       ("BOOST_TEST_DYN_LINK" . "")))))

;; Speedbar integration
(require 'semantic/sb)

;; MRU
(global-semantic-mru-bookmark-mode 1)

;; (global-semantic-show-parser-state-mode -1)
;; (global-semantic-show-unmatched-syntax-mode -1)

;; ;; imenu support
;; (add-hook 'semantic-init-hooks (lambda ()
;;                                  (imenu-add-to-menubar "TAGS")))

;; (setq semanticdb-project-roots '("~/src/jfsaccounting"))
;;                                  "~/research"))

(provide 'my-cedet)

;; my-cedet.el ends here
