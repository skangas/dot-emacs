(use-package hydra
  :bind (("M-g TAB" . sk/gogo/body)
         ("<f1>" . hydra-help/body)
         ("C-c m" . hydra-multiple-cursors/body))
  :config

  (defhydra sk/gogo (:exit nil)
    "filter"
    ("n" (next-error) "next-error")
    ("p" (previous-error) "prev-error")
    ("f" (flyspell-goto-next-error) "flyspell-next")
    ("i" (ispell-word) "ispell-word"))

  (defhydra sk/go (:exit nil)
    "filter"
    ("1" (goto-random-line) "random")
    ("2" (dired-find-file) "open")
    ("3" (progn
           (goto-random-line)
           (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))

             (dired-find-file))) "open")
    ("5" (dired-do-delete) "delete")
    ("q" nil "quit" :color blue))

  ;; TODO
  ;; (defhydra hydra-clock-in (:exit t)
  ;;   ""
  ;;   )

  (defhydra hydra-smerge (:color pink
                                 :hint nil
                                 :pre (smerge-mode 1)
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("q" nil "cancel" :color blue))

  (defhydra hydra-help (:exit t)
    ;; Better to exit after any command because otherwise helm gets in a
    ;; mess, set hint to nil: written out manually.
    
    "
  Describe        ^^Keys                    ^^Search                    ^^Documentation
  ---------------------------------------------------------------------------------------
  _f_unction        _k_eybinding              _a_propros                  _i_nfo
  _p_ackage         _w_here-is                _d_oc strings               _n_: man
  _m_ode            _b_: show all bindings    _s_: info by symbol         _h_elm-dash
  _v_ariable

  "
    ;; Boring help commands...
    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("C" describe-coding-system "coding-system")
    ("I" describe-input-method "input-method")
    
    
    ;; Documentation
    ("i" info nil)
    ("n" helm-man-woman nil)
    ("h" helm-dash)

    ;; Keybinds
    ("b" describe-bindings nil)
    ("c" describe-key-briefly nil)
    ("k" describe-key nil)
    ("w" where-is nil)

    ;; Search
    ("a" apropos-command nil)
    ("d" apropos-documentation nil)
    ("s" info-lookup-symbol nil)

    ;; Describe
    ("f" describe-function nil)
    ("p" describe-package nil)
    ("m" describe-mode nil)
    ("v" describe-variable nil)
    ("y" describe-syntax nil)

    ;; quit
    ("q" help-quit "quit"))

  (defhydra hydra-multiple-cursors (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-n" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-p" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil))

  (defhydra hydra-window-stuff (:hint nil)
    "
          Split: _v_ert  _s_:horz
         Delete: _c_lose  _o_nly
  Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
        Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
         Winner: _u_ndo  _r_edo
         Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
           Move: _a_:up  _z_:down  _i_menu"


    ("z" scroll-up-line)
    ("a" scroll-down-line)
    ("i" idomenu)

    ("u" winner-undo)
    ("r" winner-redo)

    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)

    ("p" previous-buffer)
    ("n" next-buffer)
    ("b" ido-switch-buffer) 
    ("f" ido-find-file)
    ("F" projectile-find-file)
    
    ("s" split-window-below)
    ("v" split-window-right)

    ("c" delete-window)
    ("o" delete-other-windows)

    ("H" hydra-move-splitter-left)
    ("J" hydra-move-splitter-down)
    ("K" hydra-move-splitter-up)
    ("L" hydra-move-splitter-right)

    ("q" nil)))

(provide 'init-hydra)
