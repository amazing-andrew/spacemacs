
;;* Packages

(packages:
  cython-mode
  (helm-pydoc :requires helm)
  hy-mode
  importmagic
  live-py-mode
  (nose :location local)
  pip-requirements
  pipenv
  pippel
  py-isort
  pyenv-mode
  (pylookup :location local)
  pytest
  (python :location built-in)
  pyvenv
  yapfify
  ;; packages for anaconda backend
  anaconda-mode
  (company-anaconda :requires company)
  ;; packages for lsp backend
  (lsp-python :requires lsp-mode))

;; (init: importmagic)

;; (pre-init: company)

;; (post-init: company)

;; (key-bindings:
;;   :major-mode python-mode
;;   :prefix
;;   "me" "myprefix"
;;   :keys
;;   "e" 'function
;;   "f" 'function
;;   )

;; (>extended-packages
;;  company
;;  counsel-gtags
;;  eldoc
;;  evil-matchit
;;  flycheck
;;  ggtags
;;  helm-cscope
;;  helm-gtags
;;  org
;;  semantic
;;  smartparens
;;  stickyfunc-enhance
;;  xcscope
;;  )

;;* Configuration
