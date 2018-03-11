;; -*- nameless-current-name: "ddls"; lexical-binding: t -*-
;;; core-double-dot-layer-system.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; Spacemacs Layer system 2.0
;;
;;; License: GPLv3


;; Interface

(defmacro layers: (&rest layers)
  "Define used layers."
  (declare (indent defun))
  ;; (setq dotspacemacs--configuration-layers-saved
  ;;       dotspacemacs-configuration-layers)
  )

(defmacro packages: (&rest packages)
  "Define owned packages."
  (declare (indent defun))
  (let ((layer-name (ddls//get-directory-name
                     (if load-file-name
                         ;; File is being loaded
                         (file-name-directory load-file-name)
                       ;; File is being evaluated
                       default-directory))))
    `(setq ,(ddls//package:-variable-name layer-name) ',packages)))

(defmacro init: ()
  "Initialize a package."
  (declare (indent defun)))

(defmacro pre-init: ()
  "Initialize a package before calling its `init:'."
  (declare (indent defun)))

(defmacro post-init: ()
  "Initialize a package after calling its `init:'."
  (declare (indent defun)))

(defmacro key-bindings: ()
  "Define major-mode or minor mode key-bindings behind the leader keys."
  (declare (indent defun))
  )


;; Variables

(defconst ddls-layers-directory
  (expand-file-name (concat spacemacs-start-directory "layers/"))
  "Spacemacs layers directory.")

(defconst ddls-private-directory
  (expand-file-name (concat spacemacs-start-directory "private/"))
  "Spacemacs private layers base directory.")

(defconst ddls-private-layer-directory
  (let ((dotspacemacs-layer-dir
         (when dotspacemacs-directory
           (expand-file-name
            (concat dotspacemacs-directory "layers/")))))
    (if (and dotspacemacs-directory
             (file-exists-p dotspacemacs-layer-dir))
        dotspacemacs-layer-dir
      ddls-private-directory))
  "Spacemacs default directory for private layers.")

(defconst ddls--indexed-layers-filepath (concat ddls-layers-directory "index.el")
  "File containing the cached index of all layers shipped with Spacemacs.")

(defvar ddls-categories '()
  "List of strings corresponding to category names. A category is a
directory with a name starting with `+'.")

(defvar ddls--indexed-layers (make-hash-table :size 1024)
  "Hash map to index `ddls-layer' objects by their names.")

(defvar ddls--used-layers '()
  "A non-sorted list of used layer name symbols.")

(defvar ddls-exclude-all-layers nil
  "If non nil then only the distribution layer is loaded.")

(defvar ddls--inhibit-warnings nil
  "If non-nil then warning messages emitted by the layer system are ignored.")

;; Classes

(defclass ddls-layer ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the layer.")
   (dir :initarg :dir
        :initform nil
        :type (satisfies (lambda (x) (or (null x) (stringp x))))
        :documentation "Absolute path to the layer directory.")
   (packages :initarg :packages
             :initform nil
             :type list
             :documentation "List of package symbols declared in this layer.")
   (selected-packages :initarg :selected-packages
             :initform 'all
             :type (satisfies (lambda (x) (or (and (symbolp x) (eq 'all x))
                                              (listp x))))
             :documentation "List of selected package symbols.")
   (variables :initarg :variables
              :initform nil
              :type list
              :documentation "A list of variable-value pairs.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the layer needs to be installed")
   (disabled :initarg :disabled-for
             :initform nil
             :type list
             :documentation "A list of layers where this layer is disabled.")
   (enabled :initarg :enabled-for
            :initform 'unspecified
            :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
            :documentation
            (concat "A list of layers where this layer is enabled. "
                    "(Takes precedence over `:disabled-for'.)"))
   ;; Note:
   ;; 'can-shadow' is a commutative relation:
   ;;     if Y 'can-shadow' X then X 'can-shadow' Y
   ;; but the 'shadow' operation is not commutative, the order of the operands
   ;; is determined by the order of the layers in the dotfile
   ;; (variable: dotspacemacs-configuration-layers)
   (can-shadow :initarg :can-shadow
               :initform 'unspecified
               :type (satisfies (lambda (x) (or (listp x) (eq 'unspecified x))))
               :documentation "A list of layers this layer can shadow."))
  "A configuration layer.")

(defmethod ddls-layer-owned-packages ((layer ddls-layer) &optional props)
  "Return the list of owned packages by LAYER.
If PROPS is non-nil then return packages as lists with their properties.
LAYER has to be installed for this method to work properly."
  (delq nil (mapcar
             (lambda (x)
               (let* ((pkg-name (if (listp x) (car x) x))
                      (pkg (configuration-layer/get-package pkg-name)))
                 (when (eq (oref layer :name) (car (oref pkg :owners))) x)))
             (ddls-layer-get-packages layer props))))

(defmethod ddls-layer-owned-packages ((layer nil) &optional props)
  "Accept nil as argument and return nil."
  nil)

(defmethod ddls-layer-get-shadowing-layers ((layer ddls-layer))
  "Return the list of used layers that shadow LAYER."
  (let ((rank (cl-position (oref layer :name) configuration-layer--used-layers))
        (shadow-candidates (oref layer :can-shadow))
        shadowing-layers)
    (when (and (numberp rank)
               (not (eq 'unspecified shadow-candidates))
               (listp shadow-candidates))
      (mapcar
       (lambda (other)
         (let ((orank (cl-position other configuration-layer--used-layers)))
           ;; OTHER shadows LAYER if and only if OTHER's rank is bigger than
           ;; LAYER's rank.
           (when (and (numberp orank) (< rank orank))
             (add-to-list 'shadowing-layers other))))
       ;; since the 'can-shadow' relation is commutative it is safe to use this
       ;; list, i.e. if LAYER can shadow layers X and Y then X and Y can shadow
       ;; LAYER.
       shadow-candidates))
    shadowing-layers))

(defmethod ddls-layer-get-packages ((layer ddls-layer) &optional props)
  "Return the list of packages for LAYER.
If PROPS is non-nil then return packages as lists along with their properties."
  (let ((all (eq 'all (oref layer :selected-packages))))
    (delq nil (mapcar
               (lambda (x)
                 (let ((pkg-name (if (listp x) (car x) x)))
                   (when (or all (memq pkg-name
                                       (oref layer :selected-packages)))
                     (if props x pkg-name))))
               (oref layer :packages)))))

(defun ddls/make-indexed-layer (name dir)
  "Return a `ddls-layer' object whose goal is to be indexed.
NAME is the name of the layer passed as a string.
DIR is the directory where the layer is, if it is nil then search in the indexed
layers for the path.
Return nil if the passed DIR is not valid."
  (let ((obj (cfgl-layer (symbol-name name) :name name)))
    (if (or (null dir)
            (and dir (not (file-exists-p dir))))
        (ddls//warning "Cannot make layer %S without a valid directory!" name)
      (let* ((dir (file-name-as-directory dir)))
        (oset obj :dir dir)
        obj))))

(defun ddls/set-indexed-layer (layer-specs)
  "Set slots info from LAYER-SPECS for corresponding indexed layer."
  (let* ((layer-name (if (listp layer-specs) (car layer-specs) layer-specs))
         (indexed-layer (ddls/get-indexed-layer layer-name)))
    (if indexed-layer
        (let* ((disabled (when (listp layer-specs)
                           (spacemacs/mplist-get layer-specs :disabled-for)))
               (enabled (if (and (listp layer-specs)
                                 (memq :enabled-for layer-specs))
                            (spacemacs/mplist-get layer-specs :enabled-for)
                          'unspecified))
               (variables (when (listp layer-specs)
                            (spacemacs/mplist-get layer-specs :variables)))
               (shadow (if (and (listp layer-specs)
                                (memq :can-shadow layer-specs))
                           (spacemacs/mplist-get layer-specs :can-shadow)
                         'unspecified))
               (dir (oref indexed-layer :dir))
               (main-file (concat dir "main.el"))
               (packages (progn
                           (load main-file)
                           (symbol-value (ddls//package:-variable-name
                                          layer-name))))
               (selected-packages (ddls//select-packages layer-specs packages)))
          (oset indexed-layer :disabled-for disabled)
          (oset indexed-layer :enabled-for enabled)
          (oset indexed-layer :variables variables)
          (oset indexed-layer :packages packages)
          (oset indexed-layer :selected-packages selected-packages)
          (unless (eq 'unspecified shadow)
            (oset indexed-layer :can-shadow shadow))
          indexed-layer)
      (ddls//warning "Unknown layer '%s' declared in dotfile." layer-name)
      nil)))

(defclass ddls-package ()
  ((name :initarg :name
         :type symbol
         :documentation "Name of the package.")
   (min-version :initarg :min-version
                :initform nil
                :type list
                :documentation "Minimum version to install as a version list.")
   (owners :initarg :owners
           :initform nil
           :type list
           :documentation "The layer defining the init function.")
   (pre-layers :initarg :pre-layers
               :initform '()
               :type list
               :documentation "List of layers with a pre-init function.")
   (post-layers :initarg :post-layers
                :initform '()
                :type list
                :documentation "List of layers with a post-init function.")
   (location :initarg :location
             :initform elpa
             :type (satisfies (lambda (x)
                                (or (stringp x)
                                    (memq x '(built-in local site elpa))
                                    (and (listp x) (eq 'recipe (car x))))))
             :documentation "Location of the package.")
   (toggle :initarg :toggle
           :initform t
           :type (satisfies (lambda (x) (or (symbolp x) (listp x))))
           :documentation
           "Package is enabled/installed if toggle evaluates to non-nil.")
   (step :initarg :step
         :initform nil
         :type (satisfies (lambda (x) (member x '(nil bootstrap pre))))
         :documentation "Initialization step.")
   (lazy-install :initarg :lazy-install
                 :initform nil
                 :type boolean
                 :documentation
                 "If non-nil then the package needs to be installed")
   (protected :initarg :protected
              :initform nil
              :type boolean
              :documentation
              "If non-nil then this package cannot be excluded.")
   (excluded :initarg :excluded
             :initform nil
             :type boolean
             :documentation
             "If non-nil this package is excluded from all layers.")
   (requires :initarg :requires
             :initform nil
             :type list
             :documentation
             "Packages that must be enabled for this package to be enabled.")))

(defmethod ddls-package-toggled-p ((pkg ddls-package) &optional inhibit-messages)
  "Evaluate the `toggle' slot of passed PKG.
If INHIBIT-MESSAGES is non nil then any message emitted by the toggle evaluation
is ignored."
  (let ((message-log-max (unless inhibit-messages message-log-max))
        (toggle (oref pkg :toggle)))
    (eval toggle)))

(defmethod ddls-package-reqs-satisfied-p ((pkg ddls-package) &optional inhibit-messages)
  "Check if requirements of a package are all enabled.
If INHIBIT-MESSAGES is non nil then any message emitted by the toggle evaluation
is ignored."
  (not (memq nil (mapcar
                  (lambda (dep-pkg)
                    (let ((pkg-obj (configuration-layer/get-package dep-pkg)))
                      (when pkg-obj
                        (ddls-package-enabled-p pkg-obj inhibit-messages))))
                  (oref pkg :requires)))))

(defmethod ddls-package-enabled-p ((pkg ddls-package) &optional inhibit-messages)
  "Check if a package is enabled.
This checks the excluded property, evaluates the toggle, if any, and recursively
checks whether dependent packages are also enabled.
If INHIBIT-MESSAGES is non nil then any message emitted by the toggle evaluation
is ignored."
  (and (or (oref pkg :protected) (not (oref pkg :excluded)))
       (ddls-package-reqs-satisfied-p pkg inhibit-messages)
       (ddls-package-toggled-p pkg inhibit-messages)))

(defmethod ddls-package-used-p ((pkg ddls-package))
  "Return non-nil if PKG is a used package."
  (and (not (null (oref pkg :owners)))
       (not (oref pkg :excluded))
       (ddls-package-enabled-p pkg t)))

(defmethod ddls-package-distant-p ((pkg ddls-package))
  "Return non-nil if PKG is a distant package (i.e. not built-in Emacs)."
  (and (not (memq (oref pkg :location) '(built-in site local)))
       (not (stringp (oref pkg :location)))))

(defmethod ddls-package-get-safe-owner ((pkg ddls-package))
  "Safe method to return the name of the layer which owns PKG."
  ;; The owner of a package is the first *used* layer in `:owners' slot.
  ;; Note: for packages in `configuration-layer--used-packages' the owner is
  ;; always the car of the `:owners' slot.
  (let ((layers (oref pkg :owners)))
    (while (and (consp layers)
                (not (configuration-layer/layer-used-p (car layers))))
      (pop layers))
    (when (configuration-layer/layer-used-p (car layers))
      (car layers))))

(defmethod ddls-package-set-property ((pkg ddls-package) slot value)
  "Set SLOT to the given VALUE for the package PKG.
If `configuration-layer--package-properties-read-onlyp' is non-nil then VALUE
is not set for the given SLOT."
  (unless configuration-layer--package-properties-read-onlyp
    (eval `(oset pkg ,slot value))))


;; Functions

;; (defun ddls/load-used-layers ()
;;   "Load the used layers from the dotfile."
;;   (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers..."))

(defun ddls/test ()
  (interactive)
  (let ((start-time (current-time)))
    (ddls/index-layers)
    (ddls/read-used-layers-specs)
    (message "elapsed time: %.3fs"
             (float-time (time-subtract (current-time) start-time)))))

(defun ddls/create-spacemacs-layers-index-file ()
  "Scan `ddls-layers-directory' to index layers then cache the index in a file."
  (interactive)
  (setq ddls--indexed-layers (make-hash-table :size 1024))
  (ddls//index-layers-from-directory (list ddls-layers-directory))
  (spacemacs/dump-vars-to-file '(ddls--indexed-layers) ddls--indexed-layers-filepath))

(defun ddls/index-layers ()
  "Index all discovered layers in layer directories."
  ;; load cached index layers first
  (load ddls--indexed-layers-filepath)
  ;; then crawl file system for user's private layers
  (let ((dirs `(;; layers in private folder ~/.emacs.d/private
                ,ddls-private-directory
                ;; layers in dotdirectory
                ;; this path may not exist, so check if it does
                ,(when dotspacemacs-directory
                   (let ((dir (expand-file-name (concat dotspacemacs-directory
                                                        "layers/"))))
                     (when (file-exists-p dir) (list dir))))
                ;; additional layer directories provided by the user
                ,dotspacemacs-configuration-layer-path)))
    (message "dirs: %s" dirs)
    (ddls//index-layers-from-directory dirs)))

(defun ddls//index-layers-from-directory (directories)
  "Index all discovered layers in DIRECTORIES."
  (let ((search-paths directories)
        (discovered '()))
    ;; filter out directories that don't exist
    (setq search-paths
          (ddls/filter-objects
           search-paths
           (lambda (x)
             (when x
               (let ((exists (file-exists-p x)))
                 (unless exists
                   (ddls//warning
                    "Layer directory \"%s\" not found. Ignoring it." x))
                 exists)))))
    ;; depth-first search of subdirectories
    (while search-paths
      (let ((current-path (car search-paths)))
        (setq search-paths (cdr search-paths))
        (dolist (sub (directory-files current-path t nil 'nosort))
          ;; ignore ".", ".." and non-directories
          (unless (or (string-equal ".." (substring sub -2))
                      (string-equal "." (substring sub -1))
                      (not (file-directory-p sub)))
            (let ((type (ddls//directory-type sub)))
              (cond
               ((eq 'category type)
                (let ((category (ddls//get-category-from-path
                                 sub)))
                  (spacemacs-buffer/message "-> Discovered category: %S" category)
                  (push category ddls-categories)
                  (setq search-paths (cons sub search-paths))))
               ((eq 'layer type)
                (let* ((layer-name-str (file-name-nondirectory sub))
                       (layer-name (intern layer-name-str))
                       (indexed-layer (ddls/get-indexed-layer layer-name)))
                  (if indexed-layer
                      (ddls//warning
                       (concat
                        "Duplicated layer %s detected in directory \"%s\", "
                        "replacing old directory \"%s\" with new directory.")
                       layer-name-str sub (oref indexed-layer :dir))
                    (spacemacs-buffer/message
                     "-> Discovered configuration layer: %s"
                     layer-name-str)
                    (ddls//add-indexed-layer
                     (ddls/make-indexed-layer layer-name sub)))))
               (t
                ;; layer not found, add it to search path
                (setq search-paths (cons sub search-paths)))))))))))

(defun ddls/read-used-layers-specs ()
  "Read used layers specs and set slots of corresponding indexed layers."
  (dotspacemacs|call-func dotspacemacs/layers "Calling dotfile layers...")
  (unless ddls-exclude-all-layers
    (dolist (layer-specs dotspacemacs-configuration-layers)
      (let ((layer (ddls/set-indexed-layer layer-specs)))
        (when layer (ddls//add-used-layer layer))))))

(defun ddls//package:-variable-name (layer-name)
  "Return the variable name containing the list of packages for LAYER-NAME."
  (intern (format "%s-packages" layer-name)))

(defun ddls//select-packages (layer-specs packages)
  "Return the selected packages of LAYER-SPECS from given PACKAGES list."
  (if (null packages)
      ;; default value
      'all
    ;; return a list of packages that are actually used, or `all' if there is
    ;; not package selection.
    (let* ((value (when (listp layer-specs)
                    (spacemacs/mplist-get layer-specs :packages)))
           (selected-packages (if (and (not (null (car value)))
                                       (listp (car value)))
                                  (car value)
                                value)))
      (cond
       ;; select packages
       ((and selected-packages
             (not (memq (car selected-packages) '(all not))))
        selected-packages)
       ;; unselect packages
       ((and selected-packages
             (eq 'not (car selected-packages)))
        (delq nil (mapcar (lambda (x)
                            (let ((pkg-name (if (listp x) (car x) x)))
                              (unless (memq pkg-name selected-packages)
                                pkg-name)))
                          packages)))
       ;; no package selections or all package selected
       (t 'all)))))

(defun ddls//directory-type (path)
  "Return the type of directory pointed by PATH.
Possible return values:
  layer    - the directory is a layer
  category - the directory is a category
  nil      - the directory is a regular directory."
  (when (file-directory-p path)
    (if (string-match
         "^+" (file-name-nondirectory
               (directory-file-name
                (concat ddls-layers-directory path))))
        'category
      (let ((files (directory-files path)))
        ;; most frequent files encoutered in a layer are tested first
        (when (or (member "main.el" files)
                  (member "defer.el" files))
          'layer)))))

(defun ddls//get-category-from-path (dirpath)
  "Return a category symbol from the given DIRPATH.
The directory name must start with `+'.
Returns nil if the directory is not a category."
  (when (file-directory-p dirpath)
    (let ((dirname (file-name-nondirectory
                    (directory-file-name
                     (concat ddls-layers-directory
                             dirpath)))))
      (when (string-match "^+" dirname)
        (intern (substring dirname 1))))))

(defun ddls//get-directory-name (filepath)
  "Return the name of the parent directory for passed FILEPATH"
  (file-name-nondirectory (directory-file-name (file-name-directory filepath))))

(defun ddls//add-used-layer (layer)
  "Mark the passed LAYER object as used by adding it the used layer list."
  (add-to-list 'ddls--used-layers (oref layer :name)))

(defun ddls//add-indexed-layer (layer)
  "Index a LAYER object."
  (puthash (oref layer :name) layer ddls--indexed-layers))

(defun ddls/get-indexed-layer (layer-name)
  "Return a layer object with name LAYER-NAME.
Return nil if layer object is not found."
  (when (ht-contains? ddls--indexed-layers layer-name)
    (ht-get ddls--indexed-layers layer-name)))

(defun ddls/filter-objects (objects ffunc)
  "Return a filtered OBJECTS list where each element satisfies FFUNC."
  (reverse (cl-reduce (lambda (acc x) (if (funcall ffunc x) (push x acc) acc))
                      objects
                      :initial-value nil)))

(defun ddls//warning (msg &rest args)
  "Display MSG as a warning message in buffer `*Messages*'.
If `ddls--inhibit-warnings' is non nil then this function is a
no-op."
  (unless ddls--inhibit-warnings (apply 'spacemacs-buffer/warning msg args)))
