;;; consult-hoogle.el --- Hoogle frontend using consult -*- lexical-binding: t; -*-
;;
;; Created: April 10, 2022
;; Modified: April 10, 2022
;; License: GPL-3.0-or-later
;; Version: 0.0.1
;; Keywords: docs languages
;; Homepage: https://github.com/aikrahguzar/consult-hoogle
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Search the local hoogle database from Emacs using the nicities provided by
;; consult.
;;
;;; Code:

;;;; Packages
(require 'consult)
(require 'subr-x)
(require 'haskell-mode)
(require 'shr)

;;;; Variables
(defcustom consult-hoogle-args "hoogle search --jsonl -q --count=100" "The hoogle invocation used to get results." :type 'string :group 'consult)

(defcustom consult-hoogle-show-module-and-package t "Whether to show the package and module in the candidate line." :type 'boolean :group 'consult)

(defvar consult-hoogle--history nil "Variable to store history for hoogle searches.")

(defvar consult-hoogle-map (let ((map (make-sparse-keymap)))
                             (define-key map (kbd "M-i") #'consult-hoogle-browse-item)
                             (define-key map (kbd "M-j") #'consult-hoogle-browse-package)
                             (define-key map (kbd "M-m") #'consult-hoogle-browse-module)
                             (define-key map (kbd "M-<up>") #'consult-hoogle-scroll-docs-down)
                             (define-key map (kbd "M-<down>") #'consult-hoogle-scroll-docs-up)
                             (define-key map (kbd "TAB") #'consult-hoogle-restrict-to-package)
                             map))

;;;; Constructing the string to display
(defun consult-hoogle--builder (input)
  "Build command line given CONFIG and INPUT."
  (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
    (unless (string-blank-p arg)
      (list :command (append (split-string-and-unquote consult-hoogle-args) (list arg) opts)
            :highlight (cdr (consult--default-regexp-compiler input 'basic t))))))

(defun consult-hoogle--format (lines) "Format the LINES from hoogle result."
       (seq-map #'consult-hoogle--format-result lines))

(defun consult-hoogle--fontify (text)
  "Fontify TEXT, returning the fontified text.
This is adapted from `haskell-fontify-as-mode' but for better performance
we use the same buffer throughout."
  (with-current-buffer " *Hoogle Fontification*"
    (erase-buffer) (insert text) (font-lock-ensure) (buffer-substring (point-min) (point-max))))

(defun consult-hoogle--format-value (item in module from package) "Construct the disaply string from ITEM IN MODULE FROM and PACKAGE."
         (if (not consult-hoogle-show-module-and-package) item (concat item from module in package)))

(defun consult-hoogle--name (item &optional face) "Return name of ITEM with FACE." (propertize (cadr (split-string item nil t " +")) 'face face))

(defun consult-hoogle--format-result (json) "Parse the JSON resturned by hoogle to construct a result."
       (when-let ((parsed (ignore-errors (json-parse-string json :object-type 'alist))))
         (let* ((in (propertize " in " 'face 'font-lock-comment-face))
                (from (propertize " from " 'face 'font-lock-comment-face))
                (module (cl-callf propertize (alist-get 'name (alist-get 'module parsed) "") 'face 'haskell-keyword-face))
                (package (cl-callf propertize (alist-get 'name (alist-get 'package parsed) "") 'face 'haskell-quasi-quote-face))
                (display (pcase (alist-get 'type parsed)
                           (""  (consult-hoogle--format-value (cl-callf consult-hoogle--fontify (alist-get 'item parsed)) from module in package))
                           ("module" (concat "Module " (cl-callf consult-hoogle--name (alist-get 'item parsed) 'haskell-keyword-face) in package))
                           ("package" (concat "Package " (cl-callf consult-hoogle--name (alist-get 'item parsed) 'haskell-quasi-quote-face))))))
           (propertize display 'consult--candidate parsed))))

;;;; Following the urls from hoogle results.
(defun consult-hoogle--browse-url (type &optional alist) "Open the url of TYPE from ALIST."
       (let-alist alist
       (if-let ((type-url (pcase .type
                            ("" type)
                            ("module" (if (eq type 'module) 'item type))
                            ("package" (if (eq type 'package) 'item type))))
                (url (if (eq 'item type-url) .url (alist-get 'url (alist-get type-url alist)))))
           (if (or (eq type 'package) (equal .type "package"))
                (browse-url (concat url "index.html"))
             (browse-url url))
         (message "No suitable url for current alist."))))

;;;; Constructing the details buffer for the selected result
(defun consult-hoogle--doc-line (label elem item) "Construct a line for doc buffer from LABEL ELEM and ITEM."
       (concat (propertize label 'face 'bold) (if (equal "" elem) item elem) "\n"))

(defun consult-hoogle--details (alist) "Construct the details from ALIST."
       (let-alist alist
         (let* ((package-line (consult-hoogle--doc-line "Package: " .package.name .item))
                (module-line (unless (equal "package" .type) (consult-hoogle--doc-line "Module: " .module.name .item)))
                (item-line (when (equal .type "") (concat .item "\n"))))
           (insert (concat item-line module-line package-line)))
         (let ((beg (point)))
           (insert .docs) (shr-render-region beg (point-max)) (goto-char (point-min)))))

(defun consult-hoogle--show-details (action cand) "Show the details for the current CAND and handle ACTION."
       (when-let (((equal (buffer-name) " *Hoogle Documentation*"))
                  (inhibit-read-only t))
         (erase-buffer)
         (pcase action
           ('preview (when cand (consult-hoogle--details cand)))
           ('return (kill-buffer-and-window)))))

;;;; Refining searches
(defun consult-hoogle--add-to-input (addition) "Add ADDITION to the async part of the input."
       (let* ((initial (plist-get (alist-get consult-async-split-style consult-async-split-styles-alist) :initial))
              (input (minibuffer-contents)))
         (delete-minibuffer-contents)
         (insert (replace-regexp-in-string (if initial (rx bos (0+ space) (group (opt punct))) (rx bos))
                                           (lambda (match) (concat match (when (not (match-string 1 match)) initial) addition " "))
                                           input))))

(defun consult-hoogle--get (key alist) "Return the value for KEY from the ALIST."
       (let-alist alist
         (pcase .type
           ("" (alist-get 'name (alist-get key alist)))
           ("module" (if (eq key 'module) .item .package.name))
           ("package" .item))))

;;;; Consult integration
(defun consult-hoogle--candidate () "Get the current candidate."
       (get-text-property 0 'consult--candidate (run-hook-with-args-until-success 'consult--completion-candidate-hook)))

(defun consult-hoogle--search (&optional state action)
  "Search the local hoogle database and take ACTION with the selection.
STATE is the optional state function passed to the consult--read."
  (let ((consult-async-min-input 0)
        (fun (or action (lambda (alist) (consult-hoogle--browse-url 'item alist)))))
    (with-current-buffer (get-buffer-create " *Hoogle Fontification*" t) (let (haskell-mode-hook) (haskell-mode)))
    (unwind-protect
        (funcall fun (consult--read (consult--async-command #'consult-hoogle--builder
                                      (consult--async-map #'consult-hoogle--format-result)
                                      (consult--async-highlight #'consult-hoogle--builder))
                                    :prompt "Hoogle: " :require-match t :initial (consult--async-split-initial "")
                                    :lookup #'consult--lookup-candidate :state state :sort nil
                                    :keymap consult-hoogle-map
                                    :add-history (consult--async-split-thingatpt 'symbol)
                                    :history '(:input consult-hoogle--history)))
      (when-let ((buf (get-buffer " *Hoogle Fontification*"))) (kill-buffer buf)))))

;;;; Interactive Commands
;;;###autoload
(defun consult-hoogle (arg)
  "Search the local hoogle database.
By default this shows the documentation for the current candidate in a side
window. This can be disabled by a prefix ARG."
  (interactive (list current-prefix-arg))
  (if arg (consult-hoogle--search)
    (let* ((buf (get-buffer-create " *Hoogle Documentation*" t))
           (window (display-buffer buf '(display-buffer-in-side-window (window-height . ,(+ 3 vertico-count)) (side . bottom) (slot . -1)))))
      (with-current-buffer buf (visual-line-mode) (read-only-mode))
      (with-selected-window window (consult-hoogle--search #'consult-hoogle--show-details)))))

(defun consult-hoogle-browse-item () "Browse the url for current item." (interactive)
       (consult-hoogle--browse-url 'item (consult-hoogle--candidate)))

(defun consult-hoogle-browse-module () "Browse the url for the module the current item belongs to." (interactive)
       (consult-hoogle--browse-url 'module (consult-hoogle--candidate)))

(defun consult-hoogle-browse-package () "Browse the url for the package the current item belongs to." (interactive)
       (consult-hoogle--browse-url 'package (consult-hoogle--candidate)))

(defun consult-hoogle-scroll-docs-down (&optional arg) "Scroll the window with documentation ARG lines down." (interactive)
       (with-selected-window (get-buffer-window " *Hoogle Documentation*") (scroll-down arg)))

(defun consult-hoogle-scroll-docs-up (&optional arg) "Scroll the window with documentation ARG lines down." (interactive)
       (with-selected-window (get-buffer-window " *Hoogle Documentation*") (scroll-up arg)))

(defun consult-hoogle-restrict-to-package (package) "Restrict the search to PACKAGE."
       (interactive (list (consult-hoogle--get 'package (consult-hoogle--candidate))))
       (when package (consult-hoogle--add-to-input (concat "+" package))))

(provide 'consult-hoogle)

;;; consult-hoogle.el ends here
