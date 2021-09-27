(package-initialize)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;
;; package repos ;;
;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(alchemist . "melpa-stable") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't show the splash screen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t)

;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files
      (list "~/Desktop/Caseys_Files/OrgMode"))
(setq org-tag-alist '((:startgrouptag)
		      ("everything")
		      (:grouptags)
		      ("upperontology")
		      ("gist")
		      ("training")
		      ("datagovernance")
		      (:endgrouptag)))
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "BLOCKED" "|" "TOMORROW" "DONE")
	(sequence "CLEAN" "MODIFIED")))
(setq org-enforce-todo-dependencies t)
(setq org-log-done t)
(setq org-startup-truncated nil)
(setq org-support-shift-select t)

;; load languages for org-mode

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell . t)
			       (python . t)))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;;;;;
;; alchemist ;;
;;;;;;;;;;;;;;;

(defun iex-run()
  (interactive)
  (alchemist-iex-run))

;;;;;;;;;;;;;;;;
;; load theme ;;
;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook (lambda() (load-theme 'wombat-modified)))
;(add-hook 'after-init-hook (lambda() (load-theme 'brutalist)))
;(add-hook 'after-init-hook (lambda() (load-theme 'spacemacs-light)))
;(add-hook 'after-init-hook (lambda() (load-theme 'arjen-grey)))

;;;;;;;;;;;;;;;
;; TTL Files ;;
;;;;;;;;;;;;;;;

;; load ttl-mode
(load "~/Desktop/code_dev/Lisp_Programming/turtle-mode.el")

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;


(setq visible-bell 1)

;; select matching parens
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))

(defun select-in-parens ()
  (interactive)
  (set-mark (point))
  (goto-match-paren 1))


;;;;;;;;;;;;;;;;
;; SPARQL     ;;
;;;;;;;;;;;;;;;;
(load "~/Desktop/Caseys_Files/Personal Files/repositories/sparql-mode/sparql-mode.el")
(autoload 'sparql-mode "sparql-mode.el"
    "Major mode for editing SPARQL files" t)
(add-to-list 'auto-mode-alist '("\\.sparql$" . sparql-mode))
(add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode))

;;;;;;;;;;;;;;;;
;; sat solver ;;
;;;;;;;;;;;;;;;;

(defun load-sat-solver-tests ()
  (interactive)
  (goto-char (point-max))
  (insert
   "(load \"/home/tino/git-repositories/sat.-solver/tests.lisp\")")
  (slime-repl-return))


;;;;;;;;;;;
;; elpy
;;;;;;;;;;;

(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")


;;;;;;;;;;
;; misc
;;;;;;;;;;

(setq show-paren-mode t)


(defun xscheme ()
      "Loads xscheme and runs a scheme process in the current buffer."
      (interactive)
      (load-library "xscheme")
      (xscheme-start "scheme -emacs"
                     (buffer-name)
                     (buffer-name)))


;; rename file functionality
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(put 'upcase-region 'disabled nil)




;;; THE FOLLOWING ARE FOR TEMPORARY WORK. DROP THEM PERIODICALLY. CONSIDER MAKING MINOR MODES FOR THEM.

;; create destination
(defun destination (type label property value webaddress distance)
  (interactive "sType: \nsLabel: \nsProperty: \nsValue: \nsWeb: \nsDistance (in miles): ")
  (insert "[:destination [a "type" ; skos:prefLabel \""label"\"; "property" \""value"\"; rdfs:seeAlso \""webaddress"\";]; :length [:decimalValue \""distance"\"^^xsd:decimal ; :unit :_mile]]"))

;; create room on home honer
(defun contained-room (type label width length)
  (interactive "sType: \nsLabel: \nsWidth (in feet): \nsLength (in feet):")
  (insert "[a :"type" ; skos:prefLabel \""label"\" ; :width [:decimalValue \""width"\"^^xsd:decimal ; :unit :_foot] ;:length [:decimalValue \""length"\"^^xsd:decimal ; :unit :_foot] ; ] ;"))


;; creating user preferences
(defun create-quant-preference (name property type value min nutil max xutil strength)
  (interactive "sName: \nsProperty: \nsType (:Listing/:RealEstate/:Residence/:Lot): \nsDesired Value: \nsMinimum Value: \nsUtility for Minimum Value: \nsMaximum Value: \nsUtility for Maximum Value: \nsStrength: ")
  (insert
":" name" a :Preference ;
       :preferenceProperty " property" ;
       :preferenceForType " type" ;
       :preferenceValue-Quant \"" value"\"^^xsd:decimal ;
       :preferenceMin \"" min"\"^^xsd:decimal ;
       :preferenceMinValue \"" nutil"\"^^xsd:decimal ;
       :preferenceMax \"" max"\"^^xsd:decimal ;
       :preferenceMaxValue \"" xutil"\"^^xsd:decimal ;
       :preferenceStrength \"" strength"\"^^xsd:decimal ;
.\n\n"))

(defun create-magnitude-preference (name property type value unit min nutil max xutil strength)
  (interactive "sName: \nsProperty: \nsType (:Listing/:RealEstate/:Residence/:Lot): \nsDesired Value: \nsDesired Unit: \nsMinimum Value: \nsUtility for Minimum Value: \nsMaximum Value: \nsUtility for Maximum Value: \nsStrength: ")
  (insert
":" name" a :Preference ;
       :preferenceProperty " property" ;
       :preferenceForType " type" ;
       :preferenceValue-Object [:decimalValue \"" value"\"^^xsd:decimal; :unit " unit "] ;
       :preferenceMin \"" min"\"^^xsd:decimal ;
       :preferenceMinValue \"" nutil"\"^^xsd:decimal ;
       :preferenceMax \"" max"\"^^xsd:decimal ;
       :preferenceMaxValue \"" xutil"\"^^xsd:decimal ;
       :preferenceStrength \"" strength"\"^^xsd:decimal ;
.\n\n"))

(defun create-object-preference (name property type value strength)
  (interactive "sName: \nsProperty: \nsType (:Listing/:RealEstate/:Residence/:Lot): \nsDesired Value (blank node): \nsStrength: ")
  (insert
":" name" a :Preference ;
       :preferenceProperty " property" ;
       :preferenceForType " type" ;
       :preferenceValue-Object " value " ;
       :preferenceStrength \"" strength"\"^^xsd:decimal ;
.\n\n"))


;; mapping ttl stitching files
(defun declare-term-mapped ()
  (interactive)
  (search-forward "olive:Un")
  (backward-char 2)
  (delete-char 8)
  (insert "Mapped ;\n        ")
  )

(defun declare-namespace-class-swap ()
  (interactive)
  (let ((resource-name (thing-at-point 'word 'no-properties)))
	(search-forward "olive:Un")
	(backward-char 2)
	(delete-char 8)
	(insert "Mapped ;\n        owl:equivalentClass olive:" resource-name " ;\n")
	(delete-char 1)
	(backward-char 2)
	))

(defun rule-map ()
  (interactive)
  (search-forward "olive:Un")
  (backward-char 2)
  (delete-char 8)
  (insert "Mapped ; .\n ")
  (create-stardog-rule)
  )

(defun path-map ()
  (interactive)
  (search-forward "olive:Un")
  (backward-char 2)
  (delete-char 8)
  (insert "Mapped ; .\n ")
  (create-property-path-nil)
  (move-beginning-of-line)
  )

(defun change-name (oldname newname)
  (interactive "sOld Name: \nsNew Name: ")
  (how-many oldname)
  (beginning-of-buffer) ;; go to the start of the buffer
  (while (re-search-forward oldname nil t) ;; replace every oldnname with newname
    (replace-match newname nil nil))
  (re-search-backward (concat "^" newname))
  (re-search-forward (concat "\\.$"))
  (previous-line)
  (move-end-of-line nil)
  (insert "\n\tolive:formerName \"" oldname "\"^^xsd:string;")
  (re-search-backward (concat "^" newname))
  )
  
(global-set-key (kbd "C-c d") 'declare-term-mapped)
(global-set-key (kbd "C-c D") 'declare-namespace-class-swap)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(csv-mode)))
