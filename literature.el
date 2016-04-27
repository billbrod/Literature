;;To use this, make sure the paths in literature-update, literature-add, and the helm-bibtex configurations are all correct

(defun literature-update ()
  "Updates the master literature bib and org files"
  (interactive)
  (shell-command "~/anaconda/bin/python ~/Documents/Literature/lit_update.py"))
(global-set-key (kbd "s-u") 'literature-update)
(add-hook 'kill-emacs-hook 'literature-update)

(add-hook 'emacs-startup-hook (lambda ()
                                (let ((default-directory (getenv "HOME")))
                                  (command-execute 'eshell)
                                  (bury-buffer))))

(defun eshell/literature-add (&rest args)
  "Adds a new item to the library"
  (let ((cmd (concat "~/anaconda/bin/python ~/Documents/Literature/lit_add.py " (pop args))))
    (shell-command cmd)))

;;Helm-bibtex configuration options
;;Location of your master bib file (paper_dir from lit_add.py + literature.bib)
(setq helm-bibtex-bibliography "~/Org-Docs/Papers/literature.bib")
;;Should be the same as paper_dir from lit_add.py
(setq helm-bibtex-library-path "~/Org-Docs/Papers/")
;;Should be the same as paper_dir from lit_add.py
(setq helm-bibtex-notes-path "~/Org-Docs/Papers/")
(setq helm-bibtex-notes-extension ".org")
(setq helm-bibtex-additional-search-fields '(journal))

(setq reftex-default-bibliography '("~/Org-Docs/Papers/literature.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Org-Docs/Papers/literature.org"
      org-ref-default-bibliography '("~/Org-Docs/Papers/literature.bib")
      org-ref-pdf-directory "~/Org-Docs/Papers/")

(global-set-key [f10] 'org-ref-open-bibtex-notes)
(global-set-key [f11] 'org-ref-open-bibtex-pdf)
(global-set-key [f12] 'org-ref-open-in-browser)

;; make sure you have dash, helm, helm-bibtex, ebib, s, f, hydra and key-chord
;; in your load-path
(require 'org-ref)
(require 'doi-utils)
(require 'bibtex-utils)
(setq org-ref-bibtex-hydra-key-binding (kbd "C-c j"))
(require 'org-ref-pubmed)
(require 'org-ref-arxiv)
(require 'org-ref-sci-id)
(require 'org-ref-isbn)
(require 'x2bib)

;;This is a function to test your bib files in case something's the
;;matter with your library and helm-bibtex. I ran into an error where
;;helm-bibtex would not display my library but also returned no
;;errors. Evaluating helm-bibtex-candidates returned a parentheses
;;unbalanced error, but I wasn't sure where it was. This loops through
;;all bib files (may have to update your path) and tests each one. It
;;will fail when there's a problem, so you can go and look at it
;;specifically. For me, it was a non-standard parentheses (appeared
;;too large) which was causing the issue.
(defun test-bib-files ()
  (setq test-list (f-glob "~/Org-Docs/Papers/*/*.bib"))
  (while test-list
    (setq helm-bibtex-bibliography (car test-list))
    (print (car test-list))
    (print (helm-bibtex-candidates))
    (setq test-list (cdr test-list))))

;;This uses the system default to open the pdf
(setq helm-bibtex-pdf-open-function 'helm-open-file-with-default-tool)

;Need to eval after load so our custom function is the one that
;helm-bibtex uses. These two shouldn't need to be edited at all
(eval-after-load 'helm-bibtex
  '(defun helm-bibtex-find-pdf (key)
      "Searches in all directories in `helm-bibtex-library-path' for
a PDF whose path is \"KEY/KEY.pdf\".  Returns the first matching PDF."
      (-first 'f-exists?
	      (--map (f-join it (s-concat key "/" key ".pdf"))
		     (-flatten (list helm-bibtex-library-path))))))


(eval-after-load 'helm-bibtex
  '(defun helm-bibtex-edit-notes (key)
      "Open the notes associated with the entry key (using `find-file'). Will look for \"KEY/KEY/org\" in `helm-bibtex-notes-path'."
      (if (f-directory? helm-bibtex-notes-path)
	  ;; One notes file per publication: just open the file.
	  (let ((path (f-join helm-bibtex-notes-path
			      (s-concat key "/" key helm-bibtex-notes-extension))))
	    (find-file path)
	    (unless (f-exists? path)
	      (insert (s-format helm-bibtex-notes-template
				'helm-bibtex-apa-get-value
				(helm-bibtex-get-entry key)))))
	;; One file for all notes: find the notes or create new section
	;; from the template:
	(find-file helm-bibtex-notes-path)
	(goto-char (point-min))
	(if (re-search-forward (format helm-bibtex-notes-key-pattern key) nil t)
	    (when (eq major-mode 'org-mode)
	      (hide-other)
	      (show-subtree)
	      (outline-previous-visible-heading 1)
	      (recenter-top-bottom 1))
	  (when (eq major-mode 'org-mode)
	    (hide-sublevels 1))
	  (insert (s-format helm-bibtex-notes-template
			    'helm-bibtex-apa-get-value
			    (helm-bibtex-get-entry key)))
	  (goto-char (- (point) 1))))))

(setq org-ref-get-pdf-filename-function 'helm-bibtex-find-pdf)

;;Custom function to open the individual notes file
(add-to-list 'org-ref-helm-user-candidates
	     '("Open individual notes file" . (lambda ()
						(helm-bibtex-edit-notes (car (org-ref-get-bibtex-key-and-file))))))

;;Custom version of the open bibtex notes file, since I call the field BIBTEX-KEY instead of Custom_ID
(eval-after-load 'org-ref
  '(defun org-ref-open-bibtex-notes ()
     "From a bibtex entry, open the master org file and finds the
notes. Modified function from org-ref.el"
     (interactive)

     (bibtex-beginning-of-entry)
     (let* ((cb (current-buffer))
	    (bibtex-expand-strings t)
	    (entry (cl-loop for (key . value) in (bibtex-parse-entry t)
			    collect (cons (downcase key) value)))
	    (key (reftex-get-bib-field "=key=" entry)))

       ;; save key to clipboard to make saving pdf later easier by pasting.
       (with-temp-buffer
	 (insert key)
	 (kill-ring-save (point-min) (point-max)))

       ;; now look for entry in the notes file
       (if  org-ref-bibliography-notes
	   (find-file-other-window org-ref-bibliography-notes)
	 (error "Org-ref-bib-bibliography-notes is not set to anything"))

       (goto-char (point-min))
       ;; put new entry in notes if we don't find it.
       (if (re-search-forward (format ":BIBTEX-KEY: %s$" key) nil 'end)
	   (funcall org-ref-open-notes-function)
	 	 
	 (message "Entry not found")))))
