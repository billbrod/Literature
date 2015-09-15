;;To use this, make sure the paths in literature-update, literature-add, and the helm-bibtex configurations are all correct

(defun literature-update ()
  "Updates the master literature bib and org files"
  (interactive)
  (shell-command "/home/bill/anaconda/bin/python ~/Documents/Literature/lit_update.py"))
(global-set-key (kbd "s-u") 'literature-update)
(add-hook 'kill-emacs-hook 'literature-update)

(add-hook 'emacs-startup-hook (lambda ()
                                (let ((default-directory (getenv "HOME")))
                                  (command-execute 'eshell)
                                  (bury-buffer))))

(defun eshell/literature-add (&rest args)
  "Adds a new item to the library"
  (let ((cmd (concat "/home/bill/anaconda/bin/python ~/Documents/Literature/lit_add.py " (pop args))))
    (shell-command cmd)))

;;Helm-bibtex configuration options
;;Location of your master bib file (paper_dir from lit_add.py + literature.bib)
(setq helm-bibtex-bibliography "~/Dropbox/Docs/Papers/literature.bib")
;;Should be the same as paper_dir from lit_add.py
(setq helm-bibtex-library-path "~/Dropbox/Docs/Papers/")
;;Should be the same as paper_dir from lit_add.py
(setq helm-bibtex-notes-path "~/Dropbox/Docs/Papers/")
(setq helm-bibtex-notes-extension ".org")
(setq helm-bibtex-additional-search-fields '(journal))

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
  (setq test-list (f-glob "~/Dropbox/Docs/Papers/*/*.bib"))
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
