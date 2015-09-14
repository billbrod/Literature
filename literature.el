;;To use this, make sure the paths in literature-update, literature-add, and the helm-bibtex configurations are all correct

(defun literature-update ()
  "Updates the master literature bib and org files"
  (interactive)
  (shell-command "python ~/Documents/Paper-annot/Literature/lit_update.py"))
(global-set-key (kbd "s-u") 'literature-update)
(add-hook 'kill-emacs-hook 'literature-update)

(add-hook 'emacs-startup-hook (lambda ()
                                (let ((default-directory (getenv "HOME")))
                                  (command-execute 'eshell)
                                  (bury-buffer))))

(defun eshell/literature-add (&rest args)
  "Adds a new item to the library"
  (let ((cmd (concat "python ~/Documents/Paper-annot/Literature/lit_add.py " (pop args))))
    (shell-command cmd)))

;;Helm-bibtex configuration options
;;Location of your master bib file (paper_dir from lit_add.py + literature.bib)
(setq helm-bibtex-bibliography '("~/Documents/Paper-annot/literature.bib"))
;;Should be the same as paper_dir from lit_add.py
(setq helm-bibtex-library-path '("~/Documents/Paper-annot/"))
;;Should be the same as paper_dir from lit_add.py
(setq helm-bibtex-notes-path "~/Documents/Paper-annot/")
(setq helm-bibtex-notes-extension ".org")

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
