;;; literature.el -- helper functions to manage bibliography and notes,

;; Author: Bill Broderick <billbrod@gmail.com>
;; URL: https://github.com/billbrod/Literature
;; Package-Requires: ((bibtex-utils) (org-ref) (helm-bibtex) (biblio))
;; This file is not currently part of GNU Emacs

;;; Commentary:
;; This code is based on org-ref, helm-bibtex, biblio, and
;; bibtex-utils. Some of it is simply convenience wrappers around
;; pre-existing functions from those packages, others are more
;; substantive.
;;
;; Read the README for more details.

;;; Code:
(require 'org-ref)
(require 'doi-utils)
(require 'org-ref-pdf)
(require 'org-ref-latex)
(require 'org-ref-bibtex)
(require 'org-ref-pubmed)
(require 'org-ref-arxiv)
(require 'org-ref-sci-id)
(require 'org-ref-isbn)
(require 'bibtex-utils)
(require 'x2bib)
(require 'biblio)


;;; Variables:
(defcustom literature-paper-directory
  nil
  "Parent directory for all your papers. This directory will
  contain the master .bib and .org files, and each entry will
  have a separate directory, containing the individual .bib and
  .org files, as well as the pdf (if present) and any other
  files. MUST end with a slash."
  :type 'directory
  :group 'literature)

(defcustom literature-master-bib
  nil
  "Name of your master bibliography file, which all of the
individual .bib files are combined in. This is probably the same
file as your org-ref-default-bibliography. However, this should
just be the name (eg, literature.bib), not the full path; this
will be placed within your literature-paper-directory."
  :type 'file
  :group 'literature
  )

(defcustom literature-master-org
  nil
  "Name of your master notes file, which all of the individual
.org files are combined in. This is probably the same as your
org-ref-bibliography-notes. However, this should just be the
name (eg, literature.org), not the full path; this will be placed
within your literature-paper-directory."
  :type 'file
  :group 'literature
  )

;;; Functions:

(defun literature-add-file (file)
  "Add a file to your bibliography. Accepts pdfs (in which case
we attempt to automatically get the bibtex by extracting the doi)
and .bib files. Any other file type will cause an exception"
  (interactive)
  (cond ((equalp (file-name-extension file) "pdf")
	 (literature-add-pdf file))
	((equalp (file-name-extension file) "bib")
	 (literature-add-bib file))
	(t (display-warning :warning "Only files with a pdf or bib extension are accepted"))
	)
  )

(defun literature-add-pdf (file)
  "Add a pdf file to your bibliography. For this to work, the pdf
file needs to contain a doi that can be reliably extracted (if
there are multiple dois found in the document, we use the one
that shows up the most) and the doi needs to be usable for
grabbing a bibtex entry. In case either of these are false, this
function will throw an error and you'll need to download the .bib
yourself; you should then call literature-add-bib to add it to
your bibliography."
  ;; to get the moving around within the bibtex buffer working,
  ;; biblio-synchronous needs to be true
  (let ((doi (literature-extract-doi-from-pdf file)) (biblio-synchronous t))
    (when (not doi)
      (error (format "Unable to retrieve doi from %s, download .bib yourself" file)))
    (with-temp-buffer
     (bibtex-mode)
     ;; there are two places where this could fail. The first is in
     ;; grabbing the bib for the doi (less likely) and the second is
     ;; in running org-ref-clean-bibtex-entry on the resulting
     ;; entry. An error there may be the result of a problem in the
     ;; first case, but it might not show up until then. These
     ;; condition-cases catches any error that comes out (several
     ;; different ones are possible) and raises a more descriptive
     ;; error message.
     (condition-case nil
	 (doi-insert-bibtex doi)
       ('error
	(error (format "Unable to download .bib for file %s, doi %s. download .bib yourself" file doi))))
     (beginning-of-buffer)
     ;; We next try to format (using org-ref-clean-bibtex-entry) the
     ;; downloaded bibtex entry. If we can't, we throw an error.
     (literature-try-to-format-bib file)
     ;; if the key is already in our bibliography, we throw an error
     (let ((key (literature-get-key-from-bibtex)))
       (literature-check-key key file)
       )
     ;; or if the doi is already in the bibliography
     (literature-check-doi doi file)
     ;; and now we set up the directory, passing it the pdf file path
     ;; and the contents of the bib file. Need to use
     ;; buffer-substring-no-properties so we don't grab the properties
     ;; of the buffer, which aren't helpful.
     (let ((bib-contents (buffer-substring-no-properties (point-min) (point-max)))
	   (key (literature-get-key-from-bibtex)))
       ;;and call the stuff to set it up. This will create the new
       ;;files, move over the pdf, add the entries to the master bib
       ;;and org files, and stage, commit, and push the new changes to
       ;;git.
       (literature-setup-files key bib-contents file)
       (message (format "Key %s added, check to make sure it looks like you want" key))
       )
     )
    ))


(defun literature-check-key (key &optional file throwname)
  "Check whether the key is found in your bibliographies and, if
so, notify the user (either via error or message and throw). One
of the two optional args is required: if file is given, then an
error will be raised (as used in literature-add-pdf), else
throwname must be given and this will print a message instead,
then throw to the named catch."
  (when (cdr (org-ref-get-bibtex-key-and-file key))
    (if file
	(error (format "Key %s (for pdf %s) already in your bibliography" key file))
      (message (format "Key %s already in your bibliography, skipping" key))
      (throw throwname nil))
    )
  )

(defun literature-check-doi (doi &optional file throwname)
  "Based on the end of doi-utils-add-bibtex-entry-from-doi, we
check whether the doi is already in one of the bibliography files
and, if so, notify the user (either via error or message and
throw). One of the two optional args is required: if file is
given, then an error will be raised (as used in
literature-add-pdf), else throwname must be given and this will
print a message instead, then throw to the named catch."
  (cl-loop for bibfile in org-ref-bibliography-files do
	   (with-current-buffer
	       (find-file-noselect bibfile)
	     (goto-char (point-min))
	     (when (search-forward doi nil t)
	       (if file
		   (error (format "DOI %s (for pdf %s) already in your bibliography" doi file))
		 (message (format "DOI %s already in your bibliography" doi))
		 (throw throwname nil)
		 )
	       ))
	   )
  )

(defun literature-try-to-format-bib (&optional file entry throwname)
  "Attempt to run org-ref-clean-bibtex-entry on the entry at
point. If this fails, notify the user (either via error or
message and throw). Of the optional args, either file is required
or entry and throwname are required. If file is given, then an
error will be raised (used in literature-add-pdf), else entry and
throwname will be used to print an informative message and then
throw to the named catch."
  (condition-case nil
      (org-ref-clean-bibtex-entry)
    ('error
     (if file
	 (error (format "Unable to properly format bibtex entry for file %s, download .bib yourself" file))
       (progn
	 (message (format "Unable to properly format entry for %s, skipping"
			  (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "title" entry))))
	 (throw throwname nil)
	 ))
     )
    )
  )

(defun literature-get-key-from-bibtex ()
  "attempt to get the key from the bibtex entry currently under
  point"
  (interactive)
  (bibtex-beginning-of-entry)
  ;; based on code from http://stackoverflow.com/a/15974319/4659293
  (when (re-search-forward "@[A-Za-z]+{\\(.*\\),")
    (match-string 1))
  )


(defun literature-extract-doi-from-pdf (pdf)
  "Try to extract a doi from a PDF file.
There may be more than one doi in the file. If so, we return the
one that shows up the most. Thus, if there's a tie, we return all
the ones that tie.

If there is a trailing . we chomp it off. Returns a list of doi
strings, or nil.

Based on org-ref-extract-doi-from-pdf, the only change is that I
want the doi that shows up the most instead of each that shows up.
"
  (with-temp-buffer
    (insert (shell-command-to-string (format "%s %s -"
					     pdftotext-executable
					     (shell-quote-argument (dnd-unescape-uri pdf)))))
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward org-ref-pdf-doi-regex nil t)
	;; I don't know how to avoid a trailing . on some dois with the
	;; expression above, so if it is there, I chomp it off here.
	(let ((doi (match-string 1)))
	  (when (s-ends-with? "." doi)
	    (setq doi (substring doi 0 (- (length doi) 1))))
	  (push doi matches)))
      ;; this bit is based on code from
      ;; http://stackoverflow.com/questions/6050033/elegant-way-to-count-items
      (let (result)
	(dolist (elt matches result)
	  (let ((sofar (assoc elt result)))
	    (if sofar
		(setcdr sofar (1+ (cdr sofar)))
	      (push (cons elt 1) result))))
	(car (car (sort result (lambda (a b) (> (cdr a) (cdr b))))))))))

(defun literature-add-bib (file)
  "Add all bibtex entries in a .bib file to your
bibliography. This function loops through the entries, attempts
to call org-ref-clean-bibtex-entry on each one, and then checks
if an entry with that key or doi already exists in your
bibliography files. If either of those are true or if
org-ref-clean-bibtex-entry raises an error, the entry is
skipped (with a message). Else, we add the entry to the
bibliography, either with or without an accompanying pdf file (if
the file field is present)."
  (with-temp-buffer
    (insert-file-contents file)
    (bibtex-mode)
    (beginning-of-buffer)
    (while (not (eobp))
      ;; This catch allows us to ignore the rest of the processing
      ;; steps for a bibtex entry in one of three conditions: if
      ;; org-ref-clean-bibtex-entry returns an error; if 
      (catch 'format-problem-or-already-in-bib
	(let ((entry (bibtex-parse-entry t)))
	  (literature-try-to-format-bib nil entry 'format-problem-or-already-in-bib)
	  (let ((key (literature-get-key-from-bibtex))
		(filepath (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "file" entry)))
		(doi (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "doi" entry))))
	    ;; Check if the key is already in your bibliography and
	    ;; throw if so.
	    (literature-check-key key nil 'format-problem-or-already-in-bib)
	    ;; And if the doi is already in the bibliography
	    (literature-check-doi key nil 'format-problem-or-already-in-bib)
	    ;; this let* statement is copied from bibtex-kill-entry,
	    ;; since I similarly want to grab the beginning and end of
	    ;; the entry.
	    (let* ((case-fold-search t)
		   (beg (bibtex-beginning-of-entry))
		   (end (progn (bibtex-end-of-entry)
			       (if (re-search-forward
				    bibtex-any-entry-maybe-empty-head nil 'move)
				   (goto-char (match-beginning 0)))
			       (point))))
	      ;; in the process of getting end, we move to the
	      ;; beginning of the next entry. This interferes with how
	      ;; we move through the file, so we make sure to head
	      ;; back to the previous entry before doing anything
	      ;; else.
	      (org-ref-bibtex-previous-entry)
	      ;; if there's no file field, reftex-get-bib-field will
	      ;; return an empty string, not nil, so this is required.
	      (if (string= "" filepath)
		  (progn
		    (literature-setup-files key (buffer-substring-no-properties beg end))
		    (message (format "Key %s added WITHOUT file, make sure it looks like you want" key))
		    )
		(literature-setup-files key (buffer-substring-no-properties beg end) filepath)
		(message (format "Key %s added with file, make sure it looks like you want" key))
	       ))
	    )
	  ))
      ;; If there's an actual error in the bib entry (and it's not
      ;; just formatted in a way org-ref doesn't like), this will
      ;; throw an error, so we catch it and make it clear what went
      ;; wrong.
      (condition-case nil
	  (bibtex-kill-entry)
	('error (error "Your bib file is formatted incorrectly, I can't move through it!")))
      (bibtex-beginning-of-entry)
      )
    )
  )

(defun literature-setup-files (key bib-contents &optional filepath)
  "Given the key, contents of the bib file, and the associated
  file (if present), this function creates the directory for
  this entry (directory's name will be key and it will be
  contained within literature-paper-directory), save a new .bib
  file there containing bib-contents, move the file if
  present, and create the .org notes file. We also update the
  file and notefile fields of the .bib"
  (let ((entry-dir (concat literature-paper-directory key "/")))
    (make-directory entry-dir)
    (with-current-buffer
	(find-file-noselect (concat entry-dir key ".bib"))
      (insert bib-contents)
      (bibtex-mode)
      (bibtex-beginning-of-entry)
      ;; Set the notefile and (if necessary) file fields to the
      ;; appropriate values.
      (bibtex-set-field "notefile" (concat key ".org"))
      ;; Add the file field if we have a file
      (when filepath
	(if (string= "pdf" (file-name-extension filepath))
	    (bibtex-set-field "file" (concat ":" key ".pdf:PDF"))
	  (bibtex-set-field "file" (concat ":" key "." (file-name-extension filepath) ":"))
	  ))
      (bibtex-beginning-of-entry)
      (save-buffer)
      ;; Create the note file
      (let ((entry (bibtex-parse-entry t)))
	(with-current-buffer
	    (find-file-noselect (concat entry-dir key ".org"))
	  (org-mode)
	  (insert "#+STARTUP: showall\n")
	  (org-insert-todo-heading t)
	  (insert (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "title" entry)))
	  (insert "\n\n** Keywords")
	  (insert "\n\n\n** Notes")
	  (insert "\n\n\n** Annotations")
	  (insert "\n\n\n** Links")
	  ;; Only do this if we have a file
	  (when filepath
	    (newline)
	    (indent-relative)
	    (if (string= "pdf" (file-name-extension filepath))
		(insert (concat "PDF: [[file:" key ".pdf]]"))
	      (insert (concat "File: [[file:" key "." (file-name-extension filepath) "]]"))
	      )
	    )
	  (cl-loop for elt in '(("Bibtex" . "bib") ("Notes" . "org")) do
		   (newline)
		   (indent-relative)
		   (insert (concat (car elt) ": [[file:" key "." (cdr elt) "]]"))
		   )
	  (outline-up-heading 2)
	  (org-set-property "ADDED" (format-time-string "[%Y-%m-%d]"))
	  (org-set-property "BIBTEX-KEY" key)
	  ;; the author field may contain extra newlines and
	  ;; whitespace characters, so if we do, we remove them.
	  (org-set-property "AUTHOR"
			    (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "author" entry)))
	  (org-set-property "YEAR" (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "year" entry)))
 	  (org-set-property "PUBLICATION" (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "journal" entry)))
	  (save-buffer)
	  ))
      )
    ;; If we have a file, move it in 
    (when filepath
      (rename-file filepath (concat entry-dir key "." (file-name-extension filepath))))
    ;; the number of files we add to the git repo depends on whether
    ;; we have a file or not.
    (if filepath
	(git-update-commit
	 (list (concat entry-dir key ".bib") (concat entry-dir key ".org") (concat entry-dir key "." (file-name-extension filepath)))
	 nil)
      (git-update-commit
       (list (concat entry-dir key ".bib") (concat entry-dir key ".org"))
       nil)
      )
    )
  (literature-add-to-master-bib key)
  (literature-add-to-master-org key)
  )

(defun literature-add-to-master-bib (key)
  "This file adds the new bibtex entry (corresponding to KEY,
  found at literature-paper-dir/key/key.bib) to the master
  bibliography file, specified by literature-master-bib. It DOES
  NOT double-check whether the key already exists first, since
  it's assumed that has been done before calling this."
  (save-window-excursion
    (let ((master-bib-path (concat literature-paper-directory literature-master-bib)))
      (set-file-modes master-bib-path #o666)
      (with-temp-file master-bib-path
	(insert-file-contents master-bib-path)
	;; This ugly chaining is so we can use the regexp only on the
	;; contents of the bib file we're adding without worrying
	;; about affecting the rest of the file. They make the links
	;; work correctly.
	(insert
	 (replace-regexp-in-string "notefile =\\(\\s-*\\){\\(.*\\)\\.\\(.*\\)" "notefile =\\1{\\2/\\2.\\3"
				   (replace-regexp-in-string "file =\\(\\s-*\\){:\\(.*\\)\\.\\(.*\\)" "file =\\1{:\\2/\\2.\\3"
							     (with-temp-buffer
							       (insert-file-contents (concat literature-paper-directory key "/" key ".bib"))
							       (buffer-string))))
	 )
	(insert "\n")
	(goto-char (point-min))
	(bibtex-sort-buffer)
	)
      (set-file-modes master-bib-path #o444)
      (git-update-commit (list master-bib-path) nil)
      )
    )
  )

(defun literature-add-to-master-org (key)
  "This file adds the new notes entry (corresponding to KEY,
  found at literature-paper-dir/key/key.org) to the master
  notes file, specified by literature-master-org. It DOES
  NOT double-check whether the entry already exists first, since
  it's assumed that has been done before calling this."
  (save-window-excursion
    (let ((master-org-path (concat literature-paper-directory literature-master-org)))
      (set-file-modes master-org-path #o666)
      (with-temp-file master-org-path
	(insert-file-contents master-org-path)
	;; This ugly chaining is so we can use the regexp only on the
	;; contents of the org file we're adding without worrying
	;; about affecting the rest of the file. They get rid of any
	;; startup options in the small file as well as make the links
	;; work correctly.
	(insert
	 (replace-regexp-in-string "#\\+STARTUP:.*" ""
				   (replace-regexp-in-string "file:\\(.*\\)\\.\\(.*\\)" "file:\\1/\\1.\\2"
							     (with-temp-buffer
							       (insert-file-contents (concat literature-paper-directory key "/" key ".org"))
							       (buffer-string))))
	 )
	(insert "\n\n")
	(goto-char (point-min))
	(org-sort-entries nil ?r nil nil "BIBTEX-KEY")
       )
      (set-file-modes master-org-path #o444)
      (git-update-commit (list master-org-path) nil)
      )
    )
  )


(defun git-update-commit (files rmflag)
  "stage FILES, make a new commit (noting they were added if
rmflag is nil and removed if rmflag is t) and push the change to
origin master."
  (save-window-excursion
    (cl-loop for f in files do
	     (find-file f)
	     (magit-stage-file f)
	     (kill-buffer)
	     )
    (find-file (car files))
    (if rmflag
	(magit-run-git-with-input "commit" "-m" (concat "Removes " (mapconcat 'identity files ", ")))
      (magit-run-git-with-input "commit" "-m" (mapconcat 'identity files ", "))
      )    
    (magit-run-git-with-input "push" "origin" "master")
    (kill-buffer))
  )

;;; update
;; 1. get updated time for master bib and master org, make them writeable.
;; 2. for every entry in the master bib:
;;    1. if it's directory isn't there, remove from master bib, master org
;;    2. if pdf exists and has been modified more recently than master org,
;;       update annotations in individual org
;;    3. if org has been modified more recently than master org, copy
;;       individual to master org
;;    4. if bib has been modified more recently than master bib, copy
;;       individual to master bib
;; 3. make master files read-only

;;; force_renew? may also just leave that as python. meh, probably shouldn't.

;;; force_renew? may also just leave that as python. meh, probably
;;; shouldn't. 

;;; Second function (super force renew?) that will call
;;; org-ref-clean-entry on each bibtex entry, changing folders and
;;; path as appropriate.


(defun test-bib-files ()
  "This is a function to test your bib files in case something's
   the matter with your library and helm-bibtex. I ran into an
   error where helm-bibtex would not display my library but also
   returned no errors. Evaluating bibtex-completion-candidates
   returned a parentheses unbalanced error, but I wasn't sure
   where it was. This loops through all bib files (may have to
   update the path) and tests each one. It will fail when there's
   a problem, so you can go and look at it specifically. For me,
   it was a non-standard parentheses (appeared too large) which
   was causing the issue."
  (let test-list (f-glob "~/Org-Docs/Papers/*/*.bib")
       (while test-list
	 (setq bibtex-completion-bibliography (car test-list))
	 (print (car test-list))
	 (print (bibtex-completion-candidates))
	 (setq test-list (cdr test-list)))))


;;; CUSTOMIZATION OF EXISTING PACKAGES
;; This section contains some code to make org-ref and helm-bibtex
;; work well with the structure of my bibliography.

;; We need this because org-ref-get-pdf-filename-function expects one
;; pdf, but bibtex-completion-find-pdf returns a list containing one
;; filename. So, using car, we grab the first item in that list and
;; return it.
(defun my/find-one-pdf (key)
  (car (bibtex-completion-find-pdf key))
  )

(setq org-ref-get-pdf-filename-function 'my/find-one-pdf)

;; Over-write the bibtex-completion-edit-notes function, because we
;; format the entries as KEY/KEY.org in bibtex-completion-notes-path,
;; which there's currently no support for.
(eval-after-load 'helm-bibtex		
  '(defun bibtex-completion-edit-notes (key)
     "Open the notes associated with the entry key (using `find-file'). Will look for \"KEY/KEY/org\" in `bibtex-completion-notes-path'."
     (if (f-directory? bibtex-completion-notes-path)
	 ;; One notes file per publication: just open the file.
	 (let ((path (f-join bibtex-completion-notes-path
			     (s-concat key "/" key bibtex-completion-notes-extension))))
	   (find-file path)
	   (unless (f-exists? path)
	     (insert (s-format bibtex-completion-notes-template-multiple-files
			       'bibtex-completion-apa-get-value
			       (bibtex-completion-get-entry key)))))
       (unless (and buffer-file-name
		    (f-same? bibtex-completion-notes-path buffer-file-name))
	 (find-file-other-window bibtex-completion-notes-path))
       (widen)
       (show-all)
       (goto-char (point-min))
       (if (re-search-forward (format bibtex-completion-notes-key-pattern key) nil t)
                                        ; Existing entry found:
	   (when (eq major-mode 'org-mode)
	     (org-narrow-to-subtree)
	     (re-search-backward "^\*+ " nil t)
	     (org-cycle-hide-drawers nil)
	     (bibtex-completion-notes-mode 1))
                                        ; Create a new entry:
	 (let ((entry (bibtex-completion-get-entry key)))
	   (goto-char (point-max))
	   (insert (s-format bibtex-completion-notes-template-one-file
			     'bibtex-completion-apa-get-value
			     entry)))
	 (when (eq major-mode 'org-mode)
	   (org-narrow-to-subtree)
	   (re-search-backward "^\*+ " nil t)
	   (org-cycle-hide-drawers nil)
	   (goto-char (point-max))
	   (bibtex-completion-notes-mode 1))))))



;;Custom function to open the individual notes file
(add-to-list 'org-ref-helm-user-candidates
	     '("Open individual notes file" . (lambda ()
						(bibtex-completion-edit-notes (car (org-ref-get-bibtex-key-and-file))))))


(provide 'literature)

;;; literature.el ends here

