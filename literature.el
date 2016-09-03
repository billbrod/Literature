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


;;; I don't think this will matter for use, but important for testing:
;;; the functions do not expand ~ into /home/user/, so having that in
;;; the path will cause them to fail.

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

(setq literature-paper-directory "/home/billbrod/Org-Docs/Papers/")
;; (setq literature-master-bib "tmp.bib")
(setq literature-master-bib "literature.bib")
;; (setq literature-master-org "tmp.org")
(setq literature-master-org "literature.org")

;; tests
(literature-add-file "/home/billbrod/Downloads/PNAS-2016-Chadwick-1610686113.pdf")
(literature-add-file "/home/billbrod/Downloads/VerhagenWagenmakers2014.pdf")
(literature-add-file "/home/billbrod/Downloads/071076.full.pdf")

(defun literature-add-file (file)
  "Add a file to your bibliography. Accepts pdfs (in which case
we attempt to automatically get the bibtex by extracting the doi)
and .bib files. Any other file typ ewill cause an exception"
  (interactive)
  (cond ((equalp (file-name-extension file) "pdf")
	 (literature-add-pdf file))
	((equalp (file-name-extension file) "bib")
	 (literature-add-bib file))
	(t (display-warning :warning "Only files with a pdf or bib extension are accepted"))
	)
  )

;;; add_pdf:
;; 1. attempt to get doi from pdf
;; 2. if can't, throw an exception (to start, eventually, give user option to input)
;; 3. if can, download bibtex and check to see if it's already in litearture.bib
;; 4. call setup_with_file
;; 5. call git_update_commit
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
     (condition-case nil
	 (org-ref-clean-bibtex-entry)
       ('error
	(error (format "Unable to properly format bibtex entry for file %s, download .bib yourself" file))
	))
     ;; we don't want to add the file if the key we're using already exists
     (let ((key (literature-get-key-from-bibtex)))
       (literature-check-key key file)
       )
     ;; or if the doi does
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
       )
     )
    ))


(defun literature-check-key (key &optional file)
  "Check whether the key is found in your bibliographies and, if
so, raise an error"
  (when (cdr (org-ref-get-bibtex-key-and-file key))
    (if file
	(error (format "Key %s (for pdf %s) already in your bibliography" key file)))
    (error (format "Key %s already in your bibliography" key)))
  )

(defun literature-check-doi (doi &optional file)
  "Based on the end of doi-utils-add-bibtex-entry-from-doi, we
check whether the doi is already in one of the bibliography files
and, if so, raise an error."
  (cl-loop for bibfile in org-ref-bibliography-files do
	   (with-current-buffer
	       (find-file-noselect bibfile)
	     (goto-char (point-min))
	     (when (search-forward doi nil t)
	       (if file
		   (error (format "DOI %s (for pdf %s) already in your bibliography" doi file))
		 (error (format "DOI %s already in your bibliography" doi)))
	       ))
	   )
  )

(defun literature-get-key-from-bibtex ()
  "attempt to get the key from the bibtex entry currently under
  the point"
  (interactive)
  (bibtex-beginning-of-entry)
  ;; based on code from http://stackoverflow.com/a/15974319/4659293
  (when (re-search-forward "@[A-Za-z]+{\\(.*\\),")
    (match-string 1))
  )

(defun literature-get-citation-field (field key)
  "Get the FIELD of an entry with KEY.  Return FIELD as a
string. Entry with KEY must be in your bibliography.

Based on org-ref-get-citation-year"
  (let* ((results (org-ref-get-bibtex-key-and-file key))
         (bibfile (cdr results)))
    (with-temp-buffer
      (insert-file-contents bibfile)
      (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
      (bibtex-search-entry key nil 0)
      (prog1 (reftex-get-bib-field field (bibtex-parse-entry t))))))


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

;;; add_bib:
;; 1. load in bibtex and for each entry, check if key is already in literature.bib
;; 2. skip those already in
;; 3. check if each entry has a file field and that points to a file there
;; 4. for each that does, call setup_with_file
;; 5. for each that doesn't, call setup_no_file
;; 6. call git_update_commit
(defun literature-add-bib (file)
  ""
  (with-temp-buffer
    (insert-file-contents file)
    (beginning-of-buffer)
    ;; want to use while here to say as long as there's an entry. But
    ;; I also ideally will have some reference to the contents of the
    ;; entry, so I can alert the user. Maybe loop through the entries,
    ;; parsing them? that way I can refer to the title to point out
    ;; what's going on.
    (condition-case nil
	(org-ref-clean-bibtex-entry)
      ('error (message (format "Unable to properly format entry"))))
    ;; In this loop, after we clean the entry, we check if it's
    ;; already in our bibliographies. If yes, do nothing else. If no,
    ;; check if it has a file field and grab it if it does. Then call
    ;; setup-files, with or without the contents of the file field.
    )
  )

(defun literature-setup-files (key bib-contents &optional pdf)
  "Given the key, contents of the bib file, and the associated
  pdf file (if present), this function creates the directory for
  this entry (directory's name will be key and it will be
  contained within literature-paper-directory), save a new .bib
  file there containing bib-contents, move the pdf file if
  present, and create the .org notes file. We also update the
  file and notefile fields of the .bib"
  (let ((entry-dir (concat literature-paper-directory key "/")))
    (make-directory entry-dir)
    (with-current-buffer
	(find-file-noselect (concat entry-dir key ".bib"))
      (insert bib-contents)
      (bibtex-mode)
      (bibtex-beginning-of-entry)
      (bibtex-set-field "notefile" (concat key ".org"))
      ;; Add the file field if we have a pdf
      (when pdf
	(bibtex-set-field "file" (concat ":" key ".pdf:PDF")))
      (bibtex-beginning-of-entry)
      (save-buffer)
      (let ((entry (bibtex-parse-entry t)))
	(with-current-buffer
	    (find-file-noselect (concat entry-dir key ".org"))
	  (org-mode)
	  (insert "#+STARTUP: showall\n")
	  (org-insert-todo-heading t)
	  (insert (reftex-get-bib-field "title" entry))
	  (insert "\n\n** Keywords")
	  (insert "\n\n\n** Notes")
	  (insert "\n\n\n** Annotations")
	  (insert "\n\n\n** Links")
	  ;; Only do this if we have pdf
	  (when pdf
	    (newline)
	    (indent-relative)
	    (insert (concat "PDF: [[file:" key ".pdf]]")))
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
	  (org-set-property "YEAR" (reftex-get-bib-field "year" entry))
 	  (org-set-property "PUBLICATION" (reftex-get-bib-field "journal" entry))
	  (save-buffer)
	  ))
      )
    ;; If we have a pdf, move it in 
    (when pdf
      (rename-file pdf (concat entry-dir key ".pdf")))
    (if pdf
	(git-update-commit
	 '((concat entry-dir key ".bib") (concat entry-dir key ".org") (concat entry-dir key ".pdf"))
	 nil)
      (git-update-commit
       '((concat entry-dir key ".bib") (concat entry-dir key ".org"))
       nil)
      )
    )
  (literature-add-to-master-bib key)
  (literature-add-to-master-org key)
  )

;;; for changing links, I have regex to do so.

;;; master_bib_add
;; 1. make master bib writeable
;; 2. move new bib into master bib
;; 3. change file and notefile fields to key/key.ext from key.ext
;; 4. sort.
;; 5. make read-only
(defun literature-add-to-master-bib (key)
  "This file adds the new bibtex entry (corresponding to KEY,
  found at literature-paper-dir/key/key.bib) to the master
  bibliography file, specified by literature-master-bib. It DOES
  NOT double-check whether the key already exists first, since
  it's assumed that has been done before calling this."
  (save-window-excursion
    (let ((master-bib-path (concat literature-paper-directory literature-master-bib)))
     (set-file-modes master-bib-path #o666)
     (find-file (concat literature-paper-directory key "/" key ".bib"))
     ;; We take the contents of the bib file, replace the file and
     ;; notefile fields so they're correct
     (let* ((bib-contents (buffer-substring-no-properties (point-min) (point-max)))
	    (bib-contents
	     (replace-regexp-in-string "file =\\(\\s-*\\){:\\(.*\\)\\.\\(.*\\)" "file =\\1{:\\2/\\2.\\3" bib-contents))
	    (bib-contents
	     (replace-regexp-in-string "notefile =\\(\\s-*\\){\\(.*\\)\\.\\(.*\\)" "notefile =\\1{\\2/\\2.\\3" bib-contents)))
       (kill-buffer)
       (find-file master-bib-path)
       (goto-char (point-max))
       (insert bib-contents)
       (bibtex-sort-buffer)
       (save-buffer)
       (kill-buffer)
       )
     (set-file-modes master-bib-path #o444))
    )
  )

;;; master_org_add
;; 1. make master org writeable
;; 2. move new org into master org
;; 3. change links from key.ext to key/key.ext
;; 4. sort
;; 5. make read-only
(defun literature-add-to-master-org (key)
  "This file adds the new notes entry (corresponding to KEY,
  found at literature-paper-dir/key/key.org) to the master
  notes file, specified by literature-master-org. It DOES
  NOT double-check whether the entry already exists first, since
  it's assumed that has been done before calling this."
  (save-window-excursion
    (let ((master-org-path (concat literature-paper-directory literature-master-org)))
     (set-file-modes master-org-path #o666)
     (find-file (concat literature-paper-directory key "/" key ".org"))
     ;; We take the contents of the org file, replacing the various
     ;; links so they're correct. We also remove any STARTUP options,
     ;; because we don't need to copy those over.
     (let* ((org-contents (buffer-substring-no-properties (point-min) (point-max)))
	    (org-contents
	     (replace-regexp-in-string "file:\\(.*\\)\\.\\(.*\\)" "file:\\1/\\1.\\2" org-contents))
	    (org-contents
	     (replace-regexp-in-string "#\\+STARTUP:.*" "" org-contents)))
       (kill-buffer)
       (find-file master-org-path)
       (goto-char (point-max))
       (insert org-contents)
       (insert "\n\n")
       (goto-char (point-min))
       (org-sort-entries nil ?r nil nil "BIBTEX-KEY")
       (save-buffer)
       (kill-buffer)
       )
     (set-file-modes master-org-path #o444))
    )
  )

;;; git_update_commit
;; 1. takes two lists, one of files to add one of files to remove
;; 2. (we still just call magit-stage-file for both)
;; 3. stage files, commit (w/ message) and push
;;; ACTUALLY, I don't think this is necessary because I have
;;; git-autocommit set up. Nope, it's necessary for the new files.
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
      (magit-run-git-with-input "commit" "-m" (concat "Adds " (mapconcat 'identity files ", ")))
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

;;; literature.el ends here
