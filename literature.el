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
(defcustom literature-paper-superdirectory
  nil
  "Parent directory for all your papers. This directory will
  contain the master .bib file, and each entry will have a
  separate directory, containing the individual .bib and .org
  files, as well as the pdf (if present) and any other files.
  MUST end with a slash."
  :type 'directory
  :group 'literature)

(defcustom literature-paper-directory-name "papers/"
  "Name of the sub-directory that contains files for the actual
  papers (pdfs ,etc). MUST end with a slash."
  :type 'string
  :group 'literature)

(defcustom literature-notes-directory-name "notes/"
  "Name of the sub-directory that contains individual notes
  files. MUST end with a slash."
  :type 'string
  :group 'literature)

(defcustom literature-bibtex-directory-name "bibs/"
  "Name of the sub-directory that contains individual bibtex
  entries. MUST end with a slash."
  :type 'string
  :group 'literature)

(defcustom literature-master-bib
  nil
  "Name of your master bibliography file, which all of the
individual .bib files are combined in. This is probably the same
file as your org-ref-default-bibliography. However, this should
just be the name (eg, literature.bib), not the full path; this
will be placed within your literature-paper-superdirectory."
  :type 'file
  :group 'literature
  )


(defcustom literature-shell-and " && "
  "How to join commands together in the shell. For fish shell,
  you want to customise this to: \" ; and \" instead of the default."
  :tag "Join shell commands"
  :group 'literature
  :type 'string)

;;; Functions:

;;;###autoload
(defun literature-add-file ()
  "Add a file to your bibliography. Accepts pdfs (in which case
we attempt to automatically get the bibtex by extracting the doi)
and .bib files. Any other file type will cause an exception"
  (interactive)
  (let ((file buffer-file-name))
    (cond ((equalp (file-name-extension file) "bib")
           (literature-add-bib file))
          (t (display-warning :warning "Only files with a bib extension are accepted"))
          ))
  )

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun literature-get-key-from-bibtex ()
  "attempt to get the key from the bibtex entry currently under
  point"
  (interactive)
  (bibtex-beginning-of-entry)
  ;; based on code from http://stackoverflow.com/a/15974319/4659293
  (when (re-search-forward "@[A-Za-z]+{\\(.*\\),")
    (match-string 1))
  )

;;;###autoload
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

;;;###autoload
(defun literature-setup-files (key bib-contents &optional filepath)
  "Given the key, contents of the bib file, and the associated
  file (if present), this function creates the directory for
  this entry (directory's name will be key and it will be
  contained within literature-paper-superdirectory), save a new .bib
  file there containing bib-contents, move the file if
  present, and create the .org notes file. We also update the
  file and notefile fields of the .bib"
  (with-current-buffer
      (find-file-noselect (concat literature-paper-superdirectory literature-bibtex-directory-name key ".bib"))
    (insert bib-contents)
    (bibtex-mode)
    (bibtex-beginning-of-entry)
    ;; Set the notefile and (if necessary) file fields to the
    ;; appropriate values.
    (bibtex-set-field "notefile" (concat "../notes/" key ".org"))
    ;; Add the file field if we have a file
    (when filepath
      (if (string= "pdf" (file-name-extension filepath))
          (bibtex-set-field "file" (concat ":../papers/" key ".pdf:PDF"))
        (bibtex-set-field "file" (concat ":../papers/" key "." (file-name-extension filepath) ":"))
        ))
    (bibtex-beginning-of-entry)
    (save-buffer)
    ;; Create the note file
    (let ((entry (bibtex-parse-entry t)))
      (with-current-buffer
          (find-file-noselect (concat literature-paper-superdirectory literature-notes-directory-name key ".org"))
        (org-mode)
        (insert "#+STARTUP: showall\n")
        (org-insert-todo-heading t)
        (insert (replace-regexp-in-string "\n\\s-*" " " (reftex-get-bib-field "title" entry)))
        (insert "\n\n** Keywords")
        (insert "\n\n\n** Notes")
        ;; initialize properties for org-noter
        (org-set-property "NOTER_PAGE" "1")
        (end-of-buffer)
        (insert "\n\n\n** Annotations")
        (insert "\n\n\n** Links")
        ;; Only do this if we have a file
        (when filepath
          (newline)
          (indent-relative)
          (if (string= "pdf" (file-name-extension filepath))
              (insert (concat "PDF: [[file:../papers/" key ".pdf]]"))
            (insert (concat "File: [[file:../papers/" key "." (file-name-extension filepath) "]]"))
            )
          )
        (cl-loop for elt in '(("Bibtex" "../bibs" "bib") ("Notes" "." "org")) do
                 (newline)
                 (indent-relative)
                 (insert (concat (pop elt) ": [[file:" (pop elt) "/" key "." (pop elt) "]]"))
                 )
        (outline-up-heading 2)
        (org-set-property "ADDED" (format-time-string "[%Y-%m-%d]"))
        (org-set-property "BIBTEX-KEY" key)
        (org-set-property "NOTER_DOCUMENT" (concat "../papers/" key ".pdf"))
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
    (rename-file filepath (concat literature-paper-superdirectory literature-paper-directory-name key "." (file-name-extension filepath))))
  ;; We don't commit and push the change to the bib files in the add-to-master
  ;; functions because the git step is the step that takes the longest. We
  ;; want to do all of them at once.
  (literature-add-to-master-bib key)
  ;; add files to git repo
  (git-update-commit
   (list (concat literature-paper-superdirectory literature-bibtex-directory-name key ".bib")
         (concat literature-paper-superdirectory literature-notes-directory-name key ".org")
         (concat literature-paper-superdirectory literature-master-bib))
   nil)
  (kill-buffer (concat key ".org"))
  (kill-buffer (concat key ".bib"))
  (kill-buffer literature-master-bib)

  )

;;;###autoload
(defun literature-add-to-master-bib (key)
  "This function adds the new bibtex entry (corresponding to KEY,
  found at literature-paper-dir/key/key.bib) to the master
  bibliography file, specified by literature-master-bib. It DOES
  NOT double-check whether the key already exists first, since
  it's assumed that has been done before calling this."
  (let ((master-bib-path (concat literature-paper-superdirectory literature-master-bib)))
    (set-file-modes master-bib-path #o666)
    (with-temp-file master-bib-path
      (insert-file-contents master-bib-path)
      ;; This ugly chaining is so we can use the regexp only on the
      ;; contents of the bib file we're adding without worrying
      ;; about affecting the rest of the file. They make the links
      ;; work correctly.
      (insert
       (replace-regexp-in-string (concat "notefile\\(\\s-*\\)=\\(\\s-*\\){../" literature-notes-directory-name "\\(.*\\)\\.\\(.*\\)") (concat "notefile\\1=\\2{" literature-notes-directory-name "\\3.\\4")
                                 (replace-regexp-in-string (concat "file\\(\\s-*\\)=\\(\\s-*\\){:../" literature-paper-directory-name "\\(.*\\)\\.\\(.*\\)") (concat "file\1=\\2{:" literature-paper-directory-name "\\3/\\3.\\4")
                                                           (with-temp-buffer
                                                             (insert-file-contents (concat literature-paper-superdirectory literature-bibtex-directory-name key ".bib"))
                                                             (buffer-string))))
       )
      (insert "\n")
      (goto-char (point-min))
      (bibtex-sort-buffer)
      )
    (set-file-modes master-bib-path #o444)
    )
  
  )

;;;###autoload
(defun git-update-commit (files rmflag)
  "adds or removes FILES (based on rmflag), make a new
commit (noting they were added if rmflag is nil and removed if
rmflag is t) and push the change to origin master."
  (when files
    ;; if files is empty, we don't do anything.
    (with-temp-buffer
      ;; We build up one large command to pass to the shell
      (async-shell-command
       ;; We first must cd to the directory containing this file so our
       ;; git changes happen in the right repo
       (concat "cd " (shell-quote-argument (file-name-directory (car files)))
               ;; We then remove or add all the files. mapconcat will
               ;; place "'literature-shell-and  git rm/add " between each entry of files,
               ;; but to get it at the beginning I think I need to lead
               ;; with one.
               (if rmflag
                   (concat literature-shell-and "git rm "
                           (mapconcat 'shell-quote-argument files
                                      (concat literature-shell-and "git rm ")))
                 (concat literature-shell-and "git add "
                         (mapconcat 'shell-quote-argument files
                                    (concat literature-shell-and "git add "))))
               ;; We now create the commit message in a similar way,
               ;; adding quotes so the spaces don't confuse git.
               literature-shell-and "git commit -m \""
               (when rmflag
                 "Removes ")
               (shell-quote-argument (mapconcat 'identity files ", "))
               ;; And finally push to origin master
               "\"" literature-shell-and "git push origin master"))))
  )

;;;###autoload
(defun literature-update ()
  "This function, which should be called semi-regularly (or added
to your kill-emacs-hook), will update all of your files to keep
them in sync. It goes through every key in your master-bib file and:

1. if the corresponding entry isn't there, remove key from master
   bib

2. if a pdf has been modified more recently than the
   corresponding org, extract the annotations.

3. if bib has been modified more recently than master bib, copy
   individual to master bib

If your master bib file are out-of-date and you can't seem to fix
them, use literature-force-renew.
"
  (interactive)
  ;; We have to grab the time here instead of using
  ;; file-newer-than-file-p each time because we will overwrite master
  ;; bib as we go through this loop.
  (let* ((master-bib-path (concat literature-paper-superdirectory literature-master-bib))
         (master-bib-time (nth 6 (file-attributes master-bib-path)))
         ;; we're building up two lists of files to pass to the
         ;; git-update-commit function: one for those to add to the
         ;; repo (that have been changed) and one for those to remove
         ;; (that have been deleted).
         (added-files '())
         (removed-files '()))
    (with-temp-buffer
      (insert-file-contents master-bib-path)
      (goto-char (point-min))
      (bibtex-mode)
      (bibtex-set-dialect)
      (while (not (eobp))
        (let* ((entry (bibtex-parse-entry t))
               (key (literature-get-key-from-bibtex)))
          (message key)
          ;; when the directory for a given key doesn't exists, we
          ;; remove it from the master bib files
          (unless (file-exists-p (concat literature-paper-superdirectory literature-notes-directory-name key ".org"))
            (message (format "Key %s has been removed" key))
            (literature-remove-from-master-bib key)
            (cl-pushnew master-bib-path added-files :test #'equal)
            (cl-pushnew (concat literature-paper-superdirectory literature-bibtex-directory-name key ".bib") removed-files :test #'equal)
            (cl-pushnew (concat literature-paper-superdirectory literature-notes-directory-name key ".org") removed-files :test #'equal)
            (cl-pushnew (concat literature-paper-superdirectory literature-paper-directory-name key ".pdf") removed-files :test #'equal))
          (when (and (file-exists-p (concat literature-paper-superdirectory literature-paper-directory-name key ".pdf"))
                     (time-less-p (nth 6 (file-attributes (concat literature-paper-superdirectory literature-notes-directory-name key ".org")))
                                  (nth 6 (file-attributes (concat literature-paper-superdirectory literature-paper-directory-name key ".pdf")))))
            (message (format "PDF %s updated, extracting annotations" key))
            (literature-add-annotations-to-notefile key)
            (cl-pushnew (concat literature-paper-superdirectory literature-notes-directory-name key ".org") added-files :test #'equal))
          (when (time-less-p master-bib-time
                             (nth 6 (file-attributes (concat literature-paper-superdirectory literature-bibtex-directory-name key ".bib"))))
            (message (format "Bib %s updated, copying to master bib" key))
            (literature-remove-from-master-bib key)
            (literature-add-to-master-bib key)
            (cl-pushnew master-bib-path added-files :test #'equal))
          )
        ;; Similar to literature-add-bib, we use kill-entry and
        ;; beginning-of-entry to move through the bib flie (here we've
        ;; inserted its comments into a temp-buffer, so none of these
        ;; actual changes will be made.
        (bibtex-kill-entry)
        (bibtex-beginning-of-entry)
        )
      (git-update-commit added-files nil)
      (git-update-commit removed-files t)
      )
    )
  (message "Update finished")
  )

;;;###autoload
(defun literature-add-annotations-to-notefile (key)
  "Take the annotations from the pdf associated with this key and
  add them to those for .org file associated with this key. Any
  already existing annotations will be deleted"
  (let* ((notefile (concat literature-paper-superdirectory literature-notes-directory-name key ".org"))
         (pdffile (concat literature-paper-superdirectory literature-paper-directory-name key ".pdf"))
         (annotations (literature-get-annotations pdffile)))
    (with-temp-file notefile
      (org-mode)
      (insert-file-contents notefile)
      (goto-char (point-min))
      (search-forward "** Annotations")
      (org-cut-subtree)
      (insert "** Annotations\n\n")
      (cl-loop for elt in annotations do
               (insert (concat "\"" (cdr (assoc 'contents elt)) "\""))
               (insert (concat " (page " (number-to-string (cdr (assoc 'page elt))) ")") )
               (insert "\n\n")
               )
      (indent-region (point-min) (point-max))
      ;; We don't want to call fill on the links section, so we do it
      ;; on everything up until then. I think this will not work on
      ;; any line that starts with a number, but I'm not sure.
      (goto-char (point-min))
      (fill-individual-paragraphs (search-forward "** Notes") (search-forward "** Links"))
      )
    )
  )

;;;###autoload
(defun literature-get-annotations (pdf)
  "Returns all the annotations in the given pdf as a list, and
each entry has two elements, the first is the page and the second
is the content of the annotation. "
  (cl-loop for elt in (pdf-info-getannots nil pdf)
           if (equal (cdr (assoc 'type elt)) 'text)
           collect (list (assoc 'page elt) (assoc 'contents elt))
           )
  )

;;;###autoload
(defun literature-remove-from-master-bib (key)
  "This finds and removes the entry corresponding to key from the
master bib file"
  (let ((master-bib-path (concat literature-paper-superdirectory literature-master-bib)))
    (set-file-modes master-bib-path #o666)
    (with-temp-file master-bib-path
      (insert-file-contents master-bib-path)
      (if (bibtex-search-entry key)
          (bibtex-kill-entry)
        (message (format "Key %s removed from master bib" key))
        (message (format "Key %s not found in master bib, so not removed" key))))
    (set-file-modes master-bib-path #o444)
    )
  )

;;;###autoload
(defun literature-force-renew ()
  "This function deletes your current master bib files and
recreates them. For master bib, it looks for every file that
matches literature-paper-superdirectory/*/*.bib. It copies all of the
matching files into their respective matching file (replacing
their links as necessary). Afterwards, they're sorted, and master
bib are staged and committed."
  (interactive)
  (let ((master-bib-path (concat literature-paper-superdirectory literature-master-bib)))
    (set-file-modes master-bib-path #o666)
    (with-temp-file master-bib-path
      (cl-loop for bibfile in (f-glob (concat literature-bibtex-directory-name "*.bib") literature-paper-superdirectory) do
               ;; This ugly chaining is so we can use the regexp only on the
               ;; contents of the bib file we're adding without worrying
               ;; about affecting the rest of the file. They make the links
               ;; work correctly.
               (insert
                (replace-regexp-in-string (concat "notefile\\(\\s-*\\)=\\(\\s-*\\){../" literature-notes-directory-name "\\(.*\\)\\.\\(.*\\)") (concat "notefile\\1=\\2{" literature-notes-directory-name "\\3.\\4")
                                          (replace-regexp-in-string (concat "file\\(\\s-*\\)=\\(\\s-*\\){:../" literature-paper-directory-name "\\(.*\\)\\.\\(.*\\)") (concat "file\\1=\\2{:" literature-paper-directory-name "\\3/\\3.\\4")
                                                                    (with-temp-buffer
                                                                      (insert-file-contents bibfile)
                                                                      (buffer-string))))
                )
               (insert "\n")
               )
      (goto-char (point-min))
      (bibtex-sort-buffer)
      )
    (set-file-modes master-bib-path #o444)
    (git-update-commit (list master-bib-path) nil)
    )
  )


;;; Second function (super force renew?) that will call
;;; org-ref-clean-entry on each bibtex entry, changing folders and
;;; path as appropriate.

;;;###autoload
(defun literature-test-bib-files ()
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
  (let (test-list (f-glob "~/Org-Docs/Papers/bibs/*.bib"))
    (while test-list
      (setq bibtex-completion-bibliography (car test-list))
      (print (car test-list))
      (print (bibtex-completion-candidates))
      (setq test-list (cdr test-list)))
    ))

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

;;;###autoload
(defun literature-create-mini-bib ()
  "This function searches the current directory for all md and
  tex files for \cite{KEY} and creates a new file,
  bibliography/biblio-offline.bib (overwriting if already there),
  containing the keys found (copied over from your
  literature-master-bib).  This is to use with authorea/overleaf."
  (interactive)
  (with-temp-file "./bibliography/biblio-offline.bib"
    (let (keys)
      (cl-loop for check-file in (append (f-glob "./*md") (f-glob "./*tex")) do
               (when-let (matches (mapcar (lambda (x) (elt x 1)) (s-match-strings-all "\cite{\\(.*?\n*?\\)}"
                                                                                 (with-temp-buffer
                                                                                   (insert-file-contents check-file)
                                                                                   (replace-string "\n" " ")
                                                                                   (buffer-string)))))
                 ;; need to strip any trailing spaces in order to properly find bibtex key
                 (push (mapcar (lambda (x) (mapcar 'trim-string (split-string x ","))) matches) keys)))
      (bibtex-completion-insert-bibtex (delete-dups (eshell-flatten-list keys))))
    (bibtex-sort-buffer)
    (beginning-of-buffer)
    ;; remove any file or notefile lines
    (flush-lines "^ *\\(note\\)?file")))


;;; CUSTOMIZATION OF EXISTING PACKAGES
;; This section contains some code to make org-ref and helm-bibtex
;; work well with the structure of my bibliography.


(defun my/find-one-pdf (key)
  "We need this because org-ref-get-pdf-filename-function expects
   one pdf, but bibtex-completion-find-pdf returns a list
   containing one filename. So, using car, we grab the first item
   in that list and return it. If there is nothing, we return
   'not found', so it's not nil, because that runs into issues
   with org-ref-cite-candidates"
  (let ((pdf-file (car (bibtex-completion-find-pdf key))))
    (if pdf-file
        pdf-file
      "not found")
    )
  )

(setq org-ref-get-pdf-filename-function 'my/find-one-pdf)


(provide 'literature)

;;; literature.el ends here

