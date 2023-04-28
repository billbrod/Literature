;;To use this, make sure the paths in literature-update, literature-add, and the helm-bibtex configurations are all correct

(add-to-list 'load-path "~/Documents/Literature")
(require 'literature)

(global-set-key (kbd "s-u") 'literature-update)
;; (add-hook 'kill-emacs-hook 'literature-update)
;; (remove-hook 'kill-emacs-hook 'literature-update)

(global-set-key (kbd "s-a") 'literature-add-file)

(eval-after-load 'markdown-mode
  '(progn
     (define-key markdown-mode-map (kbd "s-b") 'literature-create-mini-bib)))
(eval-after-load 'tex-mode
  '(progn
     (define-key tex-mode-map (kbd "s-b") 'literature-create-mini-bib)))

;;Helm-bibtex configuration options
;;Location of your master bib file (paper_dir from lit_add.py + literature.bib)
(setq bibtex-completion-bibliography "~/Org-Docs/Papers/literature.bib")
;;Should be the same as paper_dir from lit_add.py
(setq bibtex-completion-library-path "~/Org-Docs/Papers/papers/")
;;Should be the same as paper_dir from lit_add.py
(setq bibtex-completion-notes-path "~/Org-Docs/Papers/notes/")
(setq bibtex-completion-notes-extension ".org")
(setq bibtex-completion-additional-search-fields '(journal))

(require 'org-ref)
(require 'org-ref-ivy)

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5
      bibtex-autokey-name-case-convert 'capitalize)

;;; Literature configuration options. These should probably coordinate
;;; with your helmb-bitex and org-ref options.
(setq literature-paper-superdirectory "/home/billbrod/Org-Docs/Papers/")
(setq literature-master-bib "literature.bib")

(setq reftex-default-bibliography '("~/Org-Docs/Papers/literature.bib"))
;; remove the download pdf function from hook, because it's unnecessary and
;; messes our functions
(setq org-ref-clean-bibtex-entry-hook (delete 'orcb-download-pdf org-ref-clean-bibtex-entry-hook))

;; I use pdf-tools in emacs to open the pdf. If you want to use your
;; system default (eg, evince or okular), uncomment this line.
;; (setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)

;; This tell bibtex-completion to look at the file field of the bibtex

(setq bibtex-completion-pdf-field "file")
;; (setq bibtex-completion-pdf-field nil)

(setf (alist-get 'markdown-mode bibtex-completion-format-citation-functions) 'bibtex-completion-format-citation-cite)
(setq bibtex-completion-cite-prompt-for-optional-arguments nil)

(setq bibtex-dialect 'BibTeX)
