;;To use this, make sure the paths in literature-update, literature-add, and the helm-bibtex configurations are all correct

(add-to-list 'load-path "~/Documents/Literature")
(require 'literature)

(global-set-key (kbd "s-u") 'literature-update)
(add-hook 'kill-emacs-hook 'literature-update)

(global-set-key (kbd "s-a") 'literature-add-file)

;;Helm-bibtex configuration options
;;Location of your master bib file (paper_dir from lit_add.py + literature.bib)
(setq bibtex-completion-bibliography "~/Org-Docs/Papers/literature.bib")
;;Should be the same as paper_dir from lit_add.py
(setq bibtex-completion-library-path "~/Org-Docs/Papers/")
;;Should be the same as paper_dir from lit_add.py
(setq bibtex-completion-notes-path "~/Org-Docs/Papers/")
(setq bibtex-completion-notes-extension ".org")
(setq bibtex-completion-additional-search-fields '(journal))

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
(setq literature-paper-directory "/home/billbrod/Org-Docs/Papers/")
(setq literature-master-bib "literature.bib")
(setq literature-master-org "literature.org")

(setq reftex-default-bibliography '("~/Org-Docs/Papers/literature.bib"))
;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Org-Docs/Papers/literature.org"
      org-ref-default-bibliography '("~/Org-Docs/Papers/literature.bib")
      org-ref-pdf-directory "~/Org-Docs/Papers/")

;; make sure you have dash, helm, helm-bibtex, ebib, s, f, hydra and key-chord
;; in your load-path

;;; if you want a hydra for org-ref-bibtex
(define-key bibtex-mode-map (kbd "C-c j") 'org-ref-bibtex-hydra/body)

;; I use pdf-tools in emacs to open the pdf. If you want to use your
;; system default (eg, evince or okular), uncomment this line.
;; (setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool)

;; This tell bibtex-completion to look at the File field of the bibtex
;; entry to figure out which pdf to open
(setq bibtex-completion-pdf-field "File")
