(org-ref-extract-doi-from-pdf "/home/billbrod/Org-Docs/Papers/Wagenmakers2016/Wagenmakers2016.pdf")
(setq org-ref-pdf-doi-regex "\\b(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?![\"&\'<>])\S)+)\\b")
;;; So I think org-ref-pdf has lots of what I need already, but their
;;; regex isn't as good as the one above. So try and replace it with
;;; that (though I need to convert it to emacs regex). org-ref-pdf is
;;; also mainly drag and drop, which is actually fine, but I'll need
;;; to make changes so that, instead of trying to add to the master
;;; bibtex file, it adds to {key}/{key}.bib. Will also need to check
;;; what key it returns to make sure it's in the appropritae
;;; style. And something to add from a bibtex file. And change over lit_update.
