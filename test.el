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

;;; https://github.com/machc/pdf-tools-org/blob/master/pdf-tools-org.el
;;; this has some useful ideas, but we can't use it as is.

;;; https://gist.github.com/myrjola/15585e3461b4d3178953 is another
;;; attempt to something similar.

;;; These come from the related issues
;;; https://github.com/politza/pdf-tools/pull/133 and
;;; https://github.com/politza/pdf-tools/issues/134

;;; http://matt.hackinghistory.ca/2015/11/11/note-taking-with-pdf-tools/
;;; is someone doing something similar to me.

;;; With the above several links, I should be able to write something
;;; up myself that works. Once we get that going (along with the
;;; extract doi), we can remove the requirement for python-poppler.
