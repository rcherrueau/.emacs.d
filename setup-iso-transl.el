;; http://www.emacswiki.org/emacs/TypographicalPunctuationMarks#toc11
;; http://stackoverflow.com/a/12331998/2072144
;; Extends the iso-trans.el

(require 'iso-transl)

(iso-transl-define-keys
 `(("^0" . ,(vector (decode-char 'ucs #x2070)))
   ("^4" . ,(vector (decode-char 'ucs #x2074))) ; 1-3 already defined
   ("^5" . ,(vector (decode-char 'ucs #x2075)))
   ("^6" . ,(vector (decode-char 'ucs #x2076)))
   ("^7" . ,(vector (decode-char 'ucs #x2077)))
   ("^8" . ,(vector (decode-char 'ucs #x2078)))
   ("^9" . ,(vector (decode-char 'ucs #x2079)))
   ("^+" . ,(vector (decode-char 'ucs #x207A)))
   ("^-" . ,(vector (decode-char 'ucs #x207B)))
   ("^=" . ,(vector (decode-char 'ucs #x207C)))
   ("^(" . ,(vector (decode-char 'ucs #x207D)))
   ("^)" . ,(vector (decode-char 'ucs #x207E)))
   ("_0" . ,(vector (decode-char 'ucs #x2080)))
   ("_1" . ,(vector (decode-char 'ucs #x2081)))
   ("_2" . ,(vector (decode-char 'ucs #x2082)))
   ("_3" . ,(vector (decode-char 'ucs #x2083)))
   ("_4" . ,(vector (decode-char 'ucs #x2084)))
   ("_5" . ,(vector (decode-char 'ucs #x2085)))
   ("_6" . ,(vector (decode-char 'ucs #x2086)))
   ("_7" . ,(vector (decode-char 'ucs #x2087)))
   ("_8" . ,(vector (decode-char 'ucs #x2088)))
   ("_9" . ,(vector (decode-char 'ucs #x2089)))
   ("_+" . ,(vector (decode-char 'ucs #x208A)))
   ("_-" . ,(vector (decode-char 'ucs #x208B)))
   ("_=" . ,(vector (decode-char 'ucs #x208C)))
   ("_(" . ,(vector (decode-char 'ucs #x208D)))
   ("_)" . ,(vector (decode-char 'ucs #x208E)))))

(provide 'setup-iso-transl)
