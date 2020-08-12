;;; dra-sphinx.el -*- lexical-binding: t; -*-


(require 'cl-lib)
(require 'company)

(defconst sphinx-rst-directives
  '("code-block" "toctree" "note" "warning" "versionadded" "versionchanged"
    "deprecated" "seealso" "rubric" "centered" "hlist" "highlight"
    "literalinclude" "glossary" "sectionauthor" "codeauthor" "index"
    "only" "tabularcolumns" "math" "productionlist"
    ; admonitions
    ; images
    "image" "figure"
    ; additional body elements
    "contents" "container" "rubric" "topic" "sidebar" "parsed-literal"
    "epigraph" "highlights" "pull-quote" "compound"
    ; special tables
    "table" "csv-table" "list-table"))

(defun company-sphinx-backend (command &optional arg &rest ignored)
  "Try and write a company completion for sphinx"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-sphinx-backend))
    (prefix (when (looking-back "\.\. .*"))
            (match-string 0))
    (candidates
     (cl-remove-if-not
      (lambda (c) (string-prefix-p arg c))
      sphinx-rst-directives))
;    (candidates (when (equal arg ".. ")
;                  (list "foobar" "foobaz" "foobarbaz")))
    (meta (format "This value is named %s" arg))))

..
