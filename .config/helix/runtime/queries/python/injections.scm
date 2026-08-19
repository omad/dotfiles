; Inject a language into a string preceded by a comment like: # lang=css
; The comment text (e.g. "# lang=css") is matched against each language's
; injection-regex, which will find "css" or "html" as a substring.
;
; Usage:
;   # lang=css
;   _STYLE = """body { color: red; }"""
;
;   # lang=html
;   _PAGE = """<html>...</html>"""

; Module-level: comment followed by expression_statement with assignment
(module
  (comment) @injection.language
  .
  (expression_statement
    (assignment
      right: (string
        (string_content) @injection.content))))
