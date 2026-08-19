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

((comment) @injection.content
 (#set! injection.language "comment"))

; Match all 9 functions in the `re` module from the standard library that
; that takes a regex pattern as first argument.
; https://docs.python.org/3/library/re.html#functions
(call
  function: (attribute
    object: (identifier) @_module (#eq? @_module "re")
    attribute: (identifier) @_function (#any-of? @_function "compile" "search" "match" "fullmatch" "sub" "subn" "findall" "finditer" "split"))
  arguments: (argument_list
    . (string
        (string_content) @injection.content))
  (#set! injection.language "regex"))
