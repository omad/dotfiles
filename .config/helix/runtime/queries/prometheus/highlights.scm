[
  (comment)
  (type_line)
  (help_line)
] @comment

(comment) @spell

(help_line
  metric_help: (docstring) @spell)

[
  "TYPE"
  "HELP"
] @keyword

(type) @type.builtin

(identifier) @type

(label
  label_name: (identifier) @property
  label_value: (string) @string)

(label
  label_name: (identifier) @attribute.builtin
 (#eq? @attribute.builtin "le"))

(expression
  metric_value: (number) @number.float)

(expression
  timestamp: (number) @number)

[ "{" "}"] @punctuation.bracket

[ "=" ] @operator

[ "," ] @punctuation.comma