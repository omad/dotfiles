register /home/omad/.cargo/bin/nu_plugin_hcl  {
  "sig": {
    "name": "from hcl",
    "usage": "Parse text as .hcl and create a record",
    "extra_usage": "",
    "search_terms": [],
    "required_positional": [],
    "optional_positional": [],
    "rest_positional": null,
    "named": [
      {
        "long": "help",
        "short": "h",
        "arg": null,
        "required": false,
        "desc": "Display the help message for this command",
        "var_id": null,
        "default_value": null
      }
    ],
    "input_output_types": [
      [
        "String",
        {
          "Record": []
        }
      ]
    ],
    "allow_variants_without_examples": false,
    "is_filter": false,
    "creates_scope": false,
    "allows_unknown_args": false,
    "category": "Formats"
  },
  "examples": [
    {
      "example": "'provider \"aws\" {\n  region = \"us-east-1\"\n}\nresource \"aws_instance\" \"web\" {\n  ami           = \"ami-a1b2c3d4\"\n  instance_type = \"t2.micro\"\n}' | from hcl",
      "description": "Convert .hcl data into record",
      "result": {
        "Record": {
          "cols": [
            "provider",
            "resource"
          ],
          "vals": [
            {
              "Record": {
                "cols": [
                  "aws"
                ],
                "vals": [
                  {
                    "Record": {
                      "cols": [
                        "region"
                      ],
                      "vals": [
                        {
                          "String": {
                            "val": "us-east-1",
                            "span": {
                              "start": 0,
                              "end": 0
                            }
                          }
                        }
                      ],
                      "span": {
                        "start": 0,
                        "end": 0
                      }
                    }
                  }
                ],
                "span": {
                  "start": 0,
                  "end": 0
                }
              }
            },
            {
              "Record": {
                "cols": [
                  "aws_instance"
                ],
                "vals": [
                  {
                    "Record": {
                      "cols": [
                        "web"
                      ],
                      "vals": [
                        {
                          "Record": {
                            "cols": [
                              "ami",
                              "instance_type"
                            ],
                            "vals": [
                              {
                                "String": {
                                  "val": "ami-a1b2c3d4",
                                  "span": {
                                    "start": 0,
                                    "end": 0
                                  }
                                }
                              },
                              {
                                "String": {
                                  "val": "t2.micro",
                                  "span": {
                                    "start": 0,
                                    "end": 0
                                  }
                                }
                              }
                            ],
                            "span": {
                              "start": 0,
                              "end": 0
                            }
                          }
                        }
                      ],
                      "span": {
                        "start": 0,
                        "end": 0
                      }
                    }
                  }
                ],
                "span": {
                  "start": 0,
                  "end": 0
                }
              }
            }
          ],
          "span": {
            "start": 0,
            "end": 0
          }
        }
      }
    }
  ]
}

register /home/omad/.cargo/bin/nu_plugin_highlight  {
  "sig": {
    "name": "highlight",
    "usage": "Syntax highlight source code.",
    "extra_usage": "",
    "search_terms": [
      "syntax",
      "highlight",
      "highlighting"
    ],
    "required_positional": [],
    "optional_positional": [
      {
        "name": "language",
        "desc": "language or file extension to help language detection",
        "shape": "String",
        "var_id": null,
        "default_value": null
      }
    ],
    "rest_positional": null,
    "named": [
      {
        "long": "help",
        "short": "h",
        "arg": null,
        "required": false,
        "desc": "Display the help message for this command",
        "var_id": null,
        "default_value": null
      },
      {
        "long": "theme",
        "short": "t",
        "arg": "String",
        "required": false,
        "desc": "theme used for highlighting",
        "var_id": null,
        "default_value": null
      },
      {
        "long": "list-themes",
        "short": null,
        "arg": null,
        "required": false,
        "desc": "list all possible themes",
        "var_id": null,
        "default_value": null
      }
    ],
    "input_output_types": [
      [
        "String",
        "String"
      ],
      [
        "Any",
        {
          "Table": [
            [
              "id",
              "String"
            ],
            [
              "name",
              "String"
            ],
            [
              "author",
              "String"
            ],
            [
              "default",
              "Bool"
            ]
          ]
        }
      ]
    ],
    "allow_variants_without_examples": false,
    "is_filter": false,
    "creates_scope": false,
    "allows_unknown_args": false,
    "category": "Strings"
  },
  "examples": [
    {
      "example": "open Cargo.toml -r | highlight toml",
      "description": "Highlight a toml file by its file extension",
      "result": null
    },
    {
      "example": "open src/main.rs | highlight Rust",
      "description": "Highlight a rust file by programming language",
      "result": null
    },
    {
      "example": "open example.sh | highlight",
      "description": "Highlight a bash script by inferring the language (needs shebang)",
      "result": null
    },
    {
      "example": "open Cargo.toml -r | highlight toml -t ansi",
      "description": "Highlight a toml file with another theme",
      "result": null
    },
    {
      "example": "highlight --list-themes",
      "description": "List all available themes",
      "result": null
    }
  ]
}

