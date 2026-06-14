GNATdoc.Documentation = {
  "label": "ZanyBlue.Text.Metrics",
  "qualifier": "",
  "summary": [
  ],
  "description": [
  ],
  "entities": [
    {
      "entities": [
        {
          "label": "Write_Usage",
          "qualifier": "",
          "line": 45,
          "column": 14,
          "src": "srcs/zanyblue-text-metrics.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 45,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Write_Usage",
                      "href": "docs/zanyblue__text__metrics___spec.html#L45C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 46,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Destination",
                      "href": "docs/zanyblue__text__metrics___spec.html#L46C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "UXStrings.Text_IO.File_Type"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 47,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Catalog",
                      "href": "docs/zanyblue__text__metrics___spec.html#L47C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Catalog_Type",
                      "href": "docs/zanyblue__text__catalogs___spec.html#L606C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":="
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Standard_Catalog"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Write a summary of the usage of each message in the catalog to the\n"
                },
                {
                  "kind": "span",
                  "text": "given file as an XML document.  This should be call after the\n"
                },
                {
                  "kind": "span",
                  "text": "application has executed, e.g.,\n"
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
                },
                {
                  "kind": "span",
                  "text": "<zanyblue-message-usage>\n"
                }
              ]
            },
            {
              "kind": "code",
              "children": [
                {
                  "number": 1,
                  "children": [
                    {
                      "kind": "span",
                      "text": "<message facility=\"myfac\""
                    }
                  ]
                },
                {
                  "number": 2,
                  "children": [
                    {
                      "kind": "span",
                      "text": "         key=\"mykey\""
                    }
                  ]
                },
                {
                  "number": 3,
                  "children": [
                    {
                      "kind": "span",
                      "text": "         locale=\"mylocale\""
                    }
                  ]
                },
                {
                  "number": 4,
                  "children": [
                    {
                      "kind": "span",
                      "text": "         count=\"10\" />"
                    }
                  ]
                },
                {
                  "number": 5,
                  "children": [
                    {
                      "kind": "span",
                      "text": ""
                    }
                  ]
                },
                {
                  "number": 6,
                  "children": [
                    {
                      "kind": "span",
                      "text": ""
                    }
                  ]
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "Destination",
              "line": 46,
              "column": 7,
              "type": {
                "label": "File_Type"
              },
              "description": [
              ]
            },
            {
              "label": "Catalog",
              "line": 47,
              "column": 7,
              "type": {
                "label": "ZanyBlue.Text.Catalogs.Catalog_Type",
                "docHref": "docs/zanyblue__text__catalogs___spec.html#L57C9"
              },
              "description": [
              ]
            }
          ]
        },
        {
          "label": "Write_Usage",
          "qualifier": "",
          "line": 61,
          "column": 14,
          "src": "srcs/zanyblue-text-metrics.ads.html",
          "summary": [
          ],
          "description": [
            {
              "kind": "code",
              "children": [
                {
                  "kind": "line",
                  "number": 61,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "keyword",
                      "text": "procedure"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Write_Usage",
                      "href": "docs/zanyblue__text__metrics___spec.html#L61C14"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 62,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "     "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "("
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "File_Name",
                      "href": "docs/zanyblue__text__metrics___spec.html#L62C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "String",
                      "href": "docs/zanyblue___spec.html#L47C12"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                },
                {
                  "kind": "line",
                  "number": 63,
                  "children": [
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "      "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Catalog",
                      "href": "docs/zanyblue__text__metrics___spec.html#L63C7"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": "   "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Catalog_Type",
                      "href": "docs/zanyblue__text__catalogs___spec.html#L606C9"
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ":="
                    },
                    {
                      "kind": "span",
                      "cssClass": "text",
                      "text": " "
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": "Standard_Catalog"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ")"
                    },
                    {
                      "kind": "span",
                      "cssClass": "identifier",
                      "text": ";"
                    }
                  ]
                }
              ]
            },
            {
              "kind": "paragraph",
              "children": [
                {
                  "kind": "span",
                  "text": "Write the message usage summary to the named file.\n"
                }
              ]
            }
          ],
          "parameters": [
            {
              "label": "File_Name",
              "line": 62,
              "column": 7,
              "type": {
                "label": "ZanyBlue.String",
                "docHref": "docs/zanyblue___spec.html#L47C12"
              },
              "description": [
              ]
            },
            {
              "label": "Catalog",
              "line": 63,
              "column": 7,
              "type": {
                "label": "ZanyBlue.Text.Catalogs.Catalog_Type",
                "docHref": "docs/zanyblue__text__catalogs___spec.html#L57C9"
              },
              "description": [
              ]
            }
          ]
        }
      ],
      "label": "Subprograms"
    }
  ]
};