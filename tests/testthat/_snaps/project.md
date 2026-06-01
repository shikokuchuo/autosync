# format_file_tree renders a nested tree

    Code
      cat(autosync:::format_file_tree(c("/charlie/data.csv", "/charlie/index.qmd",
        "/charlie/deep/notes.txt", "/notes/todo.md", "/readme.md")))
    Output
      /
      ├─ charlie/
      │  ├─ deep/
      │  │  └─ notes.txt
      │  ├─ data.csv
      │  └─ index.qmd
      ├─ notes/
      │  └─ todo.md
      └─ readme.md

