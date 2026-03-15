# TUI Spreadsheet

Terminal spreadsheet written in OCaml with programmable cells, custom renderers, multiple sheets, and JSON persistence.

## Overview

This project is a text user interface spreadsheet where each cell can contain:

- A literal value
- A formula expression
- A imperative program in a Python-like language

In addition to per-cell code, there is a global config program. Everything defined in config is available from every cell.

The spreadsheet supports multiple sheets, undo/redo, custom row/column sizes, custom formatters, and save/load to readable JSON files.

## Key features

- Multiple sheets with reordering and renaming
- Cell language with expressions and statements
- Global config program shared by all cells
- Per-cell formatter functions
- Custom operators and function/operator extension
- Cycle detection during evaluation
- Computation cost limit (not wall-clock time)
- Undo/redo and interactive editors
- Save/open/new file workflow

## Build and run

Build:

```bash
dune build
```

Run:

```bash
dune exec ./src/main.exe
```

## Quick usage

From the main spreadsheet view:

- Move with arrow keys
- Press i to edit selected cell
- Press = to start formula input
- Press r to edit selected cell formatter
- Press c to open global config
- Press s to manage sheets
- Press f to open file menu (save/open/new)
- Press h to open built-in manual

In editors, press Esc to evaluate/apply and return.

## Cell definitions

Cells support three main forms:

1. Literal (no leading =)

Examples:

- 42
- 13.5
- Hello

Literal text is interpreted as int, float, or string (string is represented as a list of chars internally).

2. Formula expression (= expr)

Example:

```python
= 1 + 2 * 3
```

3. Imperative program (== followed by statements)

Example:

```python
==
a = 2
return a + 3
```

## Language summary

The cell/config language is Python-like with indentation-based blocks.

Supported concepts include:

- Primitive values: int, float, bool, None, char
- Collections: list, matrix, dict, set
- Comprehensions with optional filters
- Conditionals, loops, functions, operators, raise, try/except
- newtype declarations with constructor/destructor functions
- Type checks using expr : TypeName
- Cell addressing and ranges (including cross-sheet references)

Examples of addressing:

- A1
- Sheet1~C5
- `Sheet name with spaces`~D10
- A1:B2
- cell(2, 3)
- cell("Sheet", 2, 3)

Comments:

- Single-line: # comment
- Multiline: #= ... =#

Indentation uses 4 spaces.

## Global config

Config is a program evaluated before cell computation.

- Use it to define helper functions, types, operators, and default behavior
- Config changes can trigger recomputation of sheets
- You can edit max computation cost in config view

## Formatters

Each cell can have a custom formatter.

A formatter is a function with signature:

```python
formatter(value, width, height) -> string
```

The returned string (list of chars) is used to render the cell.

If a cell has no custom formatter, default_formatter is used.

Example formatter:

```python
def progress_bar(value, width, height):
    result = "["
    value = max(0, min(1, value))
    percent = str(int(value * 100)) + "%"
    length = width - 2 - len(percent)
    progress = round(value * length)
    result += "#" * progress
    result += "." * (length - progress)
    result += "]"
    result += percent
    return result

return progress_bar
```

## Errors and evaluation limits

### Cyclic dependencies

If cell evaluation creates a dependency cycle, cyclic dependency error is raised.

### Computation cost limit

The engine tracks an abstract computation cost derived from executed operations.

- It is not based on elapsed wall-clock time
- Exceeding max cost raises TimeLimitExceededError
- Max cost is user-configurable in config view

When accessing a cell that failed with time limit exceeded, it is reported as an accessed-cell time limit error.

## Persistence format

Spreadsheets are saved as readable JSON files.

See example.json for a full example.

## User manual

User manual is available in the app by pressing h. It contains detailed explanations of all features, language constructs, and examples. You can also find it in the repository as user_manual.ml .