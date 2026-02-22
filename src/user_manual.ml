let section_start_lines = [1; 30; 142; 257; 321]


let user_manual =
{|TUI Spreadsheet - User Manual

Overview
This is a terminal spreadsheet with multiple sheets, programmable cell formulas,
per-cell formatters, and a global config. Cells can be defined with literal values, formulas or full programs.

Quick start
1) Move with arrow keys to a cell.
2) Press i to edit the cell definition.
3) Type a value, formula or a full program. Examples:
 - 42 -> integer
 - Hello -> string (list of characters)
 - 13.5 -> float
 - = 1 + 2 * 3 -> value expression
 - ==
   a = 2
   return a + 3 -> full program (statements)
4) Press Esc to evaluate.


Contents:
[1] Navigation and controls
[2] Language syntax and semantics
[3] Built-in functions and operators
[4] Examples




[1] Navigation and controls


In each view, you can see the most important controls on the top of the screen.
Here is the full list of controls:

Main view (sheet)
- Arrow keys: move selection
- Enter: move down
- Tab: move right
- Shift + Arrow: resize row/column (up/down changes row height, left/right changes column width)
- , or <: previous sheet
- . or >: next sheet
- d: open cell definition (read-only)
- i: open cell definition (edit)
- =: open cell definition and prefill with '='
- r: open formatter (read-only)
- c: open config
- s: open sheets list
- f: file menu (save/open/new)
- z: undo
- y: redo
- q: quit
- h: show this manual

Cell definition editor
- Esc: evaluate and return to view
- i: enter insert mode (edit)
- =: enter insert mode and prefill with '=' if empty
- r: switch to formatter view
- d: switch to main view
- b: if the current text failed to parse, restore the last valid version
- Ctrl + Up/Down: adjust bottom panel height
- Ctrl + Left/Right: adjust value box width
- Page Up/Page Down: scroll editor up/down by a page
- Arrow keys, Enter, Tab, Shift + Arrow, ,, <, ., >, s, z, y : same as in main view

Formatter editor
Formatters are functions that take value, width and height as arguments
and return a list of chars (string) used to render the cell.
- Esc: evaluate and return to view
- i: enter insert mode
- d: switch to cell definition view
- r: switch to main view
- b: if the current text failed to parse, restore the last valid version
- Ctrl + Up/Down: adjust bottom panel height
- Ctrl + Left/Right: adjust value box width
- Page Up/Page Down: scroll editor up/down by a page
- Arrow keys, Enter, Tab, Shift + Arrow, ,, <, ., >, s, z, y : same as in main view

Note: If a cell has no custom formatter, it uses default_formatter.

Config editor
The config is evaluated before all cells. Changing it can recompute the sheets.
- Esc: evaluate and return to view
- i: edit config
- r: recompute sheets (only when config parses)
- b: revert to last saved config
- m: edit max computation cost
- Ctrl + Up/Down: adjust bottom panel height
- Page Up/Page Down: scroll editor up/down by a page
- Arrows: move view
- Shift + Arrows: move view
- z/y: undo/redo

Max computation cost
- While editing max cost, Esc or Enter exits max cost editor
- The value is clamped to [10000, 2000000000].
- Invalid input resets to 500000.

Note: if the config code or max cost were changed,
    you need to press r to apply changes and recompute the sheets.

Sheets list
- Arrow keys: move
- Enter: select sheet and return
- a: add new sheet
- e: rename sheet
- d: delete sheet
- Shift + Up/Down: reorder sheets
- Esc: return

Note: sheet's name can't be empty and must be unique.
    If you set an invalid name, it will be reverted to the previous one.

File menu
- s: save to current file
- e: edit file name
- n: new, empty file
- o: open file
- Esc: return

Note:
When saving, if the file doesn't exist, it will be created.
If it exists, it will be overwritten without confirmation.
If you're trying to save a file but pressing Enter does nothing,
it means it failed to save the file (probably due to invalid file name).
If you're trying to open a file but pressing Enter does nothing,
it means it failed to open the file (probably due to file not existing, check the file name).

Editor controls (insert mode)
- Arrow keys: move cursor
- Enter: new line with auto-indent (adds indent after a line ending with ':')
- Tab / Shift+Tab: indent / unindent line by 4 spaces
- Backspace/Delete: delete character before/after cursor
- Shift + Up/Down: move current line up/down
- Shift + Left/Right: move view and cursor left/right
- Page Up/Page Down: scroll editor up/down by a page




[2] Language syntax and semantics


Language syntax (Python-style)
Values
- int, float, bool (True/False), None
- list: [a, b, c]
- matrix: [| a, b, c |]
- dict: {key := value, ...}
- set: {a, b, c}
- characters: 'a', 'b', '\n', ...
  (English letters, digits, punctuation and whitespace characters are allowed.
  No escape sequences except \n, \\, \'.)
- strings are lists of chars in quotes: "abc"
    - escape sequences: \n, \\, \" (no \t)

Cell addresses
- A1, B3
- Sheet1~C5
- `Sheet name with spaces`~D10
- ranges:
    - list: A1:3 (same column), B:D5 (same row), Sheet1~C4:10
    - matrix: A1:B2, Sheet1~A1:C4
- built-in 'cell' function: cell(c, r), cell(sheetName, c, r) or cell(None, c, r)
    (where c and r are integers: column and row indices 1-indexed)

Expressions
- if ... then ... else ...
- let x = expr in expr
- lambda a, b: expr
- list/matrix/dict/set comprehensions:
    - [expr for x in it]
    - [| expr for x in it|]
    - {key := value for x in it}
    - {expr for x in it}
- list/matrix/dict/set comprehensions with filters:
    - [expr for x in it if cond]
    - [| expr for x in it if cond |]
    - {key := value for x in it if cond}
    - {expr for x in it if cond}
- function calls: f(a, b)
- indexing: a[b]
- type check: expr : TypeName

Statements (used in full programs)
- assignment: x = expr, a[1] = expr, x += 2, x -= 3, ...
- if/else, while, for x in iterable:
- def name(args): ... (also used to define operators)
- extend name(args): ...
- extended name(args): ...
- try: ... catch err: ...
- return expr
- newtype typeName, constructorName, destructorName
- newtype typeName, valueName (for singletons)
- del x, del x[expr]
- raise expr
- break, continue

Notes on code semantics:
- everything is accessed by value, no references
- you can raise any value as an exception
- if you extend function/operator interpreter will return the result
    of first implementation that didn't raise InvalidArgumentsError
- extend adds a new implementation with highest precedence
- extended adds a new implementation with lowest precedence.
- constructors and destructors of newtypes are single-argument functions,
    constructors create a value of the new type from the argument,
    destructors return the argument passed to the constructor
    or raise an error if the value is not of the correct type.
- 'bool' and 'list' functions are used for automatic type conversions
    - 'bool' is used in conditions
    - 'list' is used for iterating
- 'default_formatter' is used to convert values to strings for display
    if no custom formatter is defined for a cell

Comments and indentation
- single line: # comment
- multiline: #= ... =#
- indentation is 4 spaces per level

Operators and precedence
- Operators are user-definable and can include: + - * / % ^ < > = ! & | @ ?
    - operator starting with '!' is unary (except '!=', which is binary)
    - define __pos__ or __neg__ to support unary + and -
    - define __in__ to support 'in' operator
    - define __call__ to support calling for non-function values (e.g. f(1, 2))
    - define __getitem__ to support getting value by index (e.g. a[1])
    - define __setitem__ to support setting value by index (e.g. a[1] = 2)
    - define __delitem__ to support deleting by index (e.g. del a[1])
    - '=' cannot be last
    - <= >= == != are allowed
- Unary operators: +, -, !..., not
- Precedence based on prefix of operator (highest to lowest):
    1) unary +, unary -, !, not
    2) **
    3) * / %
    4) + -
    5) @ ^ (right associative)
    6) & | ?
    7) : (type check), in
    8) < <= > >= == !=
    9) and
    10) or

Cell definitions
- Plain text (no leading '='): interpreted as integer, float or a string (list of chars)
- = expr: expression (same as return expr)
- ==
  ... : full program (statements)

Formatter and config definitions are always full programs (don't start with '==').




[3] Built-in functions and operators


Built-in operators:
+ : addition (also list concatenation and set/dictionary union)
- : subtraction (also set difference)
* : multiplication (also list repetition)
/ : division
% : modulo
** : exponentiation
__pos__ : unary plus
__neg__ : unary minus
== : equality check
!= : inequality check
<, <=, >, >= : comparisons
and, or, not : boolean operators (with conversion to bool,
    'or' won't evaluate the second operand if the first one is truthy,
    'and' won't evaluate the second operand if the first one is falsy)
__in__ : membership test
__getitem__ : getting value by index (e.g. a[1])
__setitem__ : setting value by index (e.g. a[1] = 2)
__delitem__ : deleting by index (e.g. del a[1])

Built-in functions:
- len : length of a list, matrix, dictionary or set
- int : convert to integer
- float : convert to float
- bool : convert to boolean (empty lists, sets, dictionaries,
    False, None, 0.0 and 0 are falsy, the rest is truthy)
- list : convert matrix/set/dictionary to list 
- matrix : convert list of lists to matrix
- set : convert list to set, set() returns an empty set
- dict : convert list of key-value pairs to dictionary
- round : round a float to the nearest integer,
    if second argument is given, round to that many decimal places
- zip: given multiple lists, return a list of lists where the i-th list contains the i-th elements of the input lists
- default_formatter : default formatter used to convert values to strings for display
- str : convert to string (list of chars)
- rows : given a matrix, return a list of its rows
- columns : given a matrix, return a list of its columns
- transpose : given a matrix, return its transpose
- range : range(n) returns a list of integers from 0 to n-1,
    range(a, b) returns a list of integers from a to b-1,
    range(a, b, step) uses step as the increment (can be negative)
- slice : given a list and slice parameters returns the corresponding slice of the list
    supported arguments: (stop), (start, stop), (start, None) (start, stop, step), (start, None, stop)
- cell : cell(c, r) and cell(None, c, r) return the value of the cell at column c and row r of the current sheet (1-indexed)
    cell(sheetName, c, r) returns the value of the cell at column c and row r of the sheet with the given name

Built-in types:
- int, float, char, bool, None
- list, matrix, dict, set
- function
- InvalidArgumentsError, DivisionByZeroError, IndexError (same as value names)
- TimeLimitExceededError

Note:
TimeLimitExceededError is raised when computation cost exceeds the limit set in config.
It can't be raised or caught by user code. When accessing a cell that failed with TimeLimitExceededError
it will be converted to "Time limit exceeded in accessed cell" error)




[4] Examples


1) Simple formula
= A1 + 10

2) Multi-line program
==
def average(lst):
    xs = [x for x in lst if x != None]
    if len(xs) == 0:
        return None
    return sum(xs) / len(xs)

return average(A1:10)

3) Custom formatter
def progress_bar(value, width, height):
    result = "["
    value = max(0, min(1, value))
    percent = str(int(value*100)) + "%"
    length = width - 2 - len(percent)
    progress = round(value*length)
    result += "#"*progress
    result += "."*(length-progress)
    result += "]"
    result += percent
    return result

return progress_bar

4) Config with custom polynomial type

newtype polynomial, _Polynomial, get_coeffs

def Polynomial(xs):
    coeffs = list(xs)
    i = len(coeffs)
    while i > 0:
        if coeffs[i-1]:
            return _Polynomial(slice(coeffs,i))
        i -= 1
    return _Polynomial([])
    
del _Polynomial

extended +(pol1, pol2):
    c1 = get_coeffs(pol1)
    c2 = get_coeffs(pol2)
    if len(c1) < len(c2):
        c1 += [0]*(len(c2)-len(c1))
    else:
        c2 += [0]*(len(c1)-len(c2))
    c3 = [v[0]+v[1] for v in zip(c1,c2)]
    return Polynomial(c3)

extended *(pol1, pol2):
    c1 = get_coeffs(pol1)
    c2 = get_coeffs(pol2)
    res = [0] * (len(c1)+len(c2)-1)

    for i in range(len(c1)):
        for j in range(len(c2)):
            res[i+j] += c1[i]*c2[j]

    return Polynomial(res)

def enumerate(lst):
    lst = list(lst)
    return [[i, lst[i]] for i in range(len(lst))]

def reverse(lst):
    if not (lst : list):
        raise InvalidArgumentsError
    l = len(lst)
    return [lst[l-1-i] for i in range(l)]

extend str(pol):
    c = get_coeffs(pol)
    if c == []:
        return "0"
    if len(c) == 1:
        return str(c[0])
    
    p = enumerate(c)
    last_c_str = if c[-1] == 1 then "" else if c[-1] == -1 then "-" else str(c[-1])
    s = last_c_str + if len(c) > 2 then "x^" + str(p[-1][0]) else "x"

    for i in slice(p, -2, None, -1):
        xpow = if i[0] > 1 then "x^" + str(i[0]) else if i[0] == 1 then "x" else ""
        if not (i[1] : int) and not (i[1] : float) or i[1] > 0:
            s += "+" + (if i[1] == 1 and i[0] then "" else str(i[1])) + xpow
            continue
        if i[1] < 0:
            s += "-" + (if i[1] == -1 and i[0] then "" else str(-i[1])) + xpow
    
    return s

extended __neg__(pol):
    c = get_coeffs(pol)
    return Polynomial([-v for v in c])

extended -(pol1, pol2):
    return pol1 + -pol2

extend default_formatter(pol, w, h):
    if not (pol : polynomial):
        raise InvalidArgumentsError
    return str(pol)

def sum(lst):
    total = 0
    for item in lst:
        total += item
    return total

extend __call__(pol, x):
    res = 0
    for c in reverse(get_coeffs(pol)):
        res = res * x + c
    return res

def min(a, b):
    if a <= b:
        return a
    return b

def max(a, b):
    if a >= b:
        return a
    return b

def @(matrix1, matrix2):
    # matrix multiplication
    if not matrix1 : matrix:
        raise InvalidArgumentsError
    if not matrix2 : matrix:
        raise InvalidArgumentsError
    
    num_cols1 = len(matrix1)[1]
    num_rows2 = len(matrix2)[0]
    if num_cols1 != num_rows2:
        raise "Can't multiply matrices with incompatible dimensions"
    
    return [|
            [
                sum([v[0]*v[1] for v in zip(row, col)])
                for col in columns(matrix2)
            ]
            for row in rows(matrix1)
        |]



|}


let manual_length = String.fold_left (fun acc c -> acc + if c = '\n' then 1 else 0) 0 user_manual

