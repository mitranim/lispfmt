`lispfmt` is a fuzzy formatter for Common Lisp and Scheme.

The style is biased towards structured editing. When a form is already multiline, the formatter spends more lines so child forms can be moved, deleted, or inserted with line-based editor commands.

The implementation was mostly **bot-prompted** with OpenAI Codex.

## TOC

- [Style](#style)
- [Examples](#examples)
- [Usage](#usage)
- [Make](#make)
- [Limitations](#limitations)

## Style

- Fixed 2-space indentation.
- Line endings normalized to LF.
- In multiline forms, closing delimiters are aligned to opening tokens.
- Normalized spacing:
  - One space between adjacent same-line forms.
  - 0 or 1 blank lines between adjacent multiline forms.
- Comments are preserved and aligned to code.
- Nested `#| ... |#` block comments are supported.
- Reader-like prefix tokens are clamped to the next form where supported.
- In `def*` forms, signature is placed on the first line.
- `:key val` pairs when multiline.
- In calls, function name is always on the first line.
- `loop` uses heuristics to normalize symbolic keywords to literal `:keywords` where recognized.

## Examples

Closing delimiters placed on separate lines and aligned to openers. Structure is obvious. Forms are easy to move:

```lisp
(defun some-func ()
  (let
    (
      (one 10)
      (two 20)
    )
    (print (+ one two))
    (print (+ two one))
  )
)
```

Grouping of `:key val` pairs when multiline:

```lisp
(dict
  :one 10
  :two 20
  :three 30
)
```

In `loop`, symbolic keywords are normalized to `:keywords` where recognized:

```lisp
; From:
(loop for ind below ceil do (print ind))
; To:
(loop :for ind :below ceil :do (print ind))

; When multiline:
(loop
  :for form
  :in forms
  :for index
  :from 0
  :do
  (print form)
)
```

Block comments are treated as forms:

```lisp
(one
  #|
  comment
  |#
  two
)
```

## Usage

Library API:

```lisp
(lispfmt:format-string string)
```

Returns a formatted string or signals `lispfmt:formatter-error`.

Script CLI. Currently requires [SBCL](https://www.sbcl.org):

```sh
sbcl --script cli.lisp < input.lisp > output.lisp
```

Built CLI:

```sh
make build
./lispfmt < input.lisp > output.lisp
```

The CLI accepts no arguments. Unsupported arguments, formatter errors, and unexpected errors are written to stderr with a non-zero exit code.

## Make

```sh
make test
make test_lib
make test_cli
make build
```

Install by symlinking the built executable:

```sh
make install
```

Override install location:

```sh
make install INSTALL_DIR="$HOME/bin"
```

Remove the symlink:

```sh
make uninstall
```

## Limitations

- Fuzzy CL/Scheme surface formatting, not exact CL or Scheme reader emulation.
- Mostly CL-biased. Scheme coverage is limited.
- Not configurable.
- No CLI options.
- `loop` support is a set of heuristics, not a full CL `loop` parser.

## License

https://unlicense.org
