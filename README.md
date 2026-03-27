# Hemlock

A programmable, multiplexing, text editor written in Common Lisp.

## Highlights

- Treesitter for syntax highlighting
- Builtin terminal emulator with vt100/xterm capabilities
- Modal editing mode inspired by Helix and Vim
- Parallel agents via sento for local, and remote process management
- Immutable buffer snapshoting
- TTY and WebUI backends

## Quick start

```
ocicl install
sbcl --eval '(asdf:load-system :hemlock)' --eval '(hemlock:hemlock)'
```

## Status/Scope/Roadmap

TBD

![Hemlock](doc/screenshot.png)
