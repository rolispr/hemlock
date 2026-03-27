# Hemlock

A programmable, multiplexing, text editor written in Common Lisp.

Hemlock shares Emacs's heritage. Extensible, self-documenting, Lisp-powered, but diverges where Emacs can't: concurrent buffer access, actor-based eval agents, and a modern terminal renderer.

## Highlights

- Tree-sitter syntax highlighting
- Built-in terminal emulator
- Helix-style modal editing (Normal/Select/Insert)
- Parallel eval agents via sento (local, process, remote)
- Immutable buffer snapshots (concurrent readers, single-thread writes, no locks)
- TTY and WebUI backends

## Quick start

```
sbcl --eval '(asdf:load-system :hemlock)' --eval '(hemlock:hemlock)'
```

## Status

Phase 1 complete: sento agents work (local + process, parallel eval, remoting).
Phase 2 in progress: FSet immutable buffer state, snapshot-based display and tree-sitter.

## Heritage

Hemlock was written at CMU in 1984. Public domain. This fork descends from the bluelisp/hemlock line and is actively maintained.

![Hemlock](doc/screenshot.png)
