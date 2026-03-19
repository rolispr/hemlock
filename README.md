# Hemlock

Hemlock is a Common Lisp editor with a long history — born at CMU in the 1980s, it shipped inside CMUCL and later SBCL. This is an active modernization: dead backends removed, dependencies cleaned up, a tree-sitter integration for real syntax highlighting, and a Helix-style modal editing layer being built on top of the original architecture. The core is still the same battle-tested text model.
**Status:** v0.2.0 — loads clean on SBCL, TTY backend fully working, modal editing in place. Webui backend in progress. 

---

### What's in here

- **TTY backend** — terminal rendering, full color support (truecolor, 256, 16)
- **Tree-sitter** — async parse actor, per-line syntax colors, stable-tree for safe reads from the command thread
- **Modal editing** — Normal/Select modes, full Helix keymap, spanning selections, TS textobjects
- **Selection coloring** — region highlight painted via slot-array merge with syntax colors
- **Wire protocol** — bidirectional master/slave over TCP for eval servers
- **Echo area completions** — interactive completion in the minibuffer

---

### Building

```
(asdf:load-system :hemlock)
```

Dependencies managed with [ocicl](https://github.com/ocicl/ocicl).

---

![Hemlock](doc/screenshot.png)
