;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; Helix-style modal editing key bindings for Hemlock.
;;;
;;; Depends on: modal.lisp (command definitions)
;;;

(in-package :hemlock)


;;;; Global: Escape.

;;; Single global Escape binding.  "Helix Escape" is context-sensitive:
;;; Select → collapse, Normal → deactivate region, Insert(modal) → Normal Mode,
;;; non-modal → keyboard quit.
(bind-key "Helix Escape" #k"escape")


;;;; Normal mode -- motions (collapse selection before moving).

;;; h / j / k / l
(bind-key "Helix Forward Character"  #k"l" :mode "Normal")
(bind-key "Helix Backward Character" #k"h" :mode "Normal")
(bind-key "Helix Next Line"          #k"j" :mode "Normal")
(bind-key "Helix Previous Line"      #k"k" :mode "Normal")

;;; Arrow keys (mirrors hjkl)
(bind-key "Helix Forward Character"  #k"rightarrow"  :mode "Normal")
(bind-key "Helix Backward Character" #k"leftarrow"   :mode "Normal")
(bind-key "Helix Next Line"          #k"downarrow"   :mode "Normal")
(bind-key "Helix Previous Line"      #k"uparrow"     :mode "Normal")

;;; Word motions (word = delimited by character class)
(bind-key "Helix Forward Word"      #k"w" :mode "Normal")
(bind-key "Helix Backward Word"     #k"b" :mode "Normal")
(bind-key "Helix Forward Word End"  #k"e" :mode "Normal")

;;; Bigword motions (WORD = any non-whitespace run)
(bind-key "Helix Forward Upcase Word"      #k"W" :mode "Normal")
(bind-key "Helix Backward Upcase Word"     #k"B" :mode "Normal")
(bind-key "Helix Forward Upcase Word End"  #k"E" :mode "Normal")

;;; Line motions
(bind-key "Helix Beginning of Line"    #k"0"    :mode "Normal")
(bind-key "Helix End of Line"          #k"$"    :mode "Normal")
(bind-key "Helix First Non-Whitespace" #k"^"    :mode "Normal")
(bind-key "Helix Beginning of Line"    #k"home" :mode "Normal")
(bind-key "Helix End of Line"          #k"end"  :mode "Normal")

;;; g submap
(bind-key "Helix Buffer Start"           #k"g g" :mode "Normal")
(bind-key "Helix Buffer End"             #k"g e" :mode "Normal")
(bind-key "Helix Beginning of Line"      #k"g h" :mode "Normal")
(bind-key "Helix End of Line"            #k"g l" :mode "Normal")
(bind-key "Helix First Non-Whitespace"   #k"g s" :mode "Normal")
(bind-key "Helix Next Line"              #k"g j" :mode "Normal")
(bind-key "Helix Previous Line"          #k"g k" :mode "Normal")
(bind-key "Helix Goto Window Top"        #k"g t" :mode "Normal")
(bind-key "Scroll Window Recenter"       #k"g c" :mode "Normal")
(bind-key "Helix Goto Window Bottom"     #k"g b" :mode "Normal")
(bind-key "Helix Goto Next Buffer"       #k"g n" :mode "Normal")
(bind-key "Helix Goto Previous Buffer"   #k"g p" :mode "Normal")
(bind-key "Helix Goto Column"            #k"g |" :mode "Normal")

;;; Paragraph motions
(bind-key "Helix Forward Paragraph"  #k"}" :mode "Normal")
(bind-key "Helix Backward Paragraph" #k"{" :mode "Normal")

;;; Find-char motions (read next key internally)
(bind-key "Helix Find Char"       #k"f" :mode "Normal")
(bind-key "Helix Till Char"       #k"t" :mode "Normal")
(bind-key "Helix Find Char Back"  #k"F" :mode "Normal")
(bind-key "Helix Till Char Back"  #k"T" :mode "Normal")

;;; G = end of buffer (no prefix) or goto line N (with numeric prefix)
(bind-key "Helix Goto Line" #k"G" :mode "Normal")


;;;; Normal mode -- operators.

(bind-key "Helix Delete"            #k"d" :mode "Normal")
(bind-key "Helix Change"            #k"c" :mode "Normal")
(bind-key "Helix Yank"              #k"y" :mode "Normal")
(bind-key "Helix Paste After"       #k"p" :mode "Normal")
(bind-key "Helix Paste Before"      #k"P" :mode "Normal")
(bind-key "Helix Select Line"              #k"x"       :mode "Normal")  ; extend_line_below
(bind-key "Helix Select Line"              #k"V"       :mode "Normal")  ; shift-v muscle memory
(bind-key "Helix Extend To Line Bounds"    #k"X"       :mode "Normal")  ; extend_to_line_bounds
(bind-key "Helix Select All"               #k"%"       :mode "Normal")
(bind-key "Helix Collapse Selection"       #k";"       :mode "Normal")
(bind-key "Helix Flip Selection"           #k"meta-;"  :mode "Normal")
(bind-key "Helix Join Lines"               #k"J"       :mode "Normal")
(bind-key "Helix Toggle Case"              #k"~"       :mode "Normal")
(bind-key "Helix Replace Char"             #k"r"       :mode "Normal")
(bind-key "Helix Replace With Yanked"      #k"R"       :mode "Normal")
(bind-key "Helix Lowercase"                #k"`"       :mode "Normal")
(bind-key "Helix Uppercase"                #k"meta-`"  :mode "Normal")
(bind-key "Helix Delete No Yank"           #k"meta-d"  :mode "Normal")
(bind-key "Helix Change No Yank"           #k"meta-c"  :mode "Normal")
(bind-key "Helix Shrink To Line Bounds"    #k"meta-x"  :mode "Normal")
(bind-key "Helix Split On Newlines"        #k"meta-s"  :mode "Normal")
(bind-key "Helix Trim Selection"           #k"_"       :mode "Normal")
(bind-key "Helix Ensure Selection Forward" #k"meta-:"  :mode "Normal")
(bind-key "Helix Keep Primary"             #k","       :mode "Normal")
(bind-key "Helix Toggle Comment"           #k"control-c" :mode "Normal")
(bind-key "Helix Increment"                #k"control-a" :mode "Normal")
(bind-key "Helix Decrement"                #k"control-x" :mode "Normal")
(bind-key "Helix Search Word"              #k"*"       :mode "Normal")
(bind-key "Helix Search Selection"         #k"meta-*"  :mode "Normal")
(bind-key "Helix Add Newline Below"        #k"] space" :mode "Normal")
(bind-key "Helix Add Newline Above"        #k"[ space" :mode "Normal")

;;; Indentation (> and < must be backslash-escaped in #k strings)
(bind-key "Indent Region"   #k"\>" :mode "Normal")
(bind-key "Unindent Region" #k"\<" :mode "Normal")


;;;; Normal mode -- undo / redo.

(bind-key "Undo" #k"u"       :mode "Normal")
(bind-key "Redo" #k"U"       :mode "Normal")
(bind-key "Undo" #k"meta-u"  :mode "Normal")   ; Alt-u = earlier (undo tree alias)
(bind-key "Redo" #k"meta-U"  :mode "Normal")   ; Alt-U = later


;;;; Normal mode -- mode transitions.

(bind-key "Enter Select Mode"       #k"v" :mode "Normal")
(bind-key "Helix Insert Before"     #k"i" :mode "Normal")
(bind-key "Helix Append After"      #k"a" :mode "Normal")
(bind-key "Helix Insert Line Start" #k"I" :mode "Normal")
(bind-key "Helix Append Line End"   #k"A" :mode "Normal")
(bind-key "Helix Open Below"        #k"o" :mode "Normal")
(bind-key "Helix Open Above"        #k"O" :mode "Normal")


;;;; Normal mode -- command palette.

;;; : opens the extended command prompt (helix-style command mode).
(bind-key "Extended Command" #k":" :mode "Normal")

;;; C-x T toggles modal editing globally from any mode (insert or normal).
(bind-key "Toggle Modal Editing" #k"control-x T")


;;;; Normal mode -- space leader.

(bind-key "Find File"          #k"space f" :mode "Normal")
(bind-key "Select Buffer"      #k"space b" :mode "Normal")
(bind-key "Save File"          #k"space s" :mode "Normal")
(bind-key "Write File"         #k"space S" :mode "Normal")
(bind-key "Extended Command"   #k"space x" :mode "Normal")
(bind-key "Evaluate Expression" #k"space :" :mode "Normal")
(bind-key "Dired"              #k"space d" :mode "Normal")
(bind-key "Next Window"        #k"space w" :mode "Normal")
(bind-key "Incremental Search" #k"space /" :mode "Normal")
(bind-key "Describe Key"       #k"space ?" :mode "Normal")
(bind-key "Exit Hemlock"       #k"space q" :mode "Normal")


;;;; Normal mode -- search.

(bind-key "Incremental Search"         #k"/" :mode "Normal")
(bind-key "Reverse Incremental Search" #k"?" :mode "Normal")
(bind-key "Search Next"                #k"n" :mode "Normal")
(bind-key "Search Previous"            #k"N" :mode "Normal")


;;;; Normal mode -- count prefix (digits 1-9).

;;; Digit keys feed the prefix count.  "Argument Digit" already handles this.
;;; 0 = beginning of line (helix default; prefix-0 is handled by "Argument Digit"
;;; which ignores 0 as the first digit and falls through -- so we just bind 0 here
;;; and leave the existing Argument Digit binding for 0 via meta-0 in global mode).
(bind-key "Argument Digit" #k"1" :mode "Normal")
(bind-key "Argument Digit" #k"2" :mode "Normal")
(bind-key "Argument Digit" #k"3" :mode "Normal")
(bind-key "Argument Digit" #k"4" :mode "Normal")
(bind-key "Argument Digit" #k"5" :mode "Normal")
(bind-key "Argument Digit" #k"6" :mode "Normal")
(bind-key "Argument Digit" #k"7" :mode "Normal")
(bind-key "Argument Digit" #k"8" :mode "Normal")
(bind-key "Argument Digit" #k"9" :mode "Normal")


;;;; Normal mode -- page scroll.

(bind-key "Helix Scroll Up"   #k"control-b" :mode "Normal")
(bind-key "Helix Scroll Down" #k"control-f" :mode "Normal")
(bind-key "Helix Scroll Up"   #k"pageup"    :mode "Normal")
(bind-key "Helix Scroll Down" #k"pagedown"  :mode "Normal")
(bind-key "Helix Scroll Up"   #k"control-u" :mode "Normal")
(bind-key "Helix Scroll Down" #k"control-d" :mode "Normal")


;;;; Normal mode -- scrolling (z prefix).

(bind-key "Scroll Window Recenter"   #k"z z"    :mode "Normal")
(bind-key "Scroll Window Recenter"   #k"z c"    :mode "Normal")
(bind-key "Helix Goto Window Top"    #k"z t"    :mode "Normal")
(bind-key "Helix Goto Window Bottom" #k"z b"    :mode "Normal")
(bind-key "Scroll Window Down"       #k"z j"    :mode "Normal")
(bind-key "Scroll Window Up"         #k"z k"    :mode "Normal")
(bind-key "Scroll Window Down"       #k"z downarrow" :mode "Normal")
(bind-key "Scroll Window Up"         #k"z uparrow"   :mode "Normal")


;;;; Normal mode -- window management.

;;; C-w prefix is already global in hemlock (window commands).
;;; These just provide convenient aliases within Normal mode.
(bind-key "Next Window"   #k"control-w control-w" :mode "Normal")
(bind-key "Next Window"   #k"control-w w"         :mode "Normal")


;;;; Select mode -- raw motions (extend selection naturally).
;;;
;;; In Select mode, motions move point WITHOUT collapsing the region.
;;; Since hemlock region = [mark, point], moving point with a live mark
;;; automatically extends the selection -- no special logic needed.

;;; h / j / k / l
(bind-key "Forward Character"  #k"l" :mode "Select")
(bind-key "Backward Character" #k"h" :mode "Select")
(bind-key "Next Line"          #k"j" :mode "Select")
(bind-key "Previous Line"      #k"k" :mode "Select")

;;; Arrow keys
(bind-key "Forward Character"  #k"rightarrow"  :mode "Select")
(bind-key "Backward Character" #k"leftarrow"   :mode "Select")
(bind-key "Next Line"          #k"downarrow"   :mode "Select")
(bind-key "Previous Line"      #k"uparrow"     :mode "Select")

;;; Word motions
(bind-key "Forward Word"  #k"w" :mode "Select")
(bind-key "Backward Word" #k"b" :mode "Select")
(bind-key "Forward Word"  #k"e" :mode "Select")

;;; WORD motions
(bind-key "Helix Forward Upcase Word"     #k"W" :mode "Select")
(bind-key "Helix Backward Upcase Word"    #k"B" :mode "Select")
(bind-key "Helix Forward Upcase Word End" #k"E" :mode "Select")

;;; Line motions
(bind-key "Beginning of Line" #k"0"    :mode "Select")
(bind-key "End of Line"       #k"$"    :mode "Select")
(bind-key "Beginning of Line" #k"^"    :mode "Select")
(bind-key "Beginning of Line" #k"home" :mode "Select")
(bind-key "End of Line"       #k"end"  :mode "Select")

;;; Buffer start/end
(bind-key "Buffer Start" #k"g g" :mode "Select")
(bind-key "Buffer End"   #k"G"   :mode "Select")

;;; g submap -- extend variants (raw hemlock commands keep region active)
(bind-key "Buffer End"                   #k"g e" :mode "Select")
(bind-key "Beginning of Line"            #k"g h" :mode "Select")
(bind-key "End of Line"                  #k"g l" :mode "Select")
(bind-key "Helix First Non-Whitespace"   #k"g s" :mode "Select")
(bind-key "Next Line"                    #k"g j" :mode "Select")
(bind-key "Previous Line"                #k"g k" :mode "Select")

;;; Paragraph
(bind-key "Forward Paragraph"  #k"}" :mode "Select")
(bind-key "Backward Paragraph" #k"{" :mode "Select")

;;; Operators in select mode (act on the active region)
(bind-key "Helix Delete"               #k"d"      :mode "Select")
(bind-key "Helix Change"               #k"c"      :mode "Select")
(bind-key "Helix Yank"                 #k"y"      :mode "Select")
(bind-key "Helix Delete No Yank"       #k"meta-d" :mode "Select")
(bind-key "Helix Change No Yank"       #k"meta-c" :mode "Select")
(bind-key "Helix Extend To Line Bounds" #k"X"     :mode "Select")
(bind-key "Helix Select Line"          #k"x"      :mode "Select")
(bind-key "Helix Lowercase"            #k"`"      :mode "Select")
(bind-key "Helix Uppercase"            #k"meta-`" :mode "Select")
(bind-key "Helix Toggle Comment"       #k"control-c" :mode "Select")

;;; Mode transitions from Select
(bind-key "Normal Mode"      #k"v"      :mode "Select")  ; v again = back to Normal
(bind-key "Exit Select Mode" #k"escape" :mode "Select")  ; handled by Helix Escape globally, but explicit here too


;;;; Tree-sitter structural selection (Normal + Select modes).

;;; Alt-o / Alt-i -- expand / shrink to enclosing / child node
(bind-key "Expand Selection"    #k"meta-o" :mode "Normal")
(bind-key "Shrink Selection"    #k"meta-i" :mode "Normal")
(bind-key "Select Next Sibling" #k"meta-n" :mode "Normal")
(bind-key "Select Prev Sibling" #k"meta-p" :mode "Normal")

;;; Same bindings in Select mode so you can keep expanding/navigating
(bind-key "Expand Selection"    #k"meta-o" :mode "Select")
(bind-key "Shrink Selection"    #k"meta-i" :mode "Select")
(bind-key "Select Next Sibling" #k"meta-n" :mode "Select")
(bind-key "Select Prev Sibling" #k"meta-p" :mode "Select")


;;;; Search -- Normal + Select.

(bind-key "Helix Search Word"      #k"*"      :mode "Select")
(bind-key "Helix Search Selection" #k"meta-*" :mode "Select")


;;;; Unimpaired -- [ and ] submaps.

;;; Paragraph navigation
(bind-key "Helix Forward Paragraph"  #k"] p" :mode "Normal")
(bind-key "Helix Backward Paragraph" #k"[ p" :mode "Normal")
(bind-key "Helix Forward Paragraph"  #k"] p" :mode "Select")
(bind-key "Helix Backward Paragraph" #k"[ p" :mode "Select")

;;; Add blank lines -- already bound above in operators section
