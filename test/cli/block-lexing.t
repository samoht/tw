A brace inside a string, a comment or an escape is not a block delimiter.
Counting braces over the raw entrypoint text ends a block in the wrong place,
so the declarations that follow it are silently dropped.

[plain.html] names no declared variant, so nothing the entrypoint declares is
re-emitted and a rule missing from the output is really missing. [index.html]
applies the declared variant, which is what shows the body was sliced right.

  $ cat > plain.html <<EOF
  > <div class="flex"></div>
  > EOF
  $ cat > index.html <<EOF
  > <div class="weird:flex"></div>
  > EOF

A [}] in a string ends the [@custom-variant] body early, taking the rule after
it along:

  $ cat > str.css <<EOF
  > @import "tailwindcss";
  > @custom-variant weird { &:hover { content: "}"; @slot; } }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css str.css plain.html | grep -cF '.after{color:blue}'
  1

The variant still applies, and its own declaration keeps the brace:

  $ tw --minify --input-css str.css index.html | grep -cF '.weird\:flex:hover{content:"}"}'
  1
  $ tw --minify --input-css str.css index.html | grep -cF '.weird\:flex:hover{display:flex}'
  1

The control, the same entrypoint with a brace-free string:

  $ cat > ctrl.css <<EOF
  > @import "tailwindcss";
  > @custom-variant weird { &:hover { content: "x"; @slot; } }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css ctrl.css plain.html | grep -cF '.after{color:blue}'
  1
  $ tw --minify --input-css ctrl.css index.html | grep -cF '.weird\:flex:hover{content:"x"}'
  1

A comment is nothing to a CSS parser, so a [{] written in one opens no block:

  $ cat > cmt.css <<EOF
  > @import "tailwindcss";
  > @custom-variant weird { /* { */ &:hover { @slot; } }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css cmt.css plain.html | grep -cF '.after{color:blue}'
  1
  $ tw --minify --input-css cmt.css index.html | grep -cF '.weird\:flex:hover{display:flex}'
  1

An escaped quote does not end the string, so the [}] after it is still inside
it:

  $ cat > esc.css <<EOF
  > @import "tailwindcss";
  > @custom-variant weird { &:hover { content: "\"}"; @slot; } }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css esc.css plain.html | grep -cF '.after{color:blue}'
  1
  $ tw --minify --input-css esc.css index.html | grep -cF '.weird\:flex:hover{display:flex}'
  1

An escaped brace in a selector is part of the identifier, not a delimiter:

  $ cat > sel.css <<EOF
  > @import "tailwindcss";
  > @custom-variant weird { .a\}b & { @slot; } }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css sel.css plain.html | grep -cF '.after{color:blue}'
  1

[@utility] bodies are pulled out the same way:

  $ cat > util.html <<EOF
  > <div class="weird"></div>
  > EOF
  $ cat > util.css <<EOF
  > @import "tailwindcss";
  > @utility weird { content: "}"; padding: 1px }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css util.css util.html | grep -cF '.weird{content:"}";padding:1px}'
  1
  $ tw --minify --input-css util.css util.html | grep -cF '.after{color:blue}'
  1

So is the [@variant] block author CSS applies inside a rule:

  $ cat > var.css <<EOF
  > @import "tailwindcss";
  > .box { @variant dark { content: "}" } }
  > .after { color: blue; }
  > EOF
  $ tw --minify --input-css var.css plain.html | grep -cF '@media(prefers-color-scheme:dark){.box{content:"}"}}'
  1
  $ tw --minify --input-css var.css plain.html | grep -cF '.after{color:blue}'
  1

[theme()] and [--spacing()] are function calls, not text: one written inside a
string is part of the string and stays as the author wrote it.

  $ cat > fn.css <<EOF
  > @import "tailwindcss";
  > .u { content: "theme(--breakpoint-sm)" }
  > .v { content: "--spacing(4)" }
  > EOF
  $ tw --minify --input-css fn.css plain.html | grep -cF '.u{content:"theme(--breakpoint-sm)"}'
  1
  $ tw --minify --input-css fn.css plain.html | grep -cF '.v{content:"--spacing(4)"}'
  1

Outside a string they still resolve:

  $ cat > fn2.css <<EOF
  > @import "tailwindcss";
  > @media (width <= theme(--breakpoint-sm)) { .b { color: red } }
  > .g { margin-left: --spacing(6) }
  > EOF
  $ tw --minify --input-css fn2.css plain.html | grep -cF '@media(width<=40rem){.b{color:red}}'
  1
  $ tw --minify --input-css fn2.css plain.html | grep -cF '.g{margin-left:calc(var(--spacing)*6)}'
  1
