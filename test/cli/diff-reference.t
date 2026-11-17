`--diff` compares tw's sheet with the one the pinned tailwindcss CLI compiles
before lightningcss minifies it. Minifying, lightningcss 1.32.0 writes
Preflight's `-webkit-text-decoration: inherit` on `a` three times; that is the
minifier's output, not Tailwind's, so it is no difference of tw's:

  $ tw -s flex --diff --diff-mode=tree | grep -c -- -webkit-text-decoration
  0
  [1]

`--minify` asks both tools to minify, so the comparison then includes
lightningcss's output, duplicate and all:

  $ tw -s flex --diff --minify --diff-mode=tree | grep -c -- -webkit-text-decoration
  2
