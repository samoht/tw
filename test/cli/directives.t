Tailwind's at-rules are build-time input for the generator. Tailwind emits
none of them, so none of them belongs in the CSS a browser gets. An at-rule
that is not one of Tailwind's is the author's own and passes through.

  $ cat > index.html <<EOF
  > <div class="flex prose alpha theme-midnight:flex animate-wiggle"></div>
  > EOF

  $ cat > app.css <<EOF
  > @import "tailwindcss";
  > @tailwind utilities;
  > @source "./src/**/*.html";
  > @plugin "@tailwindcss/typography";
  > @config "./tailwind.config.js";
  > @reference "./ref.css";
  > @theme { --animate-wiggle: wiggle 1s ease-in-out infinite; }
  > @theme inline { --font-mine: Georgia, serif; }
  > @utility alpha { padding: 1rem }
  > @custom-variant theme-midnight (&:where([data-theme="midnight"] *));
  > .page { @apply flex; }
  > @variant sm { .wide { display: grid } }
  > EOF

  $ tw --minify --input-css app.css index.html > out.css

  $ grep -c -- '@theme' out.css
  0
  [1]
  $ grep -c -- '@source' out.css
  0
  [1]
  $ grep -c -- '@plugin' out.css
  0
  [1]
  $ grep -c -- '@config' out.css
  0
  [1]
  $ grep -c -- '@reference' out.css
  0
  [1]
  $ grep -c -- '@tailwind' out.css
  0
  [1]
  $ grep -c -- '@utility' out.css
  0
  [1]
  $ grep -c -- '@custom-variant' out.css
  0
  [1]
  $ grep -c -- '@apply' out.css
  0
  [1]
  $ grep -c -- '@variant' out.css
  0
  [1]
  $ grep -c -- '@slot' out.css
  0
  [1]

Removing the directive is not dropping what it declared: the theme token, the
typography plugin, the declared utility, the declared variant, the applied
rule and the built-in variant all reach the output they belong in.

  $ grep -cF -- '--animate-wiggle:wiggle 1s ease-in-out infinite' out.css
  1
  $ grep -cF '.prose' out.css
  1
  $ grep -cF '.alpha{padding:1rem}' out.css
  1
  $ grep -cF '.theme-midnight\:flex:where([data-theme=midnight] *){display:flex}' out.css
  1
  $ grep -cF '.page{display:flex}' out.css
  1
  $ grep -cF '@media(min-width:40rem){.wide{display:grid}}' out.css
  1

A directive that declares nothing is still a directive. [@utility] with no
name, [@custom-variant] with no selector, [@variant] naming a variant tw does
not know and [@theme] with no block each leave the generator nothing to read,
and a browser even less:

  $ cat > degenerate.css <<EOF
  > @import "tailwindcss";
  > @utility { color: red }
  > @custom-variant novariant;
  > @variant not-a-variant { .q { color: blue } }
  > @theme;
  > EOF
  $ tw --minify --input-css degenerate.css index.html > degenerate.out
  $ grep -c -- '@utility' degenerate.out
  0
  [1]
  $ grep -c -- '@custom-variant' degenerate.out
  0
  [1]
  $ grep -c -- '@variant' degenerate.out
  0
  [1]
  $ grep -c -- '@theme' degenerate.out
  0
  [1]

An at-rule Tailwind never defined is the author's CSS. tw has no more claim on
it than on any rule the project wrote, so it comes out the way it went in:

  $ cat > third-party.css <<EOF
  > @import "tailwindcss";
  > @unknown-directive "keep me";
  > @unknown-block { .kept { color: red } }
  > EOF
  $ tw --minify --input-css third-party.css index.html > third-party.out
  $ grep -cF '@unknown-directive "keep me"' third-party.out
  1
  $ grep -cF '@unknown-block' third-party.out
  1
  $ grep -cF '.kept' third-party.out
  1
