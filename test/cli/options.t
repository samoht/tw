Backend selectors are mutually exclusive. Asking for both is an input error,
not an implicit preference for one of them:

  $ tw -s flex --tailwind --diff 2>&1 | grep -c 'mutually exclusive'
  1

Optimization must preserve public custom-property declarations. They are a
runtime API even when no declaration in the generated sheet reads them:

  $ cat > custom-property.html <<EOF
  > <div class="[--anchor-gap:8px]"></div>
  > EOF
  $ tw --no-base --minify --optimize custom-property.html | grep -o -- '--anchor-gap:8px'
  --anchor-gap:8px

Authored CSS gets the same compatibility prefixes as generated utilities,
even when the rest of the optimizer is disabled:

  $ tw --quiet --no-base --minify --input-css prefixes.css prefixes.html
  .a{-webkit-user-select:none;user-select:none}@supports(-webkit-backdrop-filter:blur(0))or (backdrop-filter:blur(0)){.b{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}}

Inline mode exists to resolve the variables away. [--spacing] is the one theme
token tw marks as a runtime override point, which keeps the default output at
Tailwind's [calc(var(--spacing) * 4)]; under [--inline] that reference folds
like any other, so the utility carries the resolved length and no theme block
is left to read:

  $ tw --single=p-4 --inline --no-base --minify
  .p-4{padding:1rem}
