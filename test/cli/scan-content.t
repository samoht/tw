Classes are scanned from content files, not only component sources: a docs
site keeps most of its markup in .md/.mdx, and .ts/.js hold class strings
just as .tsx does.

  $ mkdir -p site
  $ cat > site/page.mdx <<EOF
  > # Title
  > <div className="mdx-only:flex p-7" />
  > EOF
  $ cat > site/helper.ts <<EOF
  > export const cls = "m-9";
  > EOF
  $ cat > site/app.tsx <<EOF
  > export const A = () => <div className="p-3" />;
  > EOF

  $ tw --minify site | grep -c '\.p-7{'
  1
  $ tw --minify site | grep -c '\.m-9{'
  1

The formats that already worked keep working:

  $ tw --minify site | grep -c '\.p-3{'
  1

Recursive scans ignore generated/dependency/metadata trees, dotfiles, and
symlinked directories instead of importing stale candidates or following a
cycle:

  $ mkdir -p site/_build site/node_modules site/.hidden external-source
  $ echo '<div class="p-11"></div>' > site/_build/stale.html
  $ echo '<div class="p-12"></div>' > site/node_modules/dependency.html
  $ echo '<div class="p-13"></div>' > site/.hidden/private.html
  $ echo '<div class="p-14"></div>' > site/.dotfile.html
  $ echo '<div class="p-15"></div>' > external-source/external.html
  $ ln -s "$PWD/external-source" site/linked
  $ tw --minify site | grep -oE '\.p-(11|12|13|14|15)\{' | wc -l | tr -d ' '
  0

One unreadable subtree does not abort readable siblings:

  $ mkdir -p site/locked
  $ echo '<div class="p-16"></div>' > site/locked/private.html
  $ chmod 000 site/locked
  $ tw --quiet --minify site 2>/dev/null | grep -c '\.p-3{'
  1
  $ chmod 755 site/locked
