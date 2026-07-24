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
