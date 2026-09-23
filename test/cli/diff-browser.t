`--diff --html FILE` renders tw's sheet and Tailwind's over FILE in a headless
Chromium, beside the canonical comparison. The document is what the browser
renders, so `--html` means nothing without `--diff`:

  $ echo '<div class="m-2"></div>' > page.html
  $ tw -s p-4 --html page.html
  Usage: tw [--help] [OPTION]… [PATH]…
  tw: --html renders the two sheets --diff compares; it needs --diff
  [124]

A class the document does not carry would be compared on no element, which a
browser run would report as agreement. It is refused, before either sheet is
compiled:

  $ tw -s p-4 --diff --html page.html
  Error: no element of page.html carries p-4, so the browser would compare nothing for it
  [2]

`--html` is a path like `-i`, so under `--cwd` it is read against that
directory rather than the one tw was started from:

  $ mkdir -p proj/templates
  $ echo '<div class="m-2"></div>' > proj/templates/index.html
  $ tw --cwd proj -s p-4 --diff --html templates/index.html
  Error: no element of templates/index.html carries p-4, so the browser would compare nothing for it
  [2]

A document that is not there is reported the way a missing `-i` is:

  $ tw --cwd proj -s p-4 --diff --html templates/missing.html
  Usage: tw [--help] [OPTION]… [PATH]…
  tw: no 'templates/missing.html' file or directory
  [124]
