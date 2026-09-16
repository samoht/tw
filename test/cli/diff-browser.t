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
