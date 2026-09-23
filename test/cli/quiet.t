A scan reads every token that could be a class, as Tailwind's extractor does,
so most of what it finds in a file is prose or code and not meant as one. A
token no utility reads is left out without a word, whether it is a typo or a
word of text:

  $ cat > page.html <<EOF
  > <div class="flex nonsense-klass">Some words here</div>
  > EOF
  $ tw --minify --no-base page.html 2>&1 >/dev/null

A scan that finds candidates and not one class is the case worth a word, since
it usually means the wrong files were scanned:

  $ cat > prose.html <<EOF
  > <p>hello world</p>
  > EOF
  $ tw --minify --no-base prose.html 2>&1 >/dev/null
  
  --- Statistics ---
  Candidate tokens scanned: 3
  Successfully parsed: 0

`-q` (`--quiet`, `--silent`) suppresses that note, and its help says so:

  $ tw -q --minify --no-base prose.html 2>&1 >/dev/null
  $ tw --help=plain | grep -A4 -- '--quiet$'
         -q, --silent, --quiet
             Suppress the note printed when a scan finds candidate tokens but
             none of them is a class. A scan does not report unknown tokens one
             by one: like Tailwind's, it reads every token that could be a
             class, and most are not meant as one.
