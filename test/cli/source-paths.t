An entrypoint's `@source "<path>"` names what to scan, relative to the
stylesheet, the way Tailwind reads it: a directory is walked, a glob matches the
files under its static prefix, and `@source not` takes files back out. A path
that names nothing is skipped, as Tailwind skips it. The entrypoint is enough on
its own; no path on the command line is needed.

  $ mkdir -p css content/sub other
  $ echo '<div class="underline"></div>' > content/a.html
  $ echo '<p class="italic"></p>' > content/sub/b.md
  $ echo '<p class="uppercase"></p>' > other/c.html
  $ echo '<p class="lowercase"></p>' > content/skip.html
  $ cat > css/app.css <<EOF
  > @import "tailwindcss" source(none);
  > @source "../content";
  > @source "../other/*.html";
  > @source not "../content/skip.html";
  > @source "../missing";
  > EOF

  $ tw --minify --input-css css/app.css > out.css
  $ grep -o '\.underline{[^}]*}' out.css
  .underline{text-decoration-line:underline}
  $ grep -o '\.italic{[^}]*}' out.css
  .italic{font-style:italic}
  $ grep -o '\.uppercase{[^}]*}' out.css
  .uppercase{text-transform:uppercase}
  $ grep -c '\.lowercase{' out.css
  0
  [1]

A path on the command line still adds to what the entrypoint names.

  $ mkdir extra
  $ echo '<p class="capitalize"></p>' > extra/d.html
  $ tw --minify --input-css css/app.css extra > both.css
  $ grep -o '\.capitalize{[^}]*}' both.css
  .capitalize{text-transform:capitalize}
  $ grep -o '\.underline{[^}]*}' both.css
  .underline{text-decoration-line:underline}
