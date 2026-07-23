The project's CSS entrypoint is compiled, not just read for its @theme:
its own rules and its relative @imports belong in the output, and
[@import "tailwindcss"] marks where the generated sheet goes.

  $ cat > sidebar.css <<EOF
  > .sidebar { color: red }
  > EOF

  $ cat > app.css <<EOF
  > @import "tailwindcss" theme(static);
  > @import "./sidebar.css";
  > .page { display: grid }
  > EOF

  $ cat > index.html <<EOF
  > <div class="flex"></div>
  > EOF

The entrypoint's own rule, and the rule it imports, both survive:

  $ tw --minify --input-css app.css index.html | grep -c '\.page{display:grid}'
  1
  $ tw --minify --input-css app.css index.html | grep -c '\.sidebar{color:red}'
  1

The generated utility is spliced in where the package import sat:

  $ tw --minify --input-css app.css index.html | grep -c '\.flex{display:flex}'
  1
