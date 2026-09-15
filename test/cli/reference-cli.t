The reference CLI is identified by compiling a probe entrypoint, since it has
no version flag: [--version] compiles a stylesheet like any other run. A CLI
of the right version can still fail that probe, because [@import "tailwindcss"]
resolves against the nearest [node_modules] above the entrypoint and a global
install of the CLI carries no such tree, so a run from a directory with no
project around it fails. The report has to say that the CLI answered, and
where the probe was written, rather than blame the version. A stub stands in
for such a CLI: it names itself on [--help] and compiles nothing.

  $ mkdir -p node_modules/.bin
  $ cat > node_modules/.bin/tailwindcss <<'EOF'
  > #!/bin/sh
  > case "$*" in
  >   *--help*) echo "= tailwindcss v4.3.3"; exit 0 ;;
  > esac
  > exit 1
  > EOF
  $ chmod +x node_modules/.bin/tailwindcss

The report is wrapped for the terminal, so it is read unwrapped:

  $ tw --tailwind -s flex 2>&1 | tr '\n' ' ' | tr -s ' ' > report.txt
  $ grep -o 'node_modules/.bin/tailwindcss: [^,]*' report.txt
  node_modules/.bin/tailwindcss: v4.3.3 by its --help banner
  $ grep -c 'but it did not compile the probe entrypoint under' report.txt
  1

A global install of the right version is not what is missing, so the advice
is to run from a project rather than to install one:

  $ grep -c 'npm install -g' report.txt
  0
  [1]
  $ grep -o 'Run tw from a project with tailwindcss@4.3.3 installed' report.txt
  Run tw from a project with tailwindcss@4.3.3 installed
