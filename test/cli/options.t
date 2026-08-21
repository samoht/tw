Backend selectors are mutually exclusive. Asking for both is an input error,
not an implicit preference for one of them:

  $ tw -s flex --tailwind --diff 2>&1 | grep -c 'mutually exclusive'
  1
