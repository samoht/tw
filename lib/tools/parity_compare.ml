let diff ?mode expected actual =
  Cascade_diff.Css_compare.diff ?mode expected actual
