(** The shape of the spliced sheet.

    Once the generated sheet stands at the entrypoint's [\@import], the author's
    [\@apply]s and the generated sheet have each brought their own theme block,
    [\@layer properties] fallbacks and [\@property] rules, and an imported file
    may fill a layer slot declared much earlier. Tailwind writes each of those
    once, in a fixed place. *)

val tidy :
  theme:Tw.Scheme.t -> Cascade.Css.statement list -> Cascade.Css.statement list
(** [tidy ~theme stmts] gives the spliced statements the shape Tailwind's output
    has. What an imported file's expansion hoisted leaves its layer; each named
    layer is written once, its block in the slot its first declaration holds and
    [\@layer properties] ahead of the rest, with the fallback blocks and the
    theme rules inside joined into one; a token [theme] declares inline and
    nothing reads goes; and the [\@property] rules, one per name, then the
    [\@keyframes], close the sheet. *)
