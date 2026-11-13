(** Tailwind v4's default colour palette.

    Each family is the OKLCh value Tailwind's [\@theme] declares for every shade
    from 50 to 950, as [--color-<family>-<shade>]. [black] and [white] are no
    family: they are single colours, with no shade. *)

open Srgb

(** {1 Families} *)

val gray : (int * oklch) list
(** [gray] is the gray family, shade by shade. *)

val slate : (int * oklch) list
(** [slate] is the slate family, shade by shade. *)

val zinc : (int * oklch) list
(** [zinc] is the zinc family, shade by shade. *)

val neutral : (int * oklch) list
(** [neutral] is the neutral family, shade by shade. *)

val stone : (int * oklch) list
(** [stone] is the stone family, shade by shade. *)

val mauve : (int * oklch) list
(** [mauve] is the mauve family, shade by shade. *)

val olive : (int * oklch) list
(** [olive] is the olive family, shade by shade. *)

val mist : (int * oklch) list
(** [mist] is the mist family, shade by shade. *)

val taupe : (int * oklch) list
(** [taupe] is the taupe family, shade by shade. *)

val red : (int * oklch) list
(** [red] is the red family, shade by shade. *)

val orange : (int * oklch) list
(** [orange] is the orange family, shade by shade. *)

val amber : (int * oklch) list
(** [amber] is the amber family, shade by shade. *)

val yellow : (int * oklch) list
(** [yellow] is the yellow family, shade by shade. *)

val lime : (int * oklch) list
(** [lime] is the lime family, shade by shade. *)

val green : (int * oklch) list
(** [green] is the green family, shade by shade. *)

val emerald : (int * oklch) list
(** [emerald] is the emerald family, shade by shade. *)

val teal : (int * oklch) list
(** [teal] is the teal family, shade by shade. *)

val cyan : (int * oklch) list
(** [cyan] is the cyan family, shade by shade. *)

val sky : (int * oklch) list
(** [sky] is the sky family, shade by shade. *)

val blue : (int * oklch) list
(** [blue] is the blue family, shade by shade. *)

val indigo : (int * oklch) list
(** [indigo] is the indigo family, shade by shade. *)

val violet : (int * oklch) list
(** [violet] is the violet family, shade by shade. *)

val purple : (int * oklch) list
(** [purple] is the purple family, shade by shade. *)

val fuchsia : (int * oklch) list
(** [fuchsia] is the fuchsia family, shade by shade. *)

val pink : (int * oklch) list
(** [pink] is the pink family, shade by shade. *)

val rose : (int * oklch) list
(** [rose] is the rose family, shade by shade. *)

(** {1 Lookup} *)

val names : string list
(** [names] is every colour the palette defines, in the order Tailwind's
    [\@theme] declares them: the shaded families, the four v4.3.3 added after
    {!val-stone}, then [black] and [white]. *)

val shades : int list
(** [shades] is the shades every family defines, from 50 to 950. *)

val oklch : string -> int -> oklch option
(** [oklch name shade] is the OKLCh value of family [name] at [shade], or [None]
    when the palette has no such family or shade. [name] is read
    case-insensitively. *)

val oklch_css : string -> int -> string option
(** [oklch_css name shade] is {!oklch} spelled for CSS, as {!Srgb.oklch_to_css}
    writes it. *)
