(* Probe spans for profiling with [obs run]: one per stage of turning classes
   into a stylesheet. This module exists on the [add-observe] branch only, which
   is never merged: [probe] and [observe] are unreleased. *)

let span name = Probe.span name Probe.Fields.unit
let with_ span f = Probe.with_span span () f
let outputs = span "tw.build.outputs"
let sort = span "tw.build.sort"
let layers = span "tw.build.layers"
let theme_layer = span "tw.build.theme_layer"
let base_layer = span "tw.build.base_layer"
let theme_extract = span "tw.theme.extract"
let theme_static = span "tw.theme.static"
let theme_finish = span "tw.theme.finish"
