(** Fuzz tests for the CSS Values module.

    Tests crash safety of value parsers (colors, lengths, angles, etc.) and
    roundtrip consistency for pretty-printed values. *)

open Crowbar

(** read_color — must not crash on arbitrary input. *)
let test_read_color buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_color r) with Css.Reader.Parse_error _ -> ()

(** read_length — must not crash. *)
let test_read_length buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_length r) with Css.Reader.Parse_error _ -> ()

(** read_angle — must not crash. *)
let test_read_angle buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_angle r) with Css.Reader.Parse_error _ -> ()

(** read_duration — must not crash. *)
let test_read_duration buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_duration r) with Css.Reader.Parse_error _ -> ()

(** read_time — must not crash (can be negative). *)
let test_read_time buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_time r) with Css.Reader.Parse_error _ -> ()

(** read_number — must not crash. *)
let test_read_number buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_number r) with Css.Reader.Parse_error _ -> ()

(** read_percentage — must not crash. *)
let test_read_percentage buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_percentage r)
  with Css.Reader.Parse_error _ -> ()

(** read_length_percentage — must not crash. *)
let test_read_length_percentage buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_length_percentage r)
  with Css.Reader.Parse_error _ -> ()

(** read_number_percentage — must not crash. *)
let test_read_number_percentage buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_number_percentage r)
  with Css.Reader.Parse_error _ -> ()

(** read_color_name — must not crash. *)
let test_read_color_name buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_color_name r)
  with Css.Reader.Parse_error _ -> ()

(** read_color_space — must not crash. *)
let test_read_color_space buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_color_space r)
  with Css.Reader.Parse_error _ -> ()

(** read_system_color — must not crash. *)
let test_read_system_color buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_system_color r)
  with Css.Reader.Parse_error _ -> ()

(** read_hue — must not crash. *)
let test_read_hue buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_hue r) with Css.Reader.Parse_error _ -> ()

(** read_alpha — must not crash. *)
let test_read_alpha buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_alpha r) with Css.Reader.Parse_error _ -> ()

(** read_hue_interpolation — must not crash. *)
let test_read_hue_interpolation buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_hue_interpolation r)
  with Css.Reader.Parse_error _ -> ()

(** read_calc — must not crash (with read_length as inner parser). *)
let test_read_calc buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_calc Css.Values.read_length r)
  with Css.Reader.Parse_error _ -> ()

(** read_channel — must not crash. *)
let test_read_channel buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_channel r) with Css.Reader.Parse_error _ -> ()

(** read_component — must not crash. *)
let test_read_component buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_component r) with Css.Reader.Parse_error _ -> ()

(** read_rgb — must not crash. *)
let test_read_rgb buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_rgb r) with Css.Reader.Parse_error _ -> ()

(** read_transition_behavior — must not crash. *)
let test_read_transition_behavior buf =
  let r = Css.Reader.of_string buf in
  try ignore (Css.Values.read_transition_behavior r)
  with Css.Reader.Parse_error _ -> ()

(** Color roundtrip: parse → pp → parse should not crash. *)
let test_color_roundtrip buf =
  let r = Css.Reader.of_string buf in
  match
    try Some (Css.Values.read_color r) with Css.Reader.Parse_error _ -> None
  with
  | None -> ()
  | Some color -> (
      let s = Css.Pp.to_string Css.Values.pp_color color in
      let r2 = Css.Reader.of_string s in
      try ignore (Css.Values.read_color r2)
      with Css.Reader.Parse_error _ -> fail "color roundtrip re-parse failed")

let suite () =
  add_test ~name:"values: read_color crash safety" [ bytes ] test_read_color;
  add_test ~name:"values: read_length crash safety" [ bytes ] test_read_length;
  add_test ~name:"values: read_angle crash safety" [ bytes ] test_read_angle;
  add_test ~name:"values: read_duration crash safety" [ bytes ]
    test_read_duration;
  add_test ~name:"values: read_time crash safety" [ bytes ] test_read_time;
  add_test ~name:"values: read_number crash safety" [ bytes ] test_read_number;
  add_test ~name:"values: read_percentage crash safety" [ bytes ]
    test_read_percentage;
  add_test ~name:"values: read_length_percentage crash safety" [ bytes ]
    test_read_length_percentage;
  add_test ~name:"values: read_number_percentage crash safety" [ bytes ]
    test_read_number_percentage;
  add_test ~name:"values: read_color_name crash safety" [ bytes ]
    test_read_color_name;
  add_test ~name:"values: read_color_space crash safety" [ bytes ]
    test_read_color_space;
  add_test ~name:"values: read_system_color crash safety" [ bytes ]
    test_read_system_color;
  add_test ~name:"values: read_hue crash safety" [ bytes ] test_read_hue;
  add_test ~name:"values: read_alpha crash safety" [ bytes ] test_read_alpha;
  add_test ~name:"values: read_hue_interpolation crash safety" [ bytes ]
    test_read_hue_interpolation;
  add_test ~name:"values: read_calc crash safety" [ bytes ] test_read_calc;
  add_test ~name:"values: read_channel crash safety" [ bytes ] test_read_channel;
  add_test ~name:"values: read_component crash safety" [ bytes ]
    test_read_component;
  add_test ~name:"values: read_rgb crash safety" [ bytes ] test_read_rgb;
  add_test ~name:"values: read_transition_behavior crash safety" [ bytes ]
    test_read_transition_behavior;
  add_test ~name:"values: color roundtrip" [ bytes ] test_color_roundtrip
