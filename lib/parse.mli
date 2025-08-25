val int_pos : name:string -> string -> (int, [> `Msg of string ]) result

val int_bounded :
  name:string ->
  min:int ->
  max:int ->
  string ->
  (int, [> `Msg of string ]) result

val int_any : string -> (int, [> `Msg of string ]) result
val ( >|= ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
