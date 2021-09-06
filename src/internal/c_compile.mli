val compile :
  ?stdout:Run.io_out ->
  stderr:Run.io_out ->
  string ->
  (int -> string -> 'a) ->
  'a
