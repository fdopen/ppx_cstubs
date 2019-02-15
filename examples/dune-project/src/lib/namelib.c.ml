let%c () = header "#include <glib.h>"

external real_name : void -> string_opt = "g_get_real_name"
