
let do_debug = true;;

let dbg msg =
	if do_debug
	then(
		print_string msg;
		flush std_out
		)
;;

let dbgbgn fnc =
  dbg ("entering function: " ^ fnc ^ ".\n")
;;

let dbgend fnc =
  dbg ("exiting function " ^ fnc ^ ".\n")
;;
