
#open "compiler";;
#open "file";;

let dbg s =
	print_string s;
	flush std_out
;;

let main argv =
	let bgn_time =sys__time () in

	let inFileName = getInFileName argv in
	let outFileName= getOutFileName argv in

	let inSrc = readWholeFile inFileName in
	let outSrc = compiler inSrc in

	let dur = (sys__time ()) -. bgn_time in
	print_string "Durée de compilation: ";
	print_string (string_of_float dur);
	print_string "\n";
	flush std_out;

	writeToFile outFileName outSrc;
;;

main (list_of_vect sys__command_line);;

