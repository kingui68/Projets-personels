
let rec gen n =
  if( n=0 )
  then(
    "1"
  )
  else(
    "(+ 1 " ^ (gen (n-1)) ^ ")";
  )
;;
  
