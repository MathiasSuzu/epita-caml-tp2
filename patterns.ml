let build_line n str =
  if n < 0 then invalid_arg "build n doit etre  positif" else
    if n = 0 then "" else
  let rec build_rc n =
    if n = 0 then ""  else
      build_rc (n-1) ^ str
 in build_rc n ;;


let square n str =
  if n <= 0 then invalid_arg "square n doit etre stritement positif" else
  let line = build_line n str in
  let rec square_rec n =
    if n = -1 then
      ()
    else
      begin
        print_string line;
        print_newline ();
        square_rec (n-1);
      end
  in square_rec n ;;



let square2 n (a,b)  =
  if n <= 0 then invalid_arg "square2 n doit etre stritement positif" else
  let str = a ^ b in
  square n str;;


let triangle n str =
  if n <= 0 then invalid_arg "tri n doit etre stritement positif" else
  let rec triangle_rec n =
    if n = 0 then () else
      begin
        triangle_rec(n-1);
        print_string (build_line n str);
        print_newline ();
      end
  in triangle_rec n;;


let pyramid n (a,b) =
  if n <= 0 then invalid_arg "pyramid n doit etre stritement positif"
  else
    let rec pyra_rec i =
       if i = 0 then () else
      begin
        pyra_rec(i-1);
        print_string (build_line (n-i) b);
        print_string  (build_line i a);
        print_string  (build_line i a);
        print_string (build_line (n-i) b);
        print_newline ();
      end
    in pyra_rec n;;


let cross n (a,b) =
  if n <= 0 then invalid_arg "cross n doit etre stritement positif" else
    let rec cross_rec i = match i with
      i when i > 1         -> print_string (build_line (n-i) a);
                              print_string b;
                              print_string (build_line (i-1) a);
                              print_string (build_line (i-2) a);
                              print_string b;
                              print_string (build_line (n-i) a);
                              print_newline ();
                              cross_rec(i-1)

    | i when i = 1         ->
                              print_string (build_line (n-1) a);
                              print_string b;
                              print_string (build_line (n-1) a);
                              print_newline ();
                              cross_rec(i-2);


    | i when (i*(-1)) < n  ->
                              cross_rec(i-1);
                              print_string (build_line ((i*(-1)) -1) a);
                              print_string b;
                              print_string (build_line (n+i) a);
                              print_string (build_line (n+i-1) a);
                              print_string b;
                              print_string (build_line ((i*(-1)) -1) a);
                              print_newline ()

    | _                    -> ()
    in cross_rec n;;