# use "topfind" ;;  (*Loads the find liblibrary *)
# require "graphics" ;;  (* Load the library *)

open Graphics ;;
open_graph " 1200x800" ;;


let draw_line (x , y ) (z , t ) =
  moveto x y ;
  lineto z t;;

let mountain n (x,y) (z,t) =
  clear_graph ();
  let rec mountain_rec n (x,y) (z,t) =
    if n = 0 then draw_line (x, y) (z,t) else
      let r = Random.int(10*n) in
        begin
        mountain_rec (n-1) (x,y) ((x+z)/2 , (y+t)/2+r);
        mountain_rec (n-1) ((x+z)/2 , (y+t)/2+r) (z, t);
        end
  in mountain_rec n (x,y) (z,t);;

let dragon n (x,y) (z,t) =
  clear_graph ();
  let rec dra_rec n (x,y) (z,t) =
    if n = 0 then draw_line (x, y) (z,t) else
      let r = ((x + z)/2 + (t - y)/2, (y + t)/2 -(z - x)/2) in
      begin
        dra_rec (n-1) (x,y) r;
        dra_rec (n-1) (z,t) r
      end
  in dra_rec n (x,y) (z,t);;

let carpet n (x,y) =
    clear_graph();
    let rec car_rec n x y = 
      if n = 1 then fill_rect x y n n else
        let n = n/3 in
          begin
            car_rec n (x) y;
            car_rec n (x+n) y;
            car_rec n (x+n+n) y;
            car_rec n (x+n+n) (y+n);
            car_rec n (x+n+n) (y+n+n);
            car_rec n (x+n) (y+n+n);
            car_rec n x (y+n+n);
            car_rec n x (y+n);
          end
    in car_rec n x y;;


let sierpinski n (x , y) size =
  clear_graph ();
  let draw_tri_equi (x, y) (u, v) (z,t) =
    draw_line (x , y) (u, v);
    draw_line (u, v) (z,t);
    draw_line (z, t) (x, y) in 
  let rec sier_rec n (x , y) size =
    let h = (int_of_float ((float_of_int(size)*.(sqrt (3.)/.2.))*.10.))/10 in
    if n = 0 then draw_tri_equi (x, y) (x+size/2, y+h) (x+size,y) else
     let size = size/2 in
     let h = (int_of_float ((float_of_int(size)*.(sqrt (3.)/.2.))*.10.))/10 in
     begin
      sier_rec (n-1) (x , y) size ;
      sier_rec (n-1) (x+size, y) size;
      sier_rec (n-1) (x+size/2,y+h) size;
     end
  in sier_rec n (x , y) size;;
  

let four_circles r (x, y) limit =
  clear_graph ();
  let rec circ_rec r (x, y) =
    if r < limit then () else
      begin
        draw_circle x y r;
        circ_rec (r/2) (x-r/2, y);
        circ_rec (r/2) (x+r/2, y);
        circ_rec (r/2) (x, y-r/2);
        circ_rec (r/2) (x, y+r/2);
      end
 in circ_rec r (x, y) ;;


let arrow r (x, y) o limit =
  clear_graph ();
  let rec arrow_rec x y r o =
    if r <= limit then () else
    begin
    fill_circle x y r;
    match o with
    | 'N' | 'n' -> begin
              arrow_rec x (y+r+r/2) (r/2) 'N';
              arrow_rec (x-r-r/2) y (r/2) 'W';
              arrow_rec (x+r+r/2) y (r/2) 'E';
             end

    | 'E' | 'e' -> begin
              arrow_rec x (y+r+r/2) (r/2) 'N';
              arrow_rec (x+r+r/2) y (r/2) 'E';
              arrow_rec x (y-r-r/2) (r/2) 'S';
             end

    | 'S' | 's' -> begin
              arrow_rec x (y-r-r/2) (r/2) 'S';
              arrow_rec (x-r-r/2) y (r/2) 'W';
              arrow_rec (x+r+r/2) y (r/2) 'E';
             end

    | 'W' | 'w' -> begin
              arrow_rec x (y+r+r/2) (r/2) 'N';
              arrow_rec (x-r-r/2) y (r/2) 'W';
              arrow_rec x (y-r-r/2) (r/2) 'S';
             end

    |  _  -> invalid_arg ("arrow : l'orientation n'est pas bonne")
    end
  in arrow_rec x y r o;;
  

let pytagora_tree n (x , y) size =
  clear_graph ();
  let draw_square (x , y) (u, v) (r, w) (z, t) =
    begin
    draw_line (x, y) (u, v);
    draw_line (u, v) (r, w);
    draw_line (r, w) (z, t);
    draw_line (z, t) (x, y);
    end 
    and arr n =
      let dixieme = (int_of_float(n*.10.) mod 10) 
      and ni = int_of_float(n) in
      if dixieme >= 5 then ni + 1 else ni in
      
    let rec pyta  n (x , y) (u, v) (r, w) (z, t) =
      if n=0 then draw_square (x , y) (u, v) (r, w) (z, t) else 
        
        let px = arr (float_of_int (r + z)/.2. +. float_of_int (t - w)/.2.)
        and py = arr (float_of_int(w + t)/.2. -. float_of_int(z - r)/.2.) in

          begin
            draw_square (x , y) (u, v) (r, w) (z, t);
            pyta (n-1) (z, t) (px, py) (px*2-r, py*2-w) (z+px-r, t+py-w);
            pyta (n-1) (px, py) (r, w) (r - z + px, w - t + py) (px*2-z, py*2-t);
          end

        in pyta (n-1) (x, y) (x+size,y) (x+size,y+size) (x,y+size);;


let vicsek_star n (x,y) size =
  clear_graph ();
  let rec vicsek n x y size =
    if n=0 then fill_rect x y size size else
      let size=size/3 in
      begin
        vicsek (n-1) x y size;
        vicsek (n-1) (x+size*2) (y) size;
        vicsek (n-1) (x+size) (y+size) size;
        vicsek (n-1) (x+size*2) (y+size*2) size;
        vicsek (n-1) (x) (y+size*2) size;
      end
  in vicsek n x y size;;


let rec koch_curve n (x,y) (z,t) =
  if n=0 then draw_line (x,y) (z,t) else
    let u0 = x+(z-x)/3 and 
        v0 = y+(t-y)/3 and
        u1 = x+(z-x)*2/3 and
        v1 = y+(t-y)*2/3 in 

    let u = (u0+u1)/2 + (int_of_float(sqrt(3.) *.float_of_int(v0-v1)))/2 and
        v = (v0+v1)/2 - (int_of_float(sqrt(3.) *.float_of_int(u0-u1)))/2 in 
    
    begin
      koch_curve (n-1) (x,y) (u0,v0);
      koch_curve (n-1) (u0,v0) (u,v);
      koch_curve (n-1) (u,v) (u1,v1);
      koch_curve (n-1) (u1,v1) (z,t); 
    end;;


let koch_snowflake n (x,y) d =
  clear_graph ();
  let angle = (sqrt(3.)/.2.) in 
  let rec koch n x y d =
    if n=0 then () else
      let h = int_of_float ((float_of_int(d)*.angle)) in
      begin
      koch_curve n (x+d,y) (x,y);
      koch_curve n (x,y) (x+d/2,y+h);
      koch_curve n (x+d/2,y+h) (x+d,y);
      end
  in koch n x y d;;