open Scanf
open Printf
exception Entrada_invalida of string;;

let num = read_int();;
if(num<0|| num>100) then raise (Entrada_invalida "N tem de ser um valor entre 0 e 100");;
let lista = [];;

let transforma = function "(" -> "PE" | ")" -> "PD" |"&"-> "O"|"|"-> "O"|"->"-> "O"|"<->"-> "O"|"!"->"N"|_->"E";;


let debugging lista=
match lista with
| "PD"::"E"::"O":: "E"::"PE"::resto-> "E"::resto
| "PD"::"E"::"PE"::"N"::resto->"E"::resto
| "PD"::"E"::"PE"::resto->"E"::resto
| "E"::"N"::resto->"E"::resto
| "E"::"O"::"E"::resto -> "E"::resto
| "PD"::"E"::"N"::"PE"::resto-> "E"::resto
|_->lista;;

let rec simplifica num lista=

  if num == 0 then lista else begin


 simplifica (num-1) (debugging ((transforma (read_line()))::lista))

end;;

if((simplifica num lista)=["E"]) then print_string "YES\n" else print_string "NO\n";
