(*

type token = CELL_INCREMENT 
            |CELL_DECREMENT
            |VALUE_INCREMENT
            |VALUE_DECREMENT
            |BEGIN_LOOP
            |END_LOOP
            |INPUT
            |OUTPUT
            |DEBUG_COMMAND

let lexerTransform = function 
    |'>'->CELL_INCREMENT 
    |'<'->CELL_DECREMENT
    |'+'->VALUE_INCREMENT
    |'-'->VALUE_DECREMENT
    |'['->BEGIN_LOOP
    |']'->END_LOOP
    |','->INPUT
    |'.'->OUTPUT
    |_->DEBUG_COMMAND

(*This can be faster *)
let lexer code=
    let lista = ref [] in
    String.iter (fun x -> lista := (lexerTransform x) :: !lista) code;
    List.rev !lista;

let interpreter commands=
    
    let interRec command currentCell =
        match commands with
                CELL_INCREMENT -> interRec commands tape (currentCell + 1);
                |CELL_DECREMENT -> interRec commands tape (currentCell - 1);
                |VALUE_INCREMENT -> 
                |VALUE_DECREMENT
                |BEGIN_LOOP
                |END_LOOP
                |INPUT
                |OUTPUT
                |DEBUG_COMMAND

    List.iter (fun x -> interRec x 0) commands
*)

let debugMemory memory =
    print_string "[|";
    Array.iter (fun x -> Printf.printf ";%d" x) memory;
    print_string "|]";;

(*
let main code =
    let memorySize = 10 in
    let memory = Array.make memorySize 0 in 
    let pointer = ref 0 in

    let interpreter command =
        match command with
            |'>'-> pointer := !pointer + 1
            |'<'-> pointer := !pointer - 1
            |'+'-> Array.set memory !pointer ((Array.get memory !pointer) +1)
            |'-'-> Array.set memory !pointer ((Array.get memory !pointer) -1)
            |'['-> ()
            |']'-> () 
            |','-> Array.set memory !pointer (read_int ())
            |'.'-> print_char (Char.chr (Array.get memory !pointer))
            |_-> ()
    in
   String.iter interpreter code;
   debugMemory memory;
   *)
let main code =
    let MEM_SIZE = 10 in
    let memory = Array.make MEM_SIZE 0 in
    
