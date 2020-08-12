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

let lexer sourceCode =
    let lexerCode = Array.make (String.length sourceCode) DEBUG_COMMAND in
    String.iteri (fun i x -> Array.set lexerCode i (lexerTransform x)) sourceCode;
    lexerCode;;


let main code =
    let mem_size = 10 in
    let memory = Array.make mem_size 0 in
    let codeLength = Array.length code in
    let loopStack = Stack.create () in
    
    let stackHandler commandPointer =
        if Stack.is_empty loopStack then

            Stack.push commandPointer loopStack

        else if Stack.top loopStack <> commandPointer then

            Stack.push commandPointer loopStack

        in
    let rec reader commandPointer memoryPointer =
        if(commandPointer < (codeLength)) then
        match (Array.get code commandPointer) with
            |CELL_INCREMENT ->if(memoryPointer = (mem_size -1) ) then reader (commandPointer + 1) 0
                              else reader (commandPointer + 1) (memoryPointer + 1)
            |CELL_DECREMENT -> if(memoryPointer = 0 ) then reader (commandPointer + 1) (mem_size -1) 
                               else reader (commandPointer + 1) (memoryPointer - 1)            
            |VALUE_INCREMENT -> Array.set memory memoryPointer ((Array.get memory memoryPointer) + 1); reader (commandPointer + 1) memoryPointer
            |VALUE_DECREMENT -> Array.set memory memoryPointer ((Array.get memory memoryPointer) - 1); reader (commandPointer + 1) memoryPointer
            |BEGIN_LOOP -> 
                    while (Array.get memory memoryPointer) <> 0 do
                        reader (commandPointer +1) memoryPointer
                    done;
                    reader ((Stack.pop loopStack) + 1) memoryPointer
            |END_LOOP -> stackHandler commandPointer
            |INPUT -> Array.set memory memoryPointer (read_int ()) ; reader (commandPointer + 1) memoryPointer
            |OUTPUT -> print_char (Char.chr (Array.get memory memoryPointer)) ; reader (commandPointer + 1) memoryPointer
            |DEBUG_COMMAND -> debugMemory memory;  reader (commandPointer + 1) memoryPointer

    in
    reader 0 0;
    debugMemory memory;;

main (lexer (read_line()))
