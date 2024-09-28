type bfRuntime = { pc : int; pointer : int; memory : bytes; stack : int list }

let bfmemory = 32768

let mkRuntime () =
  {
    pc = 0;
    pointer = 0;
    memory = Bytes.init bfmemory (fun _ -> char_of_int 0);
    stack = [];
  }

let update_pointer inst pointer =
  match inst with '>' -> pointer + 1 | '<' -> pointer - 1 | _ -> pointer

let update_memory inst memory pointer =
  let current_value = Bytes.get memory pointer in
  let update_to = Bytes.set memory pointer in
  let char_add c i = char_of_int ((int_of_char c + 256 + i) mod 256) in
  match inst with
  | '+' -> update_to (char_add current_value 1)
  | '-' -> update_to (char_add current_value (-1))
  | ',' -> update_to (input_char stdin) (*TODO: add shutdown*)
  | _ -> ()

let find_matching_rparen tape pc =
  let rec find_rec current_pc lparen_seen =
    let current_inst = String.get tape current_pc in
    let next_pc = current_pc + 1 in
    match (lparen_seen, current_inst) with
    | 1, ']' -> next_pc
    | _, '[' -> find_rec next_pc (lparen_seen + 1)
    | _, ']' -> find_rec next_pc (lparen_seen - 1)
    | _ -> find_rec next_pc lparen_seen
  in
  find_rec (pc + 1) 1

let update_pc tape memory pointer pc stack =
  match (String.get tape pc, int_of_char (Bytes.get memory pointer)) with
  | '[', 0 -> find_matching_rparen tape pc
  | ']', x when x <> 0 -> List.hd stack
  | _ -> pc + 1

let update_stack inst memory pointer pc stack =
  match (inst, int_of_char (Bytes.get memory pointer)) with
  | '[', x when x <> 0 -> (pc + 1) :: stack
  | ']', 0 -> List.tl stack
  | _ -> stack

let handle_side_effect inst memory pointer = 
  if inst = '.' then print_char (Bytes.get memory pointer); flush stdout

let eval_bf { pc; pointer; memory; stack } tape =
  let inst = String.get tape pc in
  handle_side_effect inst memory pointer;
  update_memory inst memory pointer;
  {
    pc = update_pc tape memory pointer pc stack;
    pointer = update_pointer inst pointer;
    memory;
    stack = update_stack inst memory pointer pc stack;
  }

let rec eval_bf_all runtime tape =
  if runtime.pc = String.length tape then runtime
  else eval_bf_all (eval_bf runtime tape) tape

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let input_program =
  let filename =
    if Array.length Sys.argv <> 2 then failwith "usage: bfi <bf_program>"
    else Sys.argv.(1)
  in
  read_whole_file filename

let _ = eval_bf_all (mkRuntime ()) input_program
