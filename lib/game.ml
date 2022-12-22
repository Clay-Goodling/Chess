type color = White | Black

type piece = Pawn of color * bool
           | Rook of color * bool
           | Knight of color
           | Bishop of color
           | Queen of color
           | King of color * bool

type board = piece option array array

type t = {
  board: board;
  player: color;
  history: board list;
}

let init_board = [|
  [| Some (Pawn (White, false)); Some (Rook (Black, false)); Some (Knight White); None ; None ; None ; None ; None |];
  [| Some (Pawn (White, false)); Some (Rook (Black, false)); Some (Bishop White); None ; None ; None ; None ; None |];
  [| Some (Pawn (White, false)); Some (King (Black, false)); Some (Queen White); None ; None ; None ; None ; None |];
  [| None; None; None; None; None; None; None; None; |];
  [| None; None; None; None; None; None; None; None; |];
  [| None; None; None; None; None; None; None; None; |];
  [| None; None; None; None; None; None; None; None; |];
  [| None; None; None; None; None; None; None; None; |];
|]

let init_game: t = {
  board = init_board;
  player = White;
  history = []}

let print_piece_option piece square_color =
  ANSITerminal.(print_string [square_color; black]) begin match piece with
  | Some Pawn  (White, _)  -> "♙ "
  | Some Pawn  (Black, _)  -> "♟︎ "
  | Some Rook  (White, _)  -> "♖ "
  | Some Rook  (Black, _)  -> "♜ "
  | Some Knight White      -> "♘ "
  | Some Knight Black      -> "♞ "
  | Some Bishop White      -> "♗ "
  | Some Bishop Black      -> "♝ "
  | Some Queen  White      -> "♕ "
  | Some Queen  Black      -> "♛ "
  | Some King  (White, _)  -> "♔ "
  | Some King  (Black, _)  -> "♚ "
  | None -> "  "
end

let print_board {board = b; player = p; history = h} =
  print_newline ();
  let i = ref (Array.length b - 1) in
  while !i >= 0 do
    print_int (!i + 1);
    let j = ref 0 in
    while !j < Array.length b.(!i) do
      print_piece_option b.(!i).(!j) (
        if (!i + !j) mod 2 == 0 then
          ANSITerminal.on_magenta
        else
          ANSITerminal.on_white
      );
      j := !j + 1
    done;
    i := !i - 1;
    print_newline ();
  done;
  print_endline " A B C D E F G H";

  print_endline (if p == White then "white to move" else "black to move");
  print_endline ("move " ^ (List.length h  + 1 |> string_of_int))
