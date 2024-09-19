type piece =
|X
|O

type pos =
|Piece of piece
|Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
|Top
|Middle
|Bottom

type col_index =
|Left
|Middle
|Right

type pos_index = row_index * col_index

let get_pos board pos_index = 
  (*Break board and pos_index into vars*)
  let top, middle, bottom = board in
  let row, col = pos_index in 
  (*use match to find row*)
  let row_tuple =
    match row with
    | Top -> top
    | Middle -> middle
    | Bottom -> bottom
  in
  (*break the row, use match to find col*)
  let c1, c2, c3 = row_tuple in
  match col with
  | Left -> c1
  | Middle -> c2
  | Right -> c3

let winner board =
  (*May be a better way, but create variable for every space*)
  let top, middle, bottom = board in
  let topL, topM, topR = top in
  let midL, midM, midR = middle in
  let botL, botM, botR = bottom in 

  (*winning if pieces are all the same*)
  let is_win a b c =
    match a, b, c with
    | Piece X, Piece X, Piece X -> true
    | Piece O, Piece O, Piece O -> true
    |_ -> false
  in
  (*pass every row*)
  let check_row =
    (is_win topL topM topR)||
    (is_win midL midM midR) ||
    (is_win botL botM botR)
  in
  (*pass every column*)
  let check_col =
    (is_win botL midL topL) ||
    (is_win botM midM midR) ||
    (is_win botR botM botR)
  in
  (*pass both diagonals*)
  let check_diag =
    (is_win topR midM botL) ||
    (is_win botR midM topL)
  in
  (*or statement, check everything for win*)
  check_row || check_col || check_diag