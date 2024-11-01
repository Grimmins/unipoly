
type error =
  | InvalidAction (* à enlever très clairement *)
  | InvalidMove
  | InvalidPlayer
  | InvalidBoard
  | InvalidSquare
  | NotEnoughMoney
  | NoOwner