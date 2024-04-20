33
((3) 0 () 0 () () (h ! (equal)))
struct
(struct PTFail ())
struct
(struct PTSym (c))
  c : char?
struct
(struct PTVar (var t))
  var : string?
  t : PegTree?
struct
(struct PTStr (s))
  s : string?
struct
(struct PTList (xs))
  xs : (listof? PegTree?)
procedure
(parse s) -> PegTree?
  s : string?
procedure
(parse-from-nt nt s) -> PegTree?
  nt : string?
  s : string?
procedure
(parse-file fame) -> PegTree?
  fame : string?
procedure
(parse-file-from-nt ntname fname) -> PegTree?
  ntname : string?
  fname : string?
