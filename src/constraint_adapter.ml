module StringList = struct
  open Yojson.Safe

  let normalize (src: json) = match src with
  | `Null -> `Assoc [("data", `List [`Null])]
  | `String s -> `Assoc [("data", `List [ `String s ])]
  | `List l -> `Assoc [("data", `List l)]
  | _ -> src (* malformed *)

  let restore (src: json) = Obj.magic src
end

