exception RequiredFieldMissing of string

let member tag xml =
  try Some (Ezxmlm.member tag xml) with Ezxmlm.Tag_not_found _ -> None

let members tag xml = try Ezxmlm.members tag xml with Ezxmlm.Tag_not_found _ -> []

let data_to_string = Ezxmlm.data_to_string

let required nm a =
  match a with
  | Some v -> v
  | None -> raise (RequiredFieldMissing nm)
