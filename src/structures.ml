module Structure = struct
  type member =
    { name : string
    ; shape : string
    ; loc_name : string option
    ; field_name : string
    ; required : bool
    }

  type t = member list
end

module Shape = struct
  type contents =
    | Structure of Structure.member list
    | List of string * string option
    | Enum of string list

  type t = { name : string; content : contents }


  type parsed = string * string * contents option
end

module Operation = struct
  type t =
    { name : string
    ; http_meth : string
    ; http_uri : string
    ; input_shape : string option
    ; output_shape : string option
    ; output_wrapper : string option
    ; errors : string list
    }
end

module Error = struct
  type t =
    { shape_name : string
    ; string_name : string
    ; variant_name : string
    ; http_code : int option }

  let compare t1 t2 =
    Pervasives.compare
      (t1.string_name, t1.variant_name, t1.http_code)
      (t2.string_name, t2.variant_name, t2.http_code)
end
