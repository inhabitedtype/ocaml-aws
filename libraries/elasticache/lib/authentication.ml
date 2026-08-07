open Aws.BaseTypes

type t =
  { type_ : AuthenticationType.t option
  ; password_count : Integer.t option
  }

let make ?type_ ?password_count () = { type_; password_count }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) AuthenticationType.parse
    ; password_count =
        Aws.Util.option_bind (Aws.Xml.member "PasswordCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.password_count (fun f ->
             Aws.Query.Pair ("PasswordCount", Integer.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", AuthenticationType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.password_count (fun f ->
             "PasswordCount", Integer.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", AuthenticationType.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") AuthenticationType.of_json
  ; password_count =
      Aws.Util.option_map (Aws.Json.lookup j "PasswordCount") Integer.of_json
  }
