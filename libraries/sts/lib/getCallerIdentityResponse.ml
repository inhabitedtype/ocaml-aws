open Aws.BaseTypes

type t =
  { user_id : String.t option
  ; account : String.t option
  ; arn : String.t option
  }

let make ?user_id ?account ?arn () = { user_id; account; arn }

let parse xml =
  Some
    { user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
    ; account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
    ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ; Aws.Util.option_map v.account (fun f ->
             Aws.Query.Pair ("Account", String.to_query f))
       ; Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
       ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
       ; Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
       ])

let of_json j =
  { user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
  ; account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
  ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
  }
