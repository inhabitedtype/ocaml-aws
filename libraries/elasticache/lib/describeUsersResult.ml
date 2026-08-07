open Aws.BaseTypes

type t =
  { users : UserList.t
  ; marker : String.t option
  }

let make ?(users = []) ?marker () = { users; marker }

let parse xml =
  Some
    { users =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Users" xml) UserList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("Users.member", UserList.to_query v.users))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Users", UserList.to_json v.users)
       ])

let of_json j =
  { users = UserList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Users"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
