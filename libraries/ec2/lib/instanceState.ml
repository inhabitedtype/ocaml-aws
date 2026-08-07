open Aws.BaseTypes

type t =
  { code : Integer.t
  ; name : InstanceStateName.t
  }

let make ~code ~name () = { code; name }

let parse xml =
  Some
    { code =
        Aws.Xml.required
          "code"
          (Aws.Util.option_bind (Aws.Xml.member "code" xml) Integer.parse)
    ; name =
        Aws.Xml.required
          "name"
          (Aws.Util.option_bind (Aws.Xml.member "name" xml) InstanceStateName.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Name", InstanceStateName.to_query v.name))
       ; Some (Aws.Query.Pair ("Code", Integer.to_query v.code))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("name", InstanceStateName.to_json v.name)
       ; Some ("code", Integer.to_json v.code)
       ])

let of_json j =
  { code = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "code"))
  ; name = InstanceStateName.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "name"))
  }
