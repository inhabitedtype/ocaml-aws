open Aws.BaseTypes

type t =
  { resource_identifier : String.t
  ; apply_action : String.t
  ; opt_in_type : String.t
  }

let make ~resource_identifier ~apply_action ~opt_in_type () =
  { resource_identifier; apply_action; opt_in_type }

let parse xml =
  Some
    { resource_identifier =
        Aws.Xml.required
          "ResourceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "ResourceIdentifier" xml) String.parse)
    ; apply_action =
        Aws.Xml.required
          "ApplyAction"
          (Aws.Util.option_bind (Aws.Xml.member "ApplyAction" xml) String.parse)
    ; opt_in_type =
        Aws.Xml.required
          "OptInType"
          (Aws.Util.option_bind (Aws.Xml.member "OptInType" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("OptInType", String.to_query v.opt_in_type))
       ; Some (Aws.Query.Pair ("ApplyAction", String.to_query v.apply_action))
       ; Some
           (Aws.Query.Pair ("ResourceIdentifier", String.to_query v.resource_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OptInType", String.to_json v.opt_in_type)
       ; Some ("ApplyAction", String.to_json v.apply_action)
       ; Some ("ResourceIdentifier", String.to_json v.resource_identifier)
       ])

let of_json j =
  { resource_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifier"))
  ; apply_action =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyAction"))
  ; opt_in_type = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OptInType"))
  }
