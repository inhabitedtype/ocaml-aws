open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; default_visibility : ManagedResourceDefaultVisibility.t
  }

let make ?dry_run ~default_visibility () = { dry_run; default_visibility }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; default_visibility =
        Aws.Xml.required
          "DefaultVisibility"
          (Aws.Util.option_bind
             (Aws.Xml.member "DefaultVisibility" xml)
             ManagedResourceDefaultVisibility.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DefaultVisibility"
              , ManagedResourceDefaultVisibility.to_query v.default_visibility ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "DefaultVisibility"
           , ManagedResourceDefaultVisibility.to_json v.default_visibility )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; default_visibility =
      ManagedResourceDefaultVisibility.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DefaultVisibility"))
  }
