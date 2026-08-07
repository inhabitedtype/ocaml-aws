open Aws.BaseTypes

type t =
  { name : String.t option
  ; reference_details : ReferenceDetails.t option
  }

let make ?name ?reference_details () = { name; reference_details }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; reference_details =
        Aws.Util.option_bind
          (Aws.Xml.member "ReferenceDetails" xml)
          ReferenceDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reference_details (fun f ->
             Aws.Query.Pair ("ReferenceDetails", ReferenceDetails.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reference_details (fun f ->
             "ReferenceDetails", ReferenceDetails.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; reference_details =
      Aws.Util.option_map (Aws.Json.lookup j "ReferenceDetails") ReferenceDetails.of_json
  }
