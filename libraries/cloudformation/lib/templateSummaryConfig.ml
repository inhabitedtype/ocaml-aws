open Aws.BaseTypes

type t = { treat_unrecognized_resource_types_as_warnings : Boolean.t option }

let make ?treat_unrecognized_resource_types_as_warnings () =
  { treat_unrecognized_resource_types_as_warnings }

let parse xml =
  Some
    { treat_unrecognized_resource_types_as_warnings =
        Aws.Util.option_bind
          (Aws.Xml.member "TreatUnrecognizedResourceTypesAsWarnings" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.treat_unrecognized_resource_types_as_warnings (fun f ->
             Aws.Query.Pair
               ("TreatUnrecognizedResourceTypesAsWarnings", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.treat_unrecognized_resource_types_as_warnings (fun f ->
             "TreatUnrecognizedResourceTypesAsWarnings", Boolean.to_json f)
       ])

let of_json j =
  { treat_unrecognized_resource_types_as_warnings =
      Aws.Util.option_map
        (Aws.Json.lookup j "TreatUnrecognizedResourceTypesAsWarnings")
        Boolean.of_json
  }
