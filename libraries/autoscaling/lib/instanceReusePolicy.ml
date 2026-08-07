open Aws.BaseTypes

type t = { reuse_on_scale_in : Boolean.t option }

let make ?reuse_on_scale_in () = { reuse_on_scale_in }

let parse xml =
  Some
    { reuse_on_scale_in =
        Aws.Util.option_bind (Aws.Xml.member "ReuseOnScaleIn" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reuse_on_scale_in (fun f ->
             Aws.Query.Pair ("ReuseOnScaleIn", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reuse_on_scale_in (fun f ->
             "ReuseOnScaleIn", Boolean.to_json f)
       ])

let of_json j =
  { reuse_on_scale_in =
      Aws.Util.option_map (Aws.Json.lookup j "ReuseOnScaleIn") Boolean.of_json
  }
