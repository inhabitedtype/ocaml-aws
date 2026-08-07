open Aws.BaseTypes

type t =
  { enabled : Boolean.t option
  ; banner_text : String.t option
  }

let make ?enabled ?banner_text () = { enabled; banner_text }

let parse xml =
  Some
    { enabled = Aws.Util.option_bind (Aws.Xml.member "enabled" xml) Boolean.parse
    ; banner_text = Aws.Util.option_bind (Aws.Xml.member "bannerText" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.banner_text (fun f ->
             Aws.Query.Pair ("BannerText", String.to_query f))
       ; Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.banner_text (fun f -> "bannerText", String.to_json f)
       ; Aws.Util.option_map v.enabled (fun f -> "enabled", Boolean.to_json f)
       ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "enabled") Boolean.of_json
  ; banner_text = Aws.Util.option_map (Aws.Json.lookup j "bannerText") String.of_json
  }
