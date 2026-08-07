open Aws.BaseTypes

type t =
  { enabled : Boolean.t
  ; bucket_name : String.t option
  ; prefix : String.t option
  ; bucket_owner : String.t option
  }

let make ~enabled ?bucket_name ?prefix ?bucket_owner () =
  { enabled; bucket_name; prefix; bucket_owner }

let parse xml =
  Some
    { enabled =
        Aws.Xml.required
          "Enabled"
          (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
    ; bucket_name = Aws.Util.option_bind (Aws.Xml.member "BucketName" xml) String.parse
    ; prefix = Aws.Util.option_bind (Aws.Xml.member "Prefix" xml) String.parse
    ; bucket_owner = Aws.Util.option_bind (Aws.Xml.member "BucketOwner" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bucket_owner (fun f ->
             Aws.Query.Pair ("BucketOwner", String.to_query f))
       ; Aws.Util.option_map v.prefix (fun f ->
             Aws.Query.Pair ("Prefix", String.to_query f))
       ; Aws.Util.option_map v.bucket_name (fun f ->
             Aws.Query.Pair ("BucketName", String.to_query f))
       ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bucket_owner (fun f -> "BucketOwner", String.to_json f)
       ; Aws.Util.option_map v.prefix (fun f -> "Prefix", String.to_json f)
       ; Aws.Util.option_map v.bucket_name (fun f -> "BucketName", String.to_json f)
       ; Some ("Enabled", Boolean.to_json v.enabled)
       ])

let of_json j =
  { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
  ; bucket_name = Aws.Util.option_map (Aws.Json.lookup j "BucketName") String.of_json
  ; prefix = Aws.Util.option_map (Aws.Json.lookup j "Prefix") String.of_json
  ; bucket_owner = Aws.Util.option_map (Aws.Json.lookup j "BucketOwner") String.of_json
  }
