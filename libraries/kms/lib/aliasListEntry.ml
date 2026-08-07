open Aws.BaseTypes

type t =
  { alias_name : String.t option
  ; alias_arn : String.t option
  ; target_key_id : String.t option
  ; creation_date : DateTime.t option
  ; last_updated_date : DateTime.t option
  }

let make ?alias_name ?alias_arn ?target_key_id ?creation_date ?last_updated_date () =
  { alias_name; alias_arn; target_key_id; creation_date; last_updated_date }

let parse xml =
  Some
    { alias_name = Aws.Util.option_bind (Aws.Xml.member "AliasName" xml) String.parse
    ; alias_arn = Aws.Util.option_bind (Aws.Xml.member "AliasArn" xml) String.parse
    ; target_key_id = Aws.Util.option_bind (Aws.Xml.member "TargetKeyId" xml) String.parse
    ; creation_date =
        Aws.Util.option_bind (Aws.Xml.member "CreationDate" xml) DateTime.parse
    ; last_updated_date =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_updated_date (fun f ->
             Aws.Query.Pair ("LastUpdatedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_date (fun f ->
             Aws.Query.Pair ("CreationDate", DateTime.to_query f))
       ; Aws.Util.option_map v.target_key_id (fun f ->
             Aws.Query.Pair ("TargetKeyId", String.to_query f))
       ; Aws.Util.option_map v.alias_arn (fun f ->
             Aws.Query.Pair ("AliasArn", String.to_query f))
       ; Aws.Util.option_map v.alias_name (fun f ->
             Aws.Query.Pair ("AliasName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_updated_date (fun f ->
             "LastUpdatedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_date (fun f -> "CreationDate", DateTime.to_json f)
       ; Aws.Util.option_map v.target_key_id (fun f -> "TargetKeyId", String.to_json f)
       ; Aws.Util.option_map v.alias_arn (fun f -> "AliasArn", String.to_json f)
       ; Aws.Util.option_map v.alias_name (fun f -> "AliasName", String.to_json f)
       ])

let of_json j =
  { alias_name = Aws.Util.option_map (Aws.Json.lookup j "AliasName") String.of_json
  ; alias_arn = Aws.Util.option_map (Aws.Json.lookup j "AliasArn") String.of_json
  ; target_key_id = Aws.Util.option_map (Aws.Json.lookup j "TargetKeyId") String.of_json
  ; creation_date =
      Aws.Util.option_map (Aws.Json.lookup j "CreationDate") DateTime.of_json
  ; last_updated_date =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedDate") DateTime.of_json
  }
