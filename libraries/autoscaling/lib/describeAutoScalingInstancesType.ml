open Aws.BaseTypes

type t =
  { instance_ids : InstanceIds.t
  ; max_records : Integer.t option
  ; next_token : String.t option
  }

let make ?(instance_ids = []) ?max_records ?next_token () =
  { instance_ids; max_records; next_token }

let parse xml =
  Some
    { instance_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "InstanceIds" xml) InstanceIds.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some (Aws.Query.Pair ("InstanceIds.member", InstanceIds.to_query v.instance_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("InstanceIds", InstanceIds.to_json v.instance_ids)
       ])

let of_json j =
  { instance_ids =
      InstanceIds.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceIds"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
