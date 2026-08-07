open Aws.BaseTypes

type t =
  { allocation_id : String.t option
  ; aws_account_id : String.t option
  ; aws_service : String.t option
  ; co_ip : String.t option
  }

let make ?allocation_id ?aws_account_id ?aws_service ?co_ip () =
  { allocation_id; aws_account_id; aws_service; co_ip }

let parse xml =
  Some
    { allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "allocationId" xml) String.parse
    ; aws_account_id =
        Aws.Util.option_bind (Aws.Xml.member "awsAccountId" xml) String.parse
    ; aws_service = Aws.Util.option_bind (Aws.Xml.member "awsService" xml) String.parse
    ; co_ip = Aws.Util.option_bind (Aws.Xml.member "coIp" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.co_ip (fun f -> Aws.Query.Pair ("CoIp", String.to_query f))
       ; Aws.Util.option_map v.aws_service (fun f ->
             Aws.Query.Pair ("AwsService", String.to_query f))
       ; Aws.Util.option_map v.aws_account_id (fun f ->
             Aws.Query.Pair ("AwsAccountId", String.to_query f))
       ; Aws.Util.option_map v.allocation_id (fun f ->
             Aws.Query.Pair ("AllocationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.co_ip (fun f -> "coIp", String.to_json f)
       ; Aws.Util.option_map v.aws_service (fun f -> "awsService", String.to_json f)
       ; Aws.Util.option_map v.aws_account_id (fun f -> "awsAccountId", String.to_json f)
       ; Aws.Util.option_map v.allocation_id (fun f -> "allocationId", String.to_json f)
       ])

let of_json j =
  { allocation_id = Aws.Util.option_map (Aws.Json.lookup j "allocationId") String.of_json
  ; aws_account_id = Aws.Util.option_map (Aws.Json.lookup j "awsAccountId") String.of_json
  ; aws_service = Aws.Util.option_map (Aws.Json.lookup j "awsService") String.of_json
  ; co_ip = Aws.Util.option_map (Aws.Json.lookup j "coIp") String.of_json
  }
