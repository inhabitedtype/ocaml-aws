open Aws.BaseTypes

type t =
  | Service_managed_ip
  | Service_managed_byoip
  | Amazon_owned_eip
  | Amazon_owned_contig
  | Byoip
  | Ec2_public_ip
  | Anycast_ip_list_ip

let str_to_t =
  [ "anycast-ip-list-ip", Anycast_ip_list_ip
  ; "ec2-public-ip", Ec2_public_ip
  ; "byoip", Byoip
  ; "amazon-owned-contig", Amazon_owned_contig
  ; "amazon-owned-eip", Amazon_owned_eip
  ; "service-managed-byoip", Service_managed_byoip
  ; "service-managed-ip", Service_managed_ip
  ]

let t_to_str =
  [ Anycast_ip_list_ip, "anycast-ip-list-ip"
  ; Ec2_public_ip, "ec2-public-ip"
  ; Byoip, "byoip"
  ; Amazon_owned_contig, "amazon-owned-contig"
  ; Amazon_owned_eip, "amazon-owned-eip"
  ; Service_managed_byoip, "service-managed-byoip"
  ; Service_managed_ip, "service-managed-ip"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
