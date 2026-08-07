open Aws.BaseTypes

type t =
  | Linux_UNIX
  | Red_Hat_Enterprise_Linux
  | SUSE_Linux
  | Windows
  | Windows_with_SQL_Server
  | Windows_with_SQL_Server_Enterprise
  | Windows_with_SQL_Server_Standard
  | Windows_with_SQL_Server_Web
  | Linux_with_SQL_Server_Standard
  | Linux_with_SQL_Server_Web
  | Linux_with_SQL_Server_Enterprise
  | RHEL_with_SQL_Server_Standard
  | RHEL_with_SQL_Server_Enterprise
  | RHEL_with_SQL_Server_Web
  | RHEL_with_HA
  | RHEL_with_HA_and_SQL_Server_Standard
  | RHEL_with_HA_and_SQL_Server_Enterprise
  | Ubuntu_Pro

let str_to_t =
  [ "Ubuntu Pro", Ubuntu_Pro
  ; "RHEL with HA and SQL Server Enterprise", RHEL_with_HA_and_SQL_Server_Enterprise
  ; "RHEL with HA and SQL Server Standard", RHEL_with_HA_and_SQL_Server_Standard
  ; "RHEL with HA", RHEL_with_HA
  ; "RHEL with SQL Server Web", RHEL_with_SQL_Server_Web
  ; "RHEL with SQL Server Enterprise", RHEL_with_SQL_Server_Enterprise
  ; "RHEL with SQL Server Standard", RHEL_with_SQL_Server_Standard
  ; "Linux with SQL Server Enterprise", Linux_with_SQL_Server_Enterprise
  ; "Linux with SQL Server Web", Linux_with_SQL_Server_Web
  ; "Linux with SQL Server Standard", Linux_with_SQL_Server_Standard
  ; "Windows with SQL Server Web", Windows_with_SQL_Server_Web
  ; "Windows with SQL Server Standard", Windows_with_SQL_Server_Standard
  ; "Windows with SQL Server Enterprise", Windows_with_SQL_Server_Enterprise
  ; "Windows with SQL Server", Windows_with_SQL_Server
  ; "Windows", Windows
  ; "SUSE Linux", SUSE_Linux
  ; "Red Hat Enterprise Linux", Red_Hat_Enterprise_Linux
  ; "Linux/UNIX", Linux_UNIX
  ]

let t_to_str =
  [ Ubuntu_Pro, "Ubuntu Pro"
  ; RHEL_with_HA_and_SQL_Server_Enterprise, "RHEL with HA and SQL Server Enterprise"
  ; RHEL_with_HA_and_SQL_Server_Standard, "RHEL with HA and SQL Server Standard"
  ; RHEL_with_HA, "RHEL with HA"
  ; RHEL_with_SQL_Server_Web, "RHEL with SQL Server Web"
  ; RHEL_with_SQL_Server_Enterprise, "RHEL with SQL Server Enterprise"
  ; RHEL_with_SQL_Server_Standard, "RHEL with SQL Server Standard"
  ; Linux_with_SQL_Server_Enterprise, "Linux with SQL Server Enterprise"
  ; Linux_with_SQL_Server_Web, "Linux with SQL Server Web"
  ; Linux_with_SQL_Server_Standard, "Linux with SQL Server Standard"
  ; Windows_with_SQL_Server_Web, "Windows with SQL Server Web"
  ; Windows_with_SQL_Server_Standard, "Windows with SQL Server Standard"
  ; Windows_with_SQL_Server_Enterprise, "Windows with SQL Server Enterprise"
  ; Windows_with_SQL_Server, "Windows with SQL Server"
  ; Windows, "Windows"
  ; SUSE_Linux, "SUSE Linux"
  ; Red_Hat_Enterprise_Linux, "Red Hat Enterprise Linux"
  ; Linux_UNIX, "Linux/UNIX"
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
