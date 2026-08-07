open Aws.BaseTypes

type t =
  | A1_medium
  | A1_large
  | A1_xlarge
  | A1_2xlarge
  | A1_4xlarge
  | A1_metal
  | C1_medium
  | C1_xlarge
  | C3_large
  | C3_xlarge
  | C3_2xlarge
  | C3_4xlarge
  | C3_8xlarge
  | C4_large
  | C4_xlarge
  | C4_2xlarge
  | C4_4xlarge
  | C4_8xlarge
  | C5_large
  | C5_xlarge
  | C5_2xlarge
  | C5_4xlarge
  | C5_9xlarge
  | C5_12xlarge
  | C5_18xlarge
  | C5_24xlarge
  | C5_metal
  | C5a_large
  | C5a_xlarge
  | C5a_2xlarge
  | C5a_4xlarge
  | C5a_8xlarge
  | C5a_12xlarge
  | C5a_16xlarge
  | C5a_24xlarge
  | C5ad_large
  | C5ad_xlarge
  | C5ad_2xlarge
  | C5ad_4xlarge
  | C5ad_8xlarge
  | C5ad_12xlarge
  | C5ad_16xlarge
  | C5ad_24xlarge
  | C5d_large
  | C5d_xlarge
  | C5d_2xlarge
  | C5d_4xlarge
  | C5d_9xlarge
  | C5d_12xlarge
  | C5d_18xlarge
  | C5d_24xlarge
  | C5d_metal
  | C5n_large
  | C5n_xlarge
  | C5n_2xlarge
  | C5n_4xlarge
  | C5n_9xlarge
  | C5n_18xlarge
  | C5n_metal
  | C6g_medium
  | C6g_large
  | C6g_xlarge
  | C6g_2xlarge
  | C6g_4xlarge
  | C6g_8xlarge
  | C6g_12xlarge
  | C6g_16xlarge
  | C6g_metal
  | C6gd_medium
  | C6gd_large
  | C6gd_xlarge
  | C6gd_2xlarge
  | C6gd_4xlarge
  | C6gd_8xlarge
  | C6gd_12xlarge
  | C6gd_16xlarge
  | C6gd_metal
  | C6gn_medium
  | C6gn_large
  | C6gn_xlarge
  | C6gn_2xlarge
  | C6gn_4xlarge
  | C6gn_8xlarge
  | C6gn_12xlarge
  | C6gn_16xlarge
  | C6i_large
  | C6i_xlarge
  | C6i_2xlarge
  | C6i_4xlarge
  | C6i_8xlarge
  | C6i_12xlarge
  | C6i_16xlarge
  | C6i_24xlarge
  | C6i_32xlarge
  | C6i_metal
  | Cc1_4xlarge
  | Cc2_8xlarge
  | Cg1_4xlarge
  | Cr1_8xlarge
  | D2_xlarge
  | D2_2xlarge
  | D2_4xlarge
  | D2_8xlarge
  | D3_xlarge
  | D3_2xlarge
  | D3_4xlarge
  | D3_8xlarge
  | D3en_xlarge
  | D3en_2xlarge
  | D3en_4xlarge
  | D3en_6xlarge
  | D3en_8xlarge
  | D3en_12xlarge
  | Dl1_24xlarge
  | F1_2xlarge
  | F1_4xlarge
  | F1_16xlarge
  | G2_2xlarge
  | G2_8xlarge
  | G3_4xlarge
  | G3_8xlarge
  | G3_16xlarge
  | G3s_xlarge
  | G4ad_xlarge
  | G4ad_2xlarge
  | G4ad_4xlarge
  | G4ad_8xlarge
  | G4ad_16xlarge
  | G4dn_xlarge
  | G4dn_2xlarge
  | G4dn_4xlarge
  | G4dn_8xlarge
  | G4dn_12xlarge
  | G4dn_16xlarge
  | G4dn_metal
  | G5_xlarge
  | G5_2xlarge
  | G5_4xlarge
  | G5_8xlarge
  | G5_12xlarge
  | G5_16xlarge
  | G5_24xlarge
  | G5_48xlarge
  | G5g_xlarge
  | G5g_2xlarge
  | G5g_4xlarge
  | G5g_8xlarge
  | G5g_16xlarge
  | G5g_metal
  | Hi1_4xlarge
  | Hpc6a_48xlarge
  | Hs1_8xlarge
  | H1_2xlarge
  | H1_4xlarge
  | H1_8xlarge
  | H1_16xlarge
  | I2_xlarge
  | I2_2xlarge
  | I2_4xlarge
  | I2_8xlarge
  | I3_large
  | I3_xlarge
  | I3_2xlarge
  | I3_4xlarge
  | I3_8xlarge
  | I3_16xlarge
  | I3_metal
  | I3en_large
  | I3en_xlarge
  | I3en_2xlarge
  | I3en_3xlarge
  | I3en_6xlarge
  | I3en_12xlarge
  | I3en_24xlarge
  | I3en_metal
  | Im4gn_large
  | Im4gn_xlarge
  | Im4gn_2xlarge
  | Im4gn_4xlarge
  | Im4gn_8xlarge
  | Im4gn_16xlarge
  | Inf1_xlarge
  | Inf1_2xlarge
  | Inf1_6xlarge
  | Inf1_24xlarge
  | Is4gen_medium
  | Is4gen_large
  | Is4gen_xlarge
  | Is4gen_2xlarge
  | Is4gen_4xlarge
  | Is4gen_8xlarge
  | M1_small
  | M1_medium
  | M1_large
  | M1_xlarge
  | M2_xlarge
  | M2_2xlarge
  | M2_4xlarge
  | M3_medium
  | M3_large
  | M3_xlarge
  | M3_2xlarge
  | M4_large
  | M4_xlarge
  | M4_2xlarge
  | M4_4xlarge
  | M4_10xlarge
  | M4_16xlarge
  | M5_large
  | M5_xlarge
  | M5_2xlarge
  | M5_4xlarge
  | M5_8xlarge
  | M5_12xlarge
  | M5_16xlarge
  | M5_24xlarge
  | M5_metal
  | M5a_large
  | M5a_xlarge
  | M5a_2xlarge
  | M5a_4xlarge
  | M5a_8xlarge
  | M5a_12xlarge
  | M5a_16xlarge
  | M5a_24xlarge
  | M5ad_large
  | M5ad_xlarge
  | M5ad_2xlarge
  | M5ad_4xlarge
  | M5ad_8xlarge
  | M5ad_12xlarge
  | M5ad_16xlarge
  | M5ad_24xlarge
  | M5d_large
  | M5d_xlarge
  | M5d_2xlarge
  | M5d_4xlarge
  | M5d_8xlarge
  | M5d_12xlarge
  | M5d_16xlarge
  | M5d_24xlarge
  | M5d_metal
  | M5dn_large
  | M5dn_xlarge
  | M5dn_2xlarge
  | M5dn_4xlarge
  | M5dn_8xlarge
  | M5dn_12xlarge
  | M5dn_16xlarge
  | M5dn_24xlarge
  | M5dn_metal
  | M5n_large
  | M5n_xlarge
  | M5n_2xlarge
  | M5n_4xlarge
  | M5n_8xlarge
  | M5n_12xlarge
  | M5n_16xlarge
  | M5n_24xlarge
  | M5n_metal
  | M5zn_large
  | M5zn_xlarge
  | M5zn_2xlarge
  | M5zn_3xlarge
  | M5zn_6xlarge
  | M5zn_12xlarge
  | M5zn_metal
  | M6a_large
  | M6a_xlarge
  | M6a_2xlarge
  | M6a_4xlarge
  | M6a_8xlarge
  | M6a_12xlarge
  | M6a_16xlarge
  | M6a_24xlarge
  | M6a_32xlarge
  | M6a_48xlarge
  | M6g_metal
  | M6g_medium
  | M6g_large
  | M6g_xlarge
  | M6g_2xlarge
  | M6g_4xlarge
  | M6g_8xlarge
  | M6g_12xlarge
  | M6g_16xlarge
  | M6gd_metal
  | M6gd_medium
  | M6gd_large
  | M6gd_xlarge
  | M6gd_2xlarge
  | M6gd_4xlarge
  | M6gd_8xlarge
  | M6gd_12xlarge
  | M6gd_16xlarge
  | M6i_large
  | M6i_xlarge
  | M6i_2xlarge
  | M6i_4xlarge
  | M6i_8xlarge
  | M6i_12xlarge
  | M6i_16xlarge
  | M6i_24xlarge
  | M6i_32xlarge
  | M6i_metal
  | Mac1_metal
  | P2_xlarge
  | P2_8xlarge
  | P2_16xlarge
  | P3_2xlarge
  | P3_8xlarge
  | P3_16xlarge
  | P3dn_24xlarge
  | P4d_24xlarge
  | R3_large
  | R3_xlarge
  | R3_2xlarge
  | R3_4xlarge
  | R3_8xlarge
  | R4_large
  | R4_xlarge
  | R4_2xlarge
  | R4_4xlarge
  | R4_8xlarge
  | R4_16xlarge
  | R5_large
  | R5_xlarge
  | R5_2xlarge
  | R5_4xlarge
  | R5_8xlarge
  | R5_12xlarge
  | R5_16xlarge
  | R5_24xlarge
  | R5_metal
  | R5a_large
  | R5a_xlarge
  | R5a_2xlarge
  | R5a_4xlarge
  | R5a_8xlarge
  | R5a_12xlarge
  | R5a_16xlarge
  | R5a_24xlarge
  | R5ad_large
  | R5ad_xlarge
  | R5ad_2xlarge
  | R5ad_4xlarge
  | R5ad_8xlarge
  | R5ad_12xlarge
  | R5ad_16xlarge
  | R5ad_24xlarge
  | R5b_large
  | R5b_xlarge
  | R5b_2xlarge
  | R5b_4xlarge
  | R5b_8xlarge
  | R5b_12xlarge
  | R5b_16xlarge
  | R5b_24xlarge
  | R5b_metal
  | R5d_large
  | R5d_xlarge
  | R5d_2xlarge
  | R5d_4xlarge
  | R5d_8xlarge
  | R5d_12xlarge
  | R5d_16xlarge
  | R5d_24xlarge
  | R5d_metal
  | R5dn_large
  | R5dn_xlarge
  | R5dn_2xlarge
  | R5dn_4xlarge
  | R5dn_8xlarge
  | R5dn_12xlarge
  | R5dn_16xlarge
  | R5dn_24xlarge
  | R5dn_metal
  | R5n_large
  | R5n_xlarge
  | R5n_2xlarge
  | R5n_4xlarge
  | R5n_8xlarge
  | R5n_12xlarge
  | R5n_16xlarge
  | R5n_24xlarge
  | R5n_metal
  | R6g_medium
  | R6g_large
  | R6g_xlarge
  | R6g_2xlarge
  | R6g_4xlarge
  | R6g_8xlarge
  | R6g_12xlarge
  | R6g_16xlarge
  | R6g_metal
  | R6gd_medium
  | R6gd_large
  | R6gd_xlarge
  | R6gd_2xlarge
  | R6gd_4xlarge
  | R6gd_8xlarge
  | R6gd_12xlarge
  | R6gd_16xlarge
  | R6gd_metal
  | R6i_large
  | R6i_xlarge
  | R6i_2xlarge
  | R6i_4xlarge
  | R6i_8xlarge
  | R6i_12xlarge
  | R6i_16xlarge
  | R6i_24xlarge
  | R6i_32xlarge
  | R6i_metal
  | T1_micro
  | T2_nano
  | T2_micro
  | T2_small
  | T2_medium
  | T2_large
  | T2_xlarge
  | T2_2xlarge
  | T3_nano
  | T3_micro
  | T3_small
  | T3_medium
  | T3_large
  | T3_xlarge
  | T3_2xlarge
  | T3a_nano
  | T3a_micro
  | T3a_small
  | T3a_medium
  | T3a_large
  | T3a_xlarge
  | T3a_2xlarge
  | T4g_nano
  | T4g_micro
  | T4g_small
  | T4g_medium
  | T4g_large
  | T4g_xlarge
  | T4g_2xlarge
  | U_6tb1_56xlarge
  | U_6tb1_112xlarge
  | U_9tb1_112xlarge
  | U_12tb1_112xlarge
  | U_6tb1_metal
  | U_9tb1_metal
  | U_12tb1_metal
  | U_18tb1_metal
  | U_24tb1_metal
  | Vt1_3xlarge
  | Vt1_6xlarge
  | Vt1_24xlarge
  | X1_16xlarge
  | X1_32xlarge
  | X1e_xlarge
  | X1e_2xlarge
  | X1e_4xlarge
  | X1e_8xlarge
  | X1e_16xlarge
  | X1e_32xlarge
  | X2iezn_2xlarge
  | X2iezn_4xlarge
  | X2iezn_6xlarge
  | X2iezn_8xlarge
  | X2iezn_12xlarge
  | X2iezn_metal
  | X2gd_medium
  | X2gd_large
  | X2gd_xlarge
  | X2gd_2xlarge
  | X2gd_4xlarge
  | X2gd_8xlarge
  | X2gd_12xlarge
  | X2gd_16xlarge
  | X2gd_metal
  | Z1d_large
  | Z1d_xlarge
  | Z1d_2xlarge
  | Z1d_3xlarge
  | Z1d_6xlarge
  | Z1d_12xlarge
  | Z1d_metal
  | X2idn_16xlarge
  | X2idn_24xlarge
  | X2idn_32xlarge
  | X2iedn_xlarge
  | X2iedn_2xlarge
  | X2iedn_4xlarge
  | X2iedn_8xlarge
  | X2iedn_16xlarge
  | X2iedn_24xlarge
  | X2iedn_32xlarge
  | C6a_large
  | C6a_xlarge
  | C6a_2xlarge
  | C6a_4xlarge
  | C6a_8xlarge
  | C6a_12xlarge
  | C6a_16xlarge
  | C6a_24xlarge
  | C6a_32xlarge
  | C6a_48xlarge
  | C6a_metal
  | M6a_metal
  | I4i_large
  | I4i_xlarge
  | I4i_2xlarge
  | I4i_4xlarge
  | I4i_8xlarge
  | I4i_16xlarge
  | I4i_32xlarge
  | I4i_metal
  | X2idn_metal
  | X2iedn_metal
  | C7g_medium
  | C7g_large
  | C7g_xlarge
  | C7g_2xlarge
  | C7g_4xlarge
  | C7g_8xlarge
  | C7g_12xlarge
  | C7g_16xlarge
  | Mac2_metal
  | C6id_large
  | C6id_xlarge
  | C6id_2xlarge
  | C6id_4xlarge
  | C6id_8xlarge
  | C6id_12xlarge
  | C6id_16xlarge
  | C6id_24xlarge
  | C6id_32xlarge
  | C6id_metal
  | M6id_large
  | M6id_xlarge
  | M6id_2xlarge
  | M6id_4xlarge
  | M6id_8xlarge
  | M6id_12xlarge
  | M6id_16xlarge
  | M6id_24xlarge
  | M6id_32xlarge
  | M6id_metal
  | R6id_large
  | R6id_xlarge
  | R6id_2xlarge
  | R6id_4xlarge
  | R6id_8xlarge
  | R6id_12xlarge
  | R6id_16xlarge
  | R6id_24xlarge
  | R6id_32xlarge
  | R6id_metal
  | R6a_large
  | R6a_xlarge
  | R6a_2xlarge
  | R6a_4xlarge
  | R6a_8xlarge
  | R6a_12xlarge
  | R6a_16xlarge
  | R6a_24xlarge
  | R6a_32xlarge
  | R6a_48xlarge
  | R6a_metal
  | P4de_24xlarge
  | U_3tb1_56xlarge
  | U_18tb1_112xlarge
  | U_24tb1_112xlarge
  | Trn1_2xlarge
  | Trn1_32xlarge
  | Hpc6id_32xlarge
  | C6in_large
  | C6in_xlarge
  | C6in_2xlarge
  | C6in_4xlarge
  | C6in_8xlarge
  | C6in_12xlarge
  | C6in_16xlarge
  | C6in_24xlarge
  | C6in_32xlarge
  | M6in_large
  | M6in_xlarge
  | M6in_2xlarge
  | M6in_4xlarge
  | M6in_8xlarge
  | M6in_12xlarge
  | M6in_16xlarge
  | M6in_24xlarge
  | M6in_32xlarge
  | M6idn_large
  | M6idn_xlarge
  | M6idn_2xlarge
  | M6idn_4xlarge
  | M6idn_8xlarge
  | M6idn_12xlarge
  | M6idn_16xlarge
  | M6idn_24xlarge
  | M6idn_32xlarge
  | R6in_large
  | R6in_xlarge
  | R6in_2xlarge
  | R6in_4xlarge
  | R6in_8xlarge
  | R6in_12xlarge
  | R6in_16xlarge
  | R6in_24xlarge
  | R6in_32xlarge
  | R6idn_large
  | R6idn_xlarge
  | R6idn_2xlarge
  | R6idn_4xlarge
  | R6idn_8xlarge
  | R6idn_12xlarge
  | R6idn_16xlarge
  | R6idn_24xlarge
  | R6idn_32xlarge
  | C7g_metal
  | M7g_medium
  | M7g_large
  | M7g_xlarge
  | M7g_2xlarge
  | M7g_4xlarge
  | M7g_8xlarge
  | M7g_12xlarge
  | M7g_16xlarge
  | M7g_metal
  | R7g_medium
  | R7g_large
  | R7g_xlarge
  | R7g_2xlarge
  | R7g_4xlarge
  | R7g_8xlarge
  | R7g_12xlarge
  | R7g_16xlarge
  | R7g_metal
  | C6in_metal
  | M6in_metal
  | M6idn_metal
  | R6in_metal
  | R6idn_metal
  | Inf2_xlarge
  | Inf2_8xlarge
  | Inf2_24xlarge
  | Inf2_48xlarge
  | Trn1n_32xlarge
  | I4g_large
  | I4g_xlarge
  | I4g_2xlarge
  | I4g_4xlarge
  | I4g_8xlarge
  | I4g_16xlarge
  | Hpc7g_4xlarge
  | Hpc7g_8xlarge
  | Hpc7g_16xlarge
  | C7gn_medium
  | C7gn_large
  | C7gn_xlarge
  | C7gn_2xlarge
  | C7gn_4xlarge
  | C7gn_8xlarge
  | C7gn_12xlarge
  | C7gn_16xlarge
  | P5_48xlarge
  | M7i_large
  | M7i_xlarge
  | M7i_2xlarge
  | M7i_4xlarge
  | M7i_8xlarge
  | M7i_12xlarge
  | M7i_16xlarge
  | M7i_24xlarge
  | M7i_48xlarge
  | M7i_flex_large
  | M7i_flex_xlarge
  | M7i_flex_2xlarge
  | M7i_flex_4xlarge
  | M7i_flex_8xlarge
  | M7a_medium
  | M7a_large
  | M7a_xlarge
  | M7a_2xlarge
  | M7a_4xlarge
  | M7a_8xlarge
  | M7a_12xlarge
  | M7a_16xlarge
  | M7a_24xlarge
  | M7a_32xlarge
  | M7a_48xlarge
  | M7a_metal_48xl
  | Hpc7a_12xlarge
  | Hpc7a_24xlarge
  | Hpc7a_48xlarge
  | Hpc7a_96xlarge
  | C7gd_medium
  | C7gd_large
  | C7gd_xlarge
  | C7gd_2xlarge
  | C7gd_4xlarge
  | C7gd_8xlarge
  | C7gd_12xlarge
  | C7gd_16xlarge
  | M7gd_medium
  | M7gd_large
  | M7gd_xlarge
  | M7gd_2xlarge
  | M7gd_4xlarge
  | M7gd_8xlarge
  | M7gd_12xlarge
  | M7gd_16xlarge
  | R7gd_medium
  | R7gd_large
  | R7gd_xlarge
  | R7gd_2xlarge
  | R7gd_4xlarge
  | R7gd_8xlarge
  | R7gd_12xlarge
  | R7gd_16xlarge
  | R7a_medium
  | R7a_large
  | R7a_xlarge
  | R7a_2xlarge
  | R7a_4xlarge
  | R7a_8xlarge
  | R7a_12xlarge
  | R7a_16xlarge
  | R7a_24xlarge
  | R7a_32xlarge
  | R7a_48xlarge
  | C7i_large
  | C7i_xlarge
  | C7i_2xlarge
  | C7i_4xlarge
  | C7i_8xlarge
  | C7i_12xlarge
  | C7i_16xlarge
  | C7i_24xlarge
  | C7i_48xlarge
  | Mac2_m2pro_metal
  | R7iz_large
  | R7iz_xlarge
  | R7iz_2xlarge
  | R7iz_4xlarge
  | R7iz_8xlarge
  | R7iz_12xlarge
  | R7iz_16xlarge
  | R7iz_32xlarge
  | C7a_medium
  | C7a_large
  | C7a_xlarge
  | C7a_2xlarge
  | C7a_4xlarge
  | C7a_8xlarge
  | C7a_12xlarge
  | C7a_16xlarge
  | C7a_24xlarge
  | C7a_32xlarge
  | C7a_48xlarge
  | C7a_metal_48xl
  | R7a_metal_48xl
  | R7i_large
  | R7i_xlarge
  | R7i_2xlarge
  | R7i_4xlarge
  | R7i_8xlarge
  | R7i_12xlarge
  | R7i_16xlarge
  | R7i_24xlarge
  | R7i_48xlarge
  | Dl2q_24xlarge
  | Mac2_m2_metal
  | I4i_12xlarge
  | I4i_24xlarge
  | C7i_metal_24xl
  | C7i_metal_48xl
  | M7i_metal_24xl
  | M7i_metal_48xl
  | R7i_metal_24xl
  | R7i_metal_48xl
  | R7iz_metal_16xl
  | R7iz_metal_32xl
  | C7gd_metal
  | M7gd_metal
  | R7gd_metal
  | G6_xlarge
  | G6_2xlarge
  | G6_4xlarge
  | G6_8xlarge
  | G6_12xlarge
  | G6_16xlarge
  | G6_24xlarge
  | G6_48xlarge
  | Gr6_4xlarge
  | Gr6_8xlarge
  | C7i_flex_large
  | C7i_flex_xlarge
  | C7i_flex_2xlarge
  | C7i_flex_4xlarge
  | C7i_flex_8xlarge
  | U7i_12tb_224xlarge
  | U7in_16tb_224xlarge
  | U7in_24tb_224xlarge
  | U7in_32tb_224xlarge
  | U7ib_12tb_224xlarge
  | C7gn_metal
  | R8g_medium
  | R8g_large
  | R8g_xlarge
  | R8g_2xlarge
  | R8g_4xlarge
  | R8g_8xlarge
  | R8g_12xlarge
  | R8g_16xlarge
  | R8g_24xlarge
  | R8g_48xlarge
  | R8g_metal_24xl
  | R8g_metal_48xl
  | Mac2_m1ultra_metal
  | G6e_xlarge
  | G6e_2xlarge
  | G6e_4xlarge
  | G6e_8xlarge
  | G6e_12xlarge
  | G6e_16xlarge
  | G6e_24xlarge
  | G6e_48xlarge
  | C8g_medium
  | C8g_large
  | C8g_xlarge
  | C8g_2xlarge
  | C8g_4xlarge
  | C8g_8xlarge
  | C8g_12xlarge
  | C8g_16xlarge
  | C8g_24xlarge
  | C8g_48xlarge
  | C8g_metal_24xl
  | C8g_metal_48xl
  | M8g_medium
  | M8g_large
  | M8g_xlarge
  | M8g_2xlarge
  | M8g_4xlarge
  | M8g_8xlarge
  | M8g_12xlarge
  | M8g_16xlarge
  | M8g_24xlarge
  | M8g_48xlarge
  | M8g_metal_24xl
  | M8g_metal_48xl
  | X8g_medium
  | X8g_large
  | X8g_xlarge
  | X8g_2xlarge
  | X8g_4xlarge
  | X8g_8xlarge
  | X8g_12xlarge
  | X8g_16xlarge
  | X8g_24xlarge
  | X8g_48xlarge
  | X8g_metal_24xl
  | X8g_metal_48xl
  | I7ie_large
  | I7ie_xlarge
  | I7ie_2xlarge
  | I7ie_3xlarge
  | I7ie_6xlarge
  | I7ie_12xlarge
  | I7ie_18xlarge
  | I7ie_24xlarge
  | I7ie_48xlarge
  | I8g_large
  | I8g_xlarge
  | I8g_2xlarge
  | I8g_4xlarge
  | I8g_8xlarge
  | I8g_12xlarge
  | I8g_16xlarge
  | I8g_24xlarge
  | I8g_metal_24xl
  | U7i_6tb_112xlarge
  | U7i_8tb_112xlarge
  | U7inh_32tb_480xlarge
  | P5e_48xlarge
  | P5en_48xlarge
  | F2_12xlarge
  | F2_48xlarge
  | Trn2_48xlarge
  | C7i_flex_12xlarge
  | C7i_flex_16xlarge
  | M7i_flex_12xlarge
  | M7i_flex_16xlarge
  | I7ie_metal_24xl
  | I7ie_metal_48xl
  | I8g_48xlarge
  | C8gd_medium
  | C8gd_large
  | C8gd_xlarge
  | C8gd_2xlarge
  | C8gd_4xlarge
  | C8gd_8xlarge
  | C8gd_12xlarge
  | C8gd_16xlarge
  | C8gd_24xlarge
  | C8gd_48xlarge
  | C8gd_metal_24xl
  | C8gd_metal_48xl
  | I7i_large
  | I7i_xlarge
  | I7i_2xlarge
  | I7i_4xlarge
  | I7i_8xlarge
  | I7i_12xlarge
  | I7i_16xlarge
  | I7i_24xlarge
  | I7i_48xlarge
  | I7i_metal_24xl
  | I7i_metal_48xl
  | P6_b200_48xlarge
  | M8gd_medium
  | M8gd_large
  | M8gd_xlarge
  | M8gd_2xlarge
  | M8gd_4xlarge
  | M8gd_8xlarge
  | M8gd_12xlarge
  | M8gd_16xlarge
  | M8gd_24xlarge
  | M8gd_48xlarge
  | M8gd_metal_24xl
  | M8gd_metal_48xl
  | R8gd_medium
  | R8gd_large
  | R8gd_xlarge
  | R8gd_2xlarge
  | R8gd_4xlarge
  | R8gd_8xlarge
  | R8gd_12xlarge
  | R8gd_16xlarge
  | R8gd_24xlarge
  | R8gd_48xlarge
  | R8gd_metal_24xl
  | R8gd_metal_48xl
  | C8gn_medium
  | C8gn_large
  | C8gn_xlarge
  | C8gn_2xlarge
  | C8gn_4xlarge
  | C8gn_8xlarge
  | C8gn_12xlarge
  | C8gn_16xlarge
  | C8gn_24xlarge
  | C8gn_48xlarge
  | C8gn_metal_24xl
  | C8gn_metal_48xl
  | F2_6xlarge
  | P6e_gb200_36xlarge
  | G6f_large
  | G6f_xlarge
  | G6f_2xlarge
  | G6f_4xlarge
  | Gr6f_4xlarge
  | P5_4xlarge
  | R8i_large
  | R8i_xlarge
  | R8i_2xlarge
  | R8i_4xlarge
  | R8i_8xlarge
  | R8i_12xlarge
  | R8i_16xlarge
  | R8i_24xlarge
  | R8i_32xlarge
  | R8i_48xlarge
  | R8i_96xlarge
  | R8i_metal_48xl
  | R8i_metal_96xl
  | R8i_flex_large
  | R8i_flex_xlarge
  | R8i_flex_2xlarge
  | R8i_flex_4xlarge
  | R8i_flex_8xlarge
  | R8i_flex_12xlarge
  | R8i_flex_16xlarge
  | M8i_large
  | M8i_xlarge
  | M8i_2xlarge
  | M8i_4xlarge
  | M8i_8xlarge
  | M8i_12xlarge
  | M8i_16xlarge
  | M8i_24xlarge
  | M8i_32xlarge
  | M8i_48xlarge
  | M8i_96xlarge
  | M8i_metal_48xl
  | M8i_metal_96xl
  | M8i_flex_large
  | M8i_flex_xlarge
  | M8i_flex_2xlarge
  | M8i_flex_4xlarge
  | M8i_flex_8xlarge
  | M8i_flex_12xlarge
  | M8i_flex_16xlarge
  | I8ge_large
  | I8ge_xlarge
  | I8ge_2xlarge
  | I8ge_3xlarge
  | I8ge_6xlarge
  | I8ge_12xlarge
  | I8ge_18xlarge
  | I8ge_24xlarge
  | I8ge_48xlarge
  | I8ge_metal_24xl
  | I8ge_metal_48xl
  | Mac_m4_metal
  | Mac_m4pro_metal
  | R8gn_medium
  | R8gn_large
  | R8gn_xlarge
  | R8gn_2xlarge
  | R8gn_4xlarge
  | R8gn_8xlarge
  | R8gn_12xlarge
  | R8gn_16xlarge
  | R8gn_24xlarge
  | R8gn_48xlarge
  | R8gn_metal_24xl
  | R8gn_metal_48xl
  | C8i_large
  | C8i_xlarge
  | C8i_2xlarge
  | C8i_4xlarge
  | C8i_8xlarge
  | C8i_12xlarge
  | C8i_16xlarge
  | C8i_24xlarge
  | C8i_32xlarge
  | C8i_48xlarge
  | C8i_96xlarge
  | C8i_metal_48xl
  | C8i_metal_96xl
  | C8i_flex_large
  | C8i_flex_xlarge
  | C8i_flex_2xlarge
  | C8i_flex_4xlarge
  | C8i_flex_8xlarge
  | C8i_flex_12xlarge
  | C8i_flex_16xlarge
  | R8gb_medium
  | R8gb_large
  | R8gb_xlarge
  | R8gb_2xlarge
  | R8gb_4xlarge
  | R8gb_8xlarge
  | R8gb_12xlarge
  | R8gb_16xlarge
  | R8gb_24xlarge
  | R8gb_metal_24xl
  | M8a_medium
  | M8a_large
  | M8a_xlarge
  | M8a_2xlarge
  | M8a_4xlarge
  | M8a_8xlarge
  | M8a_12xlarge
  | M8a_16xlarge
  | M8a_24xlarge
  | M8a_48xlarge
  | M8a_metal_24xl
  | M8a_metal_48xl
  | Trn2_3xlarge
  | R8a_medium
  | R8a_large
  | R8a_xlarge
  | R8a_2xlarge
  | R8a_4xlarge
  | R8a_8xlarge
  | R8a_12xlarge
  | R8a_16xlarge
  | R8a_24xlarge
  | R8a_48xlarge
  | R8a_metal_24xl
  | R8a_metal_48xl
  | P6_b300_48xlarge
  | C8a_medium
  | C8a_large
  | C8a_xlarge
  | C8a_2xlarge
  | C8a_4xlarge
  | C8a_8xlarge
  | C8a_12xlarge
  | C8a_16xlarge
  | C8a_24xlarge
  | C8a_48xlarge
  | C8a_metal_24xl
  | C8a_metal_48xl
  | C8gb_12xlarge
  | C8gb_16xlarge
  | C8gb_24xlarge
  | C8gb_2xlarge
  | C8gb_4xlarge
  | C8gb_8xlarge
  | C8gb_large
  | C8gb_medium
  | C8gb_metal_24xl
  | C8gb_xlarge
  | C8gb_48xlarge
  | C8gb_metal_48xl
  | M8gb_12xlarge
  | M8gb_16xlarge
  | M8gb_24xlarge
  | M8gb_2xlarge
  | M8gb_4xlarge
  | M8gb_8xlarge
  | M8gb_large
  | M8gb_medium
  | M8gb_xlarge
  | M8gb_48xlarge
  | M8gb_metal_24xl
  | M8gb_metal_48xl
  | M8gn_12xlarge
  | M8gn_16xlarge
  | M8gn_24xlarge
  | M8gn_2xlarge
  | M8gn_48xlarge
  | M8gn_4xlarge
  | M8gn_8xlarge
  | M8gn_large
  | M8gn_medium
  | M8gn_xlarge
  | M8gn_metal_24xl
  | M8gn_metal_48xl
  | X8aedz_12xlarge
  | X8aedz_24xlarge
  | X8aedz_3xlarge
  | X8aedz_6xlarge
  | X8aedz_large
  | X8aedz_metal_12xl
  | X8aedz_metal_24xl
  | X8aedz_xlarge
  | M8azn_medium
  | M8azn_large
  | M8azn_xlarge
  | M8azn_3xlarge
  | M8azn_6xlarge
  | M8azn_12xlarge
  | M8azn_24xlarge
  | M8azn_metal_12xl
  | M8azn_metal_24xl
  | X8i_large
  | X8i_xlarge
  | X8i_2xlarge
  | X8i_4xlarge
  | X8i_8xlarge
  | X8i_12xlarge
  | X8i_16xlarge
  | X8i_24xlarge
  | X8i_32xlarge
  | X8i_48xlarge
  | X8i_64xlarge
  | X8i_96xlarge
  | X8i_metal_48xl
  | X8i_metal_96xl
  | Mac_m4max_metal
  | G7e_2xlarge
  | G7e_4xlarge
  | G7e_8xlarge
  | G7e_12xlarge
  | G7e_24xlarge
  | G7e_48xlarge
  | R8id_large
  | R8id_xlarge
  | R8id_2xlarge
  | R8id_4xlarge
  | R8id_8xlarge
  | R8id_12xlarge
  | R8id_16xlarge
  | R8id_24xlarge
  | R8id_32xlarge
  | R8id_48xlarge
  | R8id_96xlarge
  | R8id_metal_48xl
  | R8id_metal_96xl
  | C8id_large
  | C8id_xlarge
  | C8id_2xlarge
  | C8id_4xlarge
  | C8id_8xlarge
  | C8id_12xlarge
  | C8id_16xlarge
  | C8id_24xlarge
  | C8id_32xlarge
  | C8id_48xlarge
  | C8id_96xlarge
  | C8id_metal_48xl
  | C8id_metal_96xl
  | M8id_large
  | M8id_xlarge
  | M8id_2xlarge
  | M8id_4xlarge
  | M8id_8xlarge
  | M8id_12xlarge
  | M8id_16xlarge
  | M8id_24xlarge
  | M8id_32xlarge
  | M8id_48xlarge
  | M8id_96xlarge
  | M8id_metal_48xl
  | M8id_metal_96xl
  | Hpc8a_96xlarge

let str_to_t =
  [ "hpc8a.96xlarge", Hpc8a_96xlarge
  ; "m8id.metal-96xl", M8id_metal_96xl
  ; "m8id.metal-48xl", M8id_metal_48xl
  ; "m8id.96xlarge", M8id_96xlarge
  ; "m8id.48xlarge", M8id_48xlarge
  ; "m8id.32xlarge", M8id_32xlarge
  ; "m8id.24xlarge", M8id_24xlarge
  ; "m8id.16xlarge", M8id_16xlarge
  ; "m8id.12xlarge", M8id_12xlarge
  ; "m8id.8xlarge", M8id_8xlarge
  ; "m8id.4xlarge", M8id_4xlarge
  ; "m8id.2xlarge", M8id_2xlarge
  ; "m8id.xlarge", M8id_xlarge
  ; "m8id.large", M8id_large
  ; "c8id.metal-96xl", C8id_metal_96xl
  ; "c8id.metal-48xl", C8id_metal_48xl
  ; "c8id.96xlarge", C8id_96xlarge
  ; "c8id.48xlarge", C8id_48xlarge
  ; "c8id.32xlarge", C8id_32xlarge
  ; "c8id.24xlarge", C8id_24xlarge
  ; "c8id.16xlarge", C8id_16xlarge
  ; "c8id.12xlarge", C8id_12xlarge
  ; "c8id.8xlarge", C8id_8xlarge
  ; "c8id.4xlarge", C8id_4xlarge
  ; "c8id.2xlarge", C8id_2xlarge
  ; "c8id.xlarge", C8id_xlarge
  ; "c8id.large", C8id_large
  ; "r8id.metal-96xl", R8id_metal_96xl
  ; "r8id.metal-48xl", R8id_metal_48xl
  ; "r8id.96xlarge", R8id_96xlarge
  ; "r8id.48xlarge", R8id_48xlarge
  ; "r8id.32xlarge", R8id_32xlarge
  ; "r8id.24xlarge", R8id_24xlarge
  ; "r8id.16xlarge", R8id_16xlarge
  ; "r8id.12xlarge", R8id_12xlarge
  ; "r8id.8xlarge", R8id_8xlarge
  ; "r8id.4xlarge", R8id_4xlarge
  ; "r8id.2xlarge", R8id_2xlarge
  ; "r8id.xlarge", R8id_xlarge
  ; "r8id.large", R8id_large
  ; "g7e.48xlarge", G7e_48xlarge
  ; "g7e.24xlarge", G7e_24xlarge
  ; "g7e.12xlarge", G7e_12xlarge
  ; "g7e.8xlarge", G7e_8xlarge
  ; "g7e.4xlarge", G7e_4xlarge
  ; "g7e.2xlarge", G7e_2xlarge
  ; "mac-m4max.metal", Mac_m4max_metal
  ; "x8i.metal-96xl", X8i_metal_96xl
  ; "x8i.metal-48xl", X8i_metal_48xl
  ; "x8i.96xlarge", X8i_96xlarge
  ; "x8i.64xlarge", X8i_64xlarge
  ; "x8i.48xlarge", X8i_48xlarge
  ; "x8i.32xlarge", X8i_32xlarge
  ; "x8i.24xlarge", X8i_24xlarge
  ; "x8i.16xlarge", X8i_16xlarge
  ; "x8i.12xlarge", X8i_12xlarge
  ; "x8i.8xlarge", X8i_8xlarge
  ; "x8i.4xlarge", X8i_4xlarge
  ; "x8i.2xlarge", X8i_2xlarge
  ; "x8i.xlarge", X8i_xlarge
  ; "x8i.large", X8i_large
  ; "m8azn.metal-24xl", M8azn_metal_24xl
  ; "m8azn.metal-12xl", M8azn_metal_12xl
  ; "m8azn.24xlarge", M8azn_24xlarge
  ; "m8azn.12xlarge", M8azn_12xlarge
  ; "m8azn.6xlarge", M8azn_6xlarge
  ; "m8azn.3xlarge", M8azn_3xlarge
  ; "m8azn.xlarge", M8azn_xlarge
  ; "m8azn.large", M8azn_large
  ; "m8azn.medium", M8azn_medium
  ; "x8aedz.xlarge", X8aedz_xlarge
  ; "x8aedz.metal-24xl", X8aedz_metal_24xl
  ; "x8aedz.metal-12xl", X8aedz_metal_12xl
  ; "x8aedz.large", X8aedz_large
  ; "x8aedz.6xlarge", X8aedz_6xlarge
  ; "x8aedz.3xlarge", X8aedz_3xlarge
  ; "x8aedz.24xlarge", X8aedz_24xlarge
  ; "x8aedz.12xlarge", X8aedz_12xlarge
  ; "m8gn.metal-48xl", M8gn_metal_48xl
  ; "m8gn.metal-24xl", M8gn_metal_24xl
  ; "m8gn.xlarge", M8gn_xlarge
  ; "m8gn.medium", M8gn_medium
  ; "m8gn.large", M8gn_large
  ; "m8gn.8xlarge", M8gn_8xlarge
  ; "m8gn.4xlarge", M8gn_4xlarge
  ; "m8gn.48xlarge", M8gn_48xlarge
  ; "m8gn.2xlarge", M8gn_2xlarge
  ; "m8gn.24xlarge", M8gn_24xlarge
  ; "m8gn.16xlarge", M8gn_16xlarge
  ; "m8gn.12xlarge", M8gn_12xlarge
  ; "m8gb.metal-48xl", M8gb_metal_48xl
  ; "m8gb.metal-24xl", M8gb_metal_24xl
  ; "m8gb.48xlarge", M8gb_48xlarge
  ; "m8gb.xlarge", M8gb_xlarge
  ; "m8gb.medium", M8gb_medium
  ; "m8gb.large", M8gb_large
  ; "m8gb.8xlarge", M8gb_8xlarge
  ; "m8gb.4xlarge", M8gb_4xlarge
  ; "m8gb.2xlarge", M8gb_2xlarge
  ; "m8gb.24xlarge", M8gb_24xlarge
  ; "m8gb.16xlarge", M8gb_16xlarge
  ; "m8gb.12xlarge", M8gb_12xlarge
  ; "c8gb.metal-48xl", C8gb_metal_48xl
  ; "c8gb.48xlarge", C8gb_48xlarge
  ; "c8gb.xlarge", C8gb_xlarge
  ; "c8gb.metal-24xl", C8gb_metal_24xl
  ; "c8gb.medium", C8gb_medium
  ; "c8gb.large", C8gb_large
  ; "c8gb.8xlarge", C8gb_8xlarge
  ; "c8gb.4xlarge", C8gb_4xlarge
  ; "c8gb.2xlarge", C8gb_2xlarge
  ; "c8gb.24xlarge", C8gb_24xlarge
  ; "c8gb.16xlarge", C8gb_16xlarge
  ; "c8gb.12xlarge", C8gb_12xlarge
  ; "c8a.metal-48xl", C8a_metal_48xl
  ; "c8a.metal-24xl", C8a_metal_24xl
  ; "c8a.48xlarge", C8a_48xlarge
  ; "c8a.24xlarge", C8a_24xlarge
  ; "c8a.16xlarge", C8a_16xlarge
  ; "c8a.12xlarge", C8a_12xlarge
  ; "c8a.8xlarge", C8a_8xlarge
  ; "c8a.4xlarge", C8a_4xlarge
  ; "c8a.2xlarge", C8a_2xlarge
  ; "c8a.xlarge", C8a_xlarge
  ; "c8a.large", C8a_large
  ; "c8a.medium", C8a_medium
  ; "p6-b300.48xlarge", P6_b300_48xlarge
  ; "r8a.metal-48xl", R8a_metal_48xl
  ; "r8a.metal-24xl", R8a_metal_24xl
  ; "r8a.48xlarge", R8a_48xlarge
  ; "r8a.24xlarge", R8a_24xlarge
  ; "r8a.16xlarge", R8a_16xlarge
  ; "r8a.12xlarge", R8a_12xlarge
  ; "r8a.8xlarge", R8a_8xlarge
  ; "r8a.4xlarge", R8a_4xlarge
  ; "r8a.2xlarge", R8a_2xlarge
  ; "r8a.xlarge", R8a_xlarge
  ; "r8a.large", R8a_large
  ; "r8a.medium", R8a_medium
  ; "trn2.3xlarge", Trn2_3xlarge
  ; "m8a.metal-48xl", M8a_metal_48xl
  ; "m8a.metal-24xl", M8a_metal_24xl
  ; "m8a.48xlarge", M8a_48xlarge
  ; "m8a.24xlarge", M8a_24xlarge
  ; "m8a.16xlarge", M8a_16xlarge
  ; "m8a.12xlarge", M8a_12xlarge
  ; "m8a.8xlarge", M8a_8xlarge
  ; "m8a.4xlarge", M8a_4xlarge
  ; "m8a.2xlarge", M8a_2xlarge
  ; "m8a.xlarge", M8a_xlarge
  ; "m8a.large", M8a_large
  ; "m8a.medium", M8a_medium
  ; "r8gb.metal-24xl", R8gb_metal_24xl
  ; "r8gb.24xlarge", R8gb_24xlarge
  ; "r8gb.16xlarge", R8gb_16xlarge
  ; "r8gb.12xlarge", R8gb_12xlarge
  ; "r8gb.8xlarge", R8gb_8xlarge
  ; "r8gb.4xlarge", R8gb_4xlarge
  ; "r8gb.2xlarge", R8gb_2xlarge
  ; "r8gb.xlarge", R8gb_xlarge
  ; "r8gb.large", R8gb_large
  ; "r8gb.medium", R8gb_medium
  ; "c8i-flex.16xlarge", C8i_flex_16xlarge
  ; "c8i-flex.12xlarge", C8i_flex_12xlarge
  ; "c8i-flex.8xlarge", C8i_flex_8xlarge
  ; "c8i-flex.4xlarge", C8i_flex_4xlarge
  ; "c8i-flex.2xlarge", C8i_flex_2xlarge
  ; "c8i-flex.xlarge", C8i_flex_xlarge
  ; "c8i-flex.large", C8i_flex_large
  ; "c8i.metal-96xl", C8i_metal_96xl
  ; "c8i.metal-48xl", C8i_metal_48xl
  ; "c8i.96xlarge", C8i_96xlarge
  ; "c8i.48xlarge", C8i_48xlarge
  ; "c8i.32xlarge", C8i_32xlarge
  ; "c8i.24xlarge", C8i_24xlarge
  ; "c8i.16xlarge", C8i_16xlarge
  ; "c8i.12xlarge", C8i_12xlarge
  ; "c8i.8xlarge", C8i_8xlarge
  ; "c8i.4xlarge", C8i_4xlarge
  ; "c8i.2xlarge", C8i_2xlarge
  ; "c8i.xlarge", C8i_xlarge
  ; "c8i.large", C8i_large
  ; "r8gn.metal-48xl", R8gn_metal_48xl
  ; "r8gn.metal-24xl", R8gn_metal_24xl
  ; "r8gn.48xlarge", R8gn_48xlarge
  ; "r8gn.24xlarge", R8gn_24xlarge
  ; "r8gn.16xlarge", R8gn_16xlarge
  ; "r8gn.12xlarge", R8gn_12xlarge
  ; "r8gn.8xlarge", R8gn_8xlarge
  ; "r8gn.4xlarge", R8gn_4xlarge
  ; "r8gn.2xlarge", R8gn_2xlarge
  ; "r8gn.xlarge", R8gn_xlarge
  ; "r8gn.large", R8gn_large
  ; "r8gn.medium", R8gn_medium
  ; "mac-m4pro.metal", Mac_m4pro_metal
  ; "mac-m4.metal", Mac_m4_metal
  ; "i8ge.metal-48xl", I8ge_metal_48xl
  ; "i8ge.metal-24xl", I8ge_metal_24xl
  ; "i8ge.48xlarge", I8ge_48xlarge
  ; "i8ge.24xlarge", I8ge_24xlarge
  ; "i8ge.18xlarge", I8ge_18xlarge
  ; "i8ge.12xlarge", I8ge_12xlarge
  ; "i8ge.6xlarge", I8ge_6xlarge
  ; "i8ge.3xlarge", I8ge_3xlarge
  ; "i8ge.2xlarge", I8ge_2xlarge
  ; "i8ge.xlarge", I8ge_xlarge
  ; "i8ge.large", I8ge_large
  ; "m8i-flex.16xlarge", M8i_flex_16xlarge
  ; "m8i-flex.12xlarge", M8i_flex_12xlarge
  ; "m8i-flex.8xlarge", M8i_flex_8xlarge
  ; "m8i-flex.4xlarge", M8i_flex_4xlarge
  ; "m8i-flex.2xlarge", M8i_flex_2xlarge
  ; "m8i-flex.xlarge", M8i_flex_xlarge
  ; "m8i-flex.large", M8i_flex_large
  ; "m8i.metal-96xl", M8i_metal_96xl
  ; "m8i.metal-48xl", M8i_metal_48xl
  ; "m8i.96xlarge", M8i_96xlarge
  ; "m8i.48xlarge", M8i_48xlarge
  ; "m8i.32xlarge", M8i_32xlarge
  ; "m8i.24xlarge", M8i_24xlarge
  ; "m8i.16xlarge", M8i_16xlarge
  ; "m8i.12xlarge", M8i_12xlarge
  ; "m8i.8xlarge", M8i_8xlarge
  ; "m8i.4xlarge", M8i_4xlarge
  ; "m8i.2xlarge", M8i_2xlarge
  ; "m8i.xlarge", M8i_xlarge
  ; "m8i.large", M8i_large
  ; "r8i-flex.16xlarge", R8i_flex_16xlarge
  ; "r8i-flex.12xlarge", R8i_flex_12xlarge
  ; "r8i-flex.8xlarge", R8i_flex_8xlarge
  ; "r8i-flex.4xlarge", R8i_flex_4xlarge
  ; "r8i-flex.2xlarge", R8i_flex_2xlarge
  ; "r8i-flex.xlarge", R8i_flex_xlarge
  ; "r8i-flex.large", R8i_flex_large
  ; "r8i.metal-96xl", R8i_metal_96xl
  ; "r8i.metal-48xl", R8i_metal_48xl
  ; "r8i.96xlarge", R8i_96xlarge
  ; "r8i.48xlarge", R8i_48xlarge
  ; "r8i.32xlarge", R8i_32xlarge
  ; "r8i.24xlarge", R8i_24xlarge
  ; "r8i.16xlarge", R8i_16xlarge
  ; "r8i.12xlarge", R8i_12xlarge
  ; "r8i.8xlarge", R8i_8xlarge
  ; "r8i.4xlarge", R8i_4xlarge
  ; "r8i.2xlarge", R8i_2xlarge
  ; "r8i.xlarge", R8i_xlarge
  ; "r8i.large", R8i_large
  ; "p5.4xlarge", P5_4xlarge
  ; "gr6f.4xlarge", Gr6f_4xlarge
  ; "g6f.4xlarge", G6f_4xlarge
  ; "g6f.2xlarge", G6f_2xlarge
  ; "g6f.xlarge", G6f_xlarge
  ; "g6f.large", G6f_large
  ; "p6e-gb200.36xlarge", P6e_gb200_36xlarge
  ; "f2.6xlarge", F2_6xlarge
  ; "c8gn.metal-48xl", C8gn_metal_48xl
  ; "c8gn.metal-24xl", C8gn_metal_24xl
  ; "c8gn.48xlarge", C8gn_48xlarge
  ; "c8gn.24xlarge", C8gn_24xlarge
  ; "c8gn.16xlarge", C8gn_16xlarge
  ; "c8gn.12xlarge", C8gn_12xlarge
  ; "c8gn.8xlarge", C8gn_8xlarge
  ; "c8gn.4xlarge", C8gn_4xlarge
  ; "c8gn.2xlarge", C8gn_2xlarge
  ; "c8gn.xlarge", C8gn_xlarge
  ; "c8gn.large", C8gn_large
  ; "c8gn.medium", C8gn_medium
  ; "r8gd.metal-48xl", R8gd_metal_48xl
  ; "r8gd.metal-24xl", R8gd_metal_24xl
  ; "r8gd.48xlarge", R8gd_48xlarge
  ; "r8gd.24xlarge", R8gd_24xlarge
  ; "r8gd.16xlarge", R8gd_16xlarge
  ; "r8gd.12xlarge", R8gd_12xlarge
  ; "r8gd.8xlarge", R8gd_8xlarge
  ; "r8gd.4xlarge", R8gd_4xlarge
  ; "r8gd.2xlarge", R8gd_2xlarge
  ; "r8gd.xlarge", R8gd_xlarge
  ; "r8gd.large", R8gd_large
  ; "r8gd.medium", R8gd_medium
  ; "m8gd.metal-48xl", M8gd_metal_48xl
  ; "m8gd.metal-24xl", M8gd_metal_24xl
  ; "m8gd.48xlarge", M8gd_48xlarge
  ; "m8gd.24xlarge", M8gd_24xlarge
  ; "m8gd.16xlarge", M8gd_16xlarge
  ; "m8gd.12xlarge", M8gd_12xlarge
  ; "m8gd.8xlarge", M8gd_8xlarge
  ; "m8gd.4xlarge", M8gd_4xlarge
  ; "m8gd.2xlarge", M8gd_2xlarge
  ; "m8gd.xlarge", M8gd_xlarge
  ; "m8gd.large", M8gd_large
  ; "m8gd.medium", M8gd_medium
  ; "p6-b200.48xlarge", P6_b200_48xlarge
  ; "i7i.metal-48xl", I7i_metal_48xl
  ; "i7i.metal-24xl", I7i_metal_24xl
  ; "i7i.48xlarge", I7i_48xlarge
  ; "i7i.24xlarge", I7i_24xlarge
  ; "i7i.16xlarge", I7i_16xlarge
  ; "i7i.12xlarge", I7i_12xlarge
  ; "i7i.8xlarge", I7i_8xlarge
  ; "i7i.4xlarge", I7i_4xlarge
  ; "i7i.2xlarge", I7i_2xlarge
  ; "i7i.xlarge", I7i_xlarge
  ; "i7i.large", I7i_large
  ; "c8gd.metal-48xl", C8gd_metal_48xl
  ; "c8gd.metal-24xl", C8gd_metal_24xl
  ; "c8gd.48xlarge", C8gd_48xlarge
  ; "c8gd.24xlarge", C8gd_24xlarge
  ; "c8gd.16xlarge", C8gd_16xlarge
  ; "c8gd.12xlarge", C8gd_12xlarge
  ; "c8gd.8xlarge", C8gd_8xlarge
  ; "c8gd.4xlarge", C8gd_4xlarge
  ; "c8gd.2xlarge", C8gd_2xlarge
  ; "c8gd.xlarge", C8gd_xlarge
  ; "c8gd.large", C8gd_large
  ; "c8gd.medium", C8gd_medium
  ; "i8g.48xlarge", I8g_48xlarge
  ; "i7ie.metal-48xl", I7ie_metal_48xl
  ; "i7ie.metal-24xl", I7ie_metal_24xl
  ; "m7i-flex.16xlarge", M7i_flex_16xlarge
  ; "m7i-flex.12xlarge", M7i_flex_12xlarge
  ; "c7i-flex.16xlarge", C7i_flex_16xlarge
  ; "c7i-flex.12xlarge", C7i_flex_12xlarge
  ; "trn2.48xlarge", Trn2_48xlarge
  ; "f2.48xlarge", F2_48xlarge
  ; "f2.12xlarge", F2_12xlarge
  ; "p5en.48xlarge", P5en_48xlarge
  ; "p5e.48xlarge", P5e_48xlarge
  ; "u7inh-32tb.480xlarge", U7inh_32tb_480xlarge
  ; "u7i-8tb.112xlarge", U7i_8tb_112xlarge
  ; "u7i-6tb.112xlarge", U7i_6tb_112xlarge
  ; "i8g.metal-24xl", I8g_metal_24xl
  ; "i8g.24xlarge", I8g_24xlarge
  ; "i8g.16xlarge", I8g_16xlarge
  ; "i8g.12xlarge", I8g_12xlarge
  ; "i8g.8xlarge", I8g_8xlarge
  ; "i8g.4xlarge", I8g_4xlarge
  ; "i8g.2xlarge", I8g_2xlarge
  ; "i8g.xlarge", I8g_xlarge
  ; "i8g.large", I8g_large
  ; "i7ie.48xlarge", I7ie_48xlarge
  ; "i7ie.24xlarge", I7ie_24xlarge
  ; "i7ie.18xlarge", I7ie_18xlarge
  ; "i7ie.12xlarge", I7ie_12xlarge
  ; "i7ie.6xlarge", I7ie_6xlarge
  ; "i7ie.3xlarge", I7ie_3xlarge
  ; "i7ie.2xlarge", I7ie_2xlarge
  ; "i7ie.xlarge", I7ie_xlarge
  ; "i7ie.large", I7ie_large
  ; "x8g.metal-48xl", X8g_metal_48xl
  ; "x8g.metal-24xl", X8g_metal_24xl
  ; "x8g.48xlarge", X8g_48xlarge
  ; "x8g.24xlarge", X8g_24xlarge
  ; "x8g.16xlarge", X8g_16xlarge
  ; "x8g.12xlarge", X8g_12xlarge
  ; "x8g.8xlarge", X8g_8xlarge
  ; "x8g.4xlarge", X8g_4xlarge
  ; "x8g.2xlarge", X8g_2xlarge
  ; "x8g.xlarge", X8g_xlarge
  ; "x8g.large", X8g_large
  ; "x8g.medium", X8g_medium
  ; "m8g.metal-48xl", M8g_metal_48xl
  ; "m8g.metal-24xl", M8g_metal_24xl
  ; "m8g.48xlarge", M8g_48xlarge
  ; "m8g.24xlarge", M8g_24xlarge
  ; "m8g.16xlarge", M8g_16xlarge
  ; "m8g.12xlarge", M8g_12xlarge
  ; "m8g.8xlarge", M8g_8xlarge
  ; "m8g.4xlarge", M8g_4xlarge
  ; "m8g.2xlarge", M8g_2xlarge
  ; "m8g.xlarge", M8g_xlarge
  ; "m8g.large", M8g_large
  ; "m8g.medium", M8g_medium
  ; "c8g.metal-48xl", C8g_metal_48xl
  ; "c8g.metal-24xl", C8g_metal_24xl
  ; "c8g.48xlarge", C8g_48xlarge
  ; "c8g.24xlarge", C8g_24xlarge
  ; "c8g.16xlarge", C8g_16xlarge
  ; "c8g.12xlarge", C8g_12xlarge
  ; "c8g.8xlarge", C8g_8xlarge
  ; "c8g.4xlarge", C8g_4xlarge
  ; "c8g.2xlarge", C8g_2xlarge
  ; "c8g.xlarge", C8g_xlarge
  ; "c8g.large", C8g_large
  ; "c8g.medium", C8g_medium
  ; "g6e.48xlarge", G6e_48xlarge
  ; "g6e.24xlarge", G6e_24xlarge
  ; "g6e.16xlarge", G6e_16xlarge
  ; "g6e.12xlarge", G6e_12xlarge
  ; "g6e.8xlarge", G6e_8xlarge
  ; "g6e.4xlarge", G6e_4xlarge
  ; "g6e.2xlarge", G6e_2xlarge
  ; "g6e.xlarge", G6e_xlarge
  ; "mac2-m1ultra.metal", Mac2_m1ultra_metal
  ; "r8g.metal-48xl", R8g_metal_48xl
  ; "r8g.metal-24xl", R8g_metal_24xl
  ; "r8g.48xlarge", R8g_48xlarge
  ; "r8g.24xlarge", R8g_24xlarge
  ; "r8g.16xlarge", R8g_16xlarge
  ; "r8g.12xlarge", R8g_12xlarge
  ; "r8g.8xlarge", R8g_8xlarge
  ; "r8g.4xlarge", R8g_4xlarge
  ; "r8g.2xlarge", R8g_2xlarge
  ; "r8g.xlarge", R8g_xlarge
  ; "r8g.large", R8g_large
  ; "r8g.medium", R8g_medium
  ; "c7gn.metal", C7gn_metal
  ; "u7ib-12tb.224xlarge", U7ib_12tb_224xlarge
  ; "u7in-32tb.224xlarge", U7in_32tb_224xlarge
  ; "u7in-24tb.224xlarge", U7in_24tb_224xlarge
  ; "u7in-16tb.224xlarge", U7in_16tb_224xlarge
  ; "u7i-12tb.224xlarge", U7i_12tb_224xlarge
  ; "c7i-flex.8xlarge", C7i_flex_8xlarge
  ; "c7i-flex.4xlarge", C7i_flex_4xlarge
  ; "c7i-flex.2xlarge", C7i_flex_2xlarge
  ; "c7i-flex.xlarge", C7i_flex_xlarge
  ; "c7i-flex.large", C7i_flex_large
  ; "gr6.8xlarge", Gr6_8xlarge
  ; "gr6.4xlarge", Gr6_4xlarge
  ; "g6.48xlarge", G6_48xlarge
  ; "g6.24xlarge", G6_24xlarge
  ; "g6.16xlarge", G6_16xlarge
  ; "g6.12xlarge", G6_12xlarge
  ; "g6.8xlarge", G6_8xlarge
  ; "g6.4xlarge", G6_4xlarge
  ; "g6.2xlarge", G6_2xlarge
  ; "g6.xlarge", G6_xlarge
  ; "r7gd.metal", R7gd_metal
  ; "m7gd.metal", M7gd_metal
  ; "c7gd.metal", C7gd_metal
  ; "r7iz.metal-32xl", R7iz_metal_32xl
  ; "r7iz.metal-16xl", R7iz_metal_16xl
  ; "r7i.metal-48xl", R7i_metal_48xl
  ; "r7i.metal-24xl", R7i_metal_24xl
  ; "m7i.metal-48xl", M7i_metal_48xl
  ; "m7i.metal-24xl", M7i_metal_24xl
  ; "c7i.metal-48xl", C7i_metal_48xl
  ; "c7i.metal-24xl", C7i_metal_24xl
  ; "i4i.24xlarge", I4i_24xlarge
  ; "i4i.12xlarge", I4i_12xlarge
  ; "mac2-m2.metal", Mac2_m2_metal
  ; "dl2q.24xlarge", Dl2q_24xlarge
  ; "r7i.48xlarge", R7i_48xlarge
  ; "r7i.24xlarge", R7i_24xlarge
  ; "r7i.16xlarge", R7i_16xlarge
  ; "r7i.12xlarge", R7i_12xlarge
  ; "r7i.8xlarge", R7i_8xlarge
  ; "r7i.4xlarge", R7i_4xlarge
  ; "r7i.2xlarge", R7i_2xlarge
  ; "r7i.xlarge", R7i_xlarge
  ; "r7i.large", R7i_large
  ; "r7a.metal-48xl", R7a_metal_48xl
  ; "c7a.metal-48xl", C7a_metal_48xl
  ; "c7a.48xlarge", C7a_48xlarge
  ; "c7a.32xlarge", C7a_32xlarge
  ; "c7a.24xlarge", C7a_24xlarge
  ; "c7a.16xlarge", C7a_16xlarge
  ; "c7a.12xlarge", C7a_12xlarge
  ; "c7a.8xlarge", C7a_8xlarge
  ; "c7a.4xlarge", C7a_4xlarge
  ; "c7a.2xlarge", C7a_2xlarge
  ; "c7a.xlarge", C7a_xlarge
  ; "c7a.large", C7a_large
  ; "c7a.medium", C7a_medium
  ; "r7iz.32xlarge", R7iz_32xlarge
  ; "r7iz.16xlarge", R7iz_16xlarge
  ; "r7iz.12xlarge", R7iz_12xlarge
  ; "r7iz.8xlarge", R7iz_8xlarge
  ; "r7iz.4xlarge", R7iz_4xlarge
  ; "r7iz.2xlarge", R7iz_2xlarge
  ; "r7iz.xlarge", R7iz_xlarge
  ; "r7iz.large", R7iz_large
  ; "mac2-m2pro.metal", Mac2_m2pro_metal
  ; "c7i.48xlarge", C7i_48xlarge
  ; "c7i.24xlarge", C7i_24xlarge
  ; "c7i.16xlarge", C7i_16xlarge
  ; "c7i.12xlarge", C7i_12xlarge
  ; "c7i.8xlarge", C7i_8xlarge
  ; "c7i.4xlarge", C7i_4xlarge
  ; "c7i.2xlarge", C7i_2xlarge
  ; "c7i.xlarge", C7i_xlarge
  ; "c7i.large", C7i_large
  ; "r7a.48xlarge", R7a_48xlarge
  ; "r7a.32xlarge", R7a_32xlarge
  ; "r7a.24xlarge", R7a_24xlarge
  ; "r7a.16xlarge", R7a_16xlarge
  ; "r7a.12xlarge", R7a_12xlarge
  ; "r7a.8xlarge", R7a_8xlarge
  ; "r7a.4xlarge", R7a_4xlarge
  ; "r7a.2xlarge", R7a_2xlarge
  ; "r7a.xlarge", R7a_xlarge
  ; "r7a.large", R7a_large
  ; "r7a.medium", R7a_medium
  ; "r7gd.16xlarge", R7gd_16xlarge
  ; "r7gd.12xlarge", R7gd_12xlarge
  ; "r7gd.8xlarge", R7gd_8xlarge
  ; "r7gd.4xlarge", R7gd_4xlarge
  ; "r7gd.2xlarge", R7gd_2xlarge
  ; "r7gd.xlarge", R7gd_xlarge
  ; "r7gd.large", R7gd_large
  ; "r7gd.medium", R7gd_medium
  ; "m7gd.16xlarge", M7gd_16xlarge
  ; "m7gd.12xlarge", M7gd_12xlarge
  ; "m7gd.8xlarge", M7gd_8xlarge
  ; "m7gd.4xlarge", M7gd_4xlarge
  ; "m7gd.2xlarge", M7gd_2xlarge
  ; "m7gd.xlarge", M7gd_xlarge
  ; "m7gd.large", M7gd_large
  ; "m7gd.medium", M7gd_medium
  ; "c7gd.16xlarge", C7gd_16xlarge
  ; "c7gd.12xlarge", C7gd_12xlarge
  ; "c7gd.8xlarge", C7gd_8xlarge
  ; "c7gd.4xlarge", C7gd_4xlarge
  ; "c7gd.2xlarge", C7gd_2xlarge
  ; "c7gd.xlarge", C7gd_xlarge
  ; "c7gd.large", C7gd_large
  ; "c7gd.medium", C7gd_medium
  ; "hpc7a.96xlarge", Hpc7a_96xlarge
  ; "hpc7a.48xlarge", Hpc7a_48xlarge
  ; "hpc7a.24xlarge", Hpc7a_24xlarge
  ; "hpc7a.12xlarge", Hpc7a_12xlarge
  ; "m7a.metal-48xl", M7a_metal_48xl
  ; "m7a.48xlarge", M7a_48xlarge
  ; "m7a.32xlarge", M7a_32xlarge
  ; "m7a.24xlarge", M7a_24xlarge
  ; "m7a.16xlarge", M7a_16xlarge
  ; "m7a.12xlarge", M7a_12xlarge
  ; "m7a.8xlarge", M7a_8xlarge
  ; "m7a.4xlarge", M7a_4xlarge
  ; "m7a.2xlarge", M7a_2xlarge
  ; "m7a.xlarge", M7a_xlarge
  ; "m7a.large", M7a_large
  ; "m7a.medium", M7a_medium
  ; "m7i-flex.8xlarge", M7i_flex_8xlarge
  ; "m7i-flex.4xlarge", M7i_flex_4xlarge
  ; "m7i-flex.2xlarge", M7i_flex_2xlarge
  ; "m7i-flex.xlarge", M7i_flex_xlarge
  ; "m7i-flex.large", M7i_flex_large
  ; "m7i.48xlarge", M7i_48xlarge
  ; "m7i.24xlarge", M7i_24xlarge
  ; "m7i.16xlarge", M7i_16xlarge
  ; "m7i.12xlarge", M7i_12xlarge
  ; "m7i.8xlarge", M7i_8xlarge
  ; "m7i.4xlarge", M7i_4xlarge
  ; "m7i.2xlarge", M7i_2xlarge
  ; "m7i.xlarge", M7i_xlarge
  ; "m7i.large", M7i_large
  ; "p5.48xlarge", P5_48xlarge
  ; "c7gn.16xlarge", C7gn_16xlarge
  ; "c7gn.12xlarge", C7gn_12xlarge
  ; "c7gn.8xlarge", C7gn_8xlarge
  ; "c7gn.4xlarge", C7gn_4xlarge
  ; "c7gn.2xlarge", C7gn_2xlarge
  ; "c7gn.xlarge", C7gn_xlarge
  ; "c7gn.large", C7gn_large
  ; "c7gn.medium", C7gn_medium
  ; "hpc7g.16xlarge", Hpc7g_16xlarge
  ; "hpc7g.8xlarge", Hpc7g_8xlarge
  ; "hpc7g.4xlarge", Hpc7g_4xlarge
  ; "i4g.16xlarge", I4g_16xlarge
  ; "i4g.8xlarge", I4g_8xlarge
  ; "i4g.4xlarge", I4g_4xlarge
  ; "i4g.2xlarge", I4g_2xlarge
  ; "i4g.xlarge", I4g_xlarge
  ; "i4g.large", I4g_large
  ; "trn1n.32xlarge", Trn1n_32xlarge
  ; "inf2.48xlarge", Inf2_48xlarge
  ; "inf2.24xlarge", Inf2_24xlarge
  ; "inf2.8xlarge", Inf2_8xlarge
  ; "inf2.xlarge", Inf2_xlarge
  ; "r6idn.metal", R6idn_metal
  ; "r6in.metal", R6in_metal
  ; "m6idn.metal", M6idn_metal
  ; "m6in.metal", M6in_metal
  ; "c6in.metal", C6in_metal
  ; "r7g.metal", R7g_metal
  ; "r7g.16xlarge", R7g_16xlarge
  ; "r7g.12xlarge", R7g_12xlarge
  ; "r7g.8xlarge", R7g_8xlarge
  ; "r7g.4xlarge", R7g_4xlarge
  ; "r7g.2xlarge", R7g_2xlarge
  ; "r7g.xlarge", R7g_xlarge
  ; "r7g.large", R7g_large
  ; "r7g.medium", R7g_medium
  ; "m7g.metal", M7g_metal
  ; "m7g.16xlarge", M7g_16xlarge
  ; "m7g.12xlarge", M7g_12xlarge
  ; "m7g.8xlarge", M7g_8xlarge
  ; "m7g.4xlarge", M7g_4xlarge
  ; "m7g.2xlarge", M7g_2xlarge
  ; "m7g.xlarge", M7g_xlarge
  ; "m7g.large", M7g_large
  ; "m7g.medium", M7g_medium
  ; "c7g.metal", C7g_metal
  ; "r6idn.32xlarge", R6idn_32xlarge
  ; "r6idn.24xlarge", R6idn_24xlarge
  ; "r6idn.16xlarge", R6idn_16xlarge
  ; "r6idn.12xlarge", R6idn_12xlarge
  ; "r6idn.8xlarge", R6idn_8xlarge
  ; "r6idn.4xlarge", R6idn_4xlarge
  ; "r6idn.2xlarge", R6idn_2xlarge
  ; "r6idn.xlarge", R6idn_xlarge
  ; "r6idn.large", R6idn_large
  ; "r6in.32xlarge", R6in_32xlarge
  ; "r6in.24xlarge", R6in_24xlarge
  ; "r6in.16xlarge", R6in_16xlarge
  ; "r6in.12xlarge", R6in_12xlarge
  ; "r6in.8xlarge", R6in_8xlarge
  ; "r6in.4xlarge", R6in_4xlarge
  ; "r6in.2xlarge", R6in_2xlarge
  ; "r6in.xlarge", R6in_xlarge
  ; "r6in.large", R6in_large
  ; "m6idn.32xlarge", M6idn_32xlarge
  ; "m6idn.24xlarge", M6idn_24xlarge
  ; "m6idn.16xlarge", M6idn_16xlarge
  ; "m6idn.12xlarge", M6idn_12xlarge
  ; "m6idn.8xlarge", M6idn_8xlarge
  ; "m6idn.4xlarge", M6idn_4xlarge
  ; "m6idn.2xlarge", M6idn_2xlarge
  ; "m6idn.xlarge", M6idn_xlarge
  ; "m6idn.large", M6idn_large
  ; "m6in.32xlarge", M6in_32xlarge
  ; "m6in.24xlarge", M6in_24xlarge
  ; "m6in.16xlarge", M6in_16xlarge
  ; "m6in.12xlarge", M6in_12xlarge
  ; "m6in.8xlarge", M6in_8xlarge
  ; "m6in.4xlarge", M6in_4xlarge
  ; "m6in.2xlarge", M6in_2xlarge
  ; "m6in.xlarge", M6in_xlarge
  ; "m6in.large", M6in_large
  ; "c6in.32xlarge", C6in_32xlarge
  ; "c6in.24xlarge", C6in_24xlarge
  ; "c6in.16xlarge", C6in_16xlarge
  ; "c6in.12xlarge", C6in_12xlarge
  ; "c6in.8xlarge", C6in_8xlarge
  ; "c6in.4xlarge", C6in_4xlarge
  ; "c6in.2xlarge", C6in_2xlarge
  ; "c6in.xlarge", C6in_xlarge
  ; "c6in.large", C6in_large
  ; "hpc6id.32xlarge", Hpc6id_32xlarge
  ; "trn1.32xlarge", Trn1_32xlarge
  ; "trn1.2xlarge", Trn1_2xlarge
  ; "u-24tb1.112xlarge", U_24tb1_112xlarge
  ; "u-18tb1.112xlarge", U_18tb1_112xlarge
  ; "u-3tb1.56xlarge", U_3tb1_56xlarge
  ; "p4de.24xlarge", P4de_24xlarge
  ; "r6a.metal", R6a_metal
  ; "r6a.48xlarge", R6a_48xlarge
  ; "r6a.32xlarge", R6a_32xlarge
  ; "r6a.24xlarge", R6a_24xlarge
  ; "r6a.16xlarge", R6a_16xlarge
  ; "r6a.12xlarge", R6a_12xlarge
  ; "r6a.8xlarge", R6a_8xlarge
  ; "r6a.4xlarge", R6a_4xlarge
  ; "r6a.2xlarge", R6a_2xlarge
  ; "r6a.xlarge", R6a_xlarge
  ; "r6a.large", R6a_large
  ; "r6id.metal", R6id_metal
  ; "r6id.32xlarge", R6id_32xlarge
  ; "r6id.24xlarge", R6id_24xlarge
  ; "r6id.16xlarge", R6id_16xlarge
  ; "r6id.12xlarge", R6id_12xlarge
  ; "r6id.8xlarge", R6id_8xlarge
  ; "r6id.4xlarge", R6id_4xlarge
  ; "r6id.2xlarge", R6id_2xlarge
  ; "r6id.xlarge", R6id_xlarge
  ; "r6id.large", R6id_large
  ; "m6id.metal", M6id_metal
  ; "m6id.32xlarge", M6id_32xlarge
  ; "m6id.24xlarge", M6id_24xlarge
  ; "m6id.16xlarge", M6id_16xlarge
  ; "m6id.12xlarge", M6id_12xlarge
  ; "m6id.8xlarge", M6id_8xlarge
  ; "m6id.4xlarge", M6id_4xlarge
  ; "m6id.2xlarge", M6id_2xlarge
  ; "m6id.xlarge", M6id_xlarge
  ; "m6id.large", M6id_large
  ; "c6id.metal", C6id_metal
  ; "c6id.32xlarge", C6id_32xlarge
  ; "c6id.24xlarge", C6id_24xlarge
  ; "c6id.16xlarge", C6id_16xlarge
  ; "c6id.12xlarge", C6id_12xlarge
  ; "c6id.8xlarge", C6id_8xlarge
  ; "c6id.4xlarge", C6id_4xlarge
  ; "c6id.2xlarge", C6id_2xlarge
  ; "c6id.xlarge", C6id_xlarge
  ; "c6id.large", C6id_large
  ; "mac2.metal", Mac2_metal
  ; "c7g.16xlarge", C7g_16xlarge
  ; "c7g.12xlarge", C7g_12xlarge
  ; "c7g.8xlarge", C7g_8xlarge
  ; "c7g.4xlarge", C7g_4xlarge
  ; "c7g.2xlarge", C7g_2xlarge
  ; "c7g.xlarge", C7g_xlarge
  ; "c7g.large", C7g_large
  ; "c7g.medium", C7g_medium
  ; "x2iedn.metal", X2iedn_metal
  ; "x2idn.metal", X2idn_metal
  ; "i4i.metal", I4i_metal
  ; "i4i.32xlarge", I4i_32xlarge
  ; "i4i.16xlarge", I4i_16xlarge
  ; "i4i.8xlarge", I4i_8xlarge
  ; "i4i.4xlarge", I4i_4xlarge
  ; "i4i.2xlarge", I4i_2xlarge
  ; "i4i.xlarge", I4i_xlarge
  ; "i4i.large", I4i_large
  ; "m6a.metal", M6a_metal
  ; "c6a.metal", C6a_metal
  ; "c6a.48xlarge", C6a_48xlarge
  ; "c6a.32xlarge", C6a_32xlarge
  ; "c6a.24xlarge", C6a_24xlarge
  ; "c6a.16xlarge", C6a_16xlarge
  ; "c6a.12xlarge", C6a_12xlarge
  ; "c6a.8xlarge", C6a_8xlarge
  ; "c6a.4xlarge", C6a_4xlarge
  ; "c6a.2xlarge", C6a_2xlarge
  ; "c6a.xlarge", C6a_xlarge
  ; "c6a.large", C6a_large
  ; "x2iedn.32xlarge", X2iedn_32xlarge
  ; "x2iedn.24xlarge", X2iedn_24xlarge
  ; "x2iedn.16xlarge", X2iedn_16xlarge
  ; "x2iedn.8xlarge", X2iedn_8xlarge
  ; "x2iedn.4xlarge", X2iedn_4xlarge
  ; "x2iedn.2xlarge", X2iedn_2xlarge
  ; "x2iedn.xlarge", X2iedn_xlarge
  ; "x2idn.32xlarge", X2idn_32xlarge
  ; "x2idn.24xlarge", X2idn_24xlarge
  ; "x2idn.16xlarge", X2idn_16xlarge
  ; "z1d.metal", Z1d_metal
  ; "z1d.12xlarge", Z1d_12xlarge
  ; "z1d.6xlarge", Z1d_6xlarge
  ; "z1d.3xlarge", Z1d_3xlarge
  ; "z1d.2xlarge", Z1d_2xlarge
  ; "z1d.xlarge", Z1d_xlarge
  ; "z1d.large", Z1d_large
  ; "x2gd.metal", X2gd_metal
  ; "x2gd.16xlarge", X2gd_16xlarge
  ; "x2gd.12xlarge", X2gd_12xlarge
  ; "x2gd.8xlarge", X2gd_8xlarge
  ; "x2gd.4xlarge", X2gd_4xlarge
  ; "x2gd.2xlarge", X2gd_2xlarge
  ; "x2gd.xlarge", X2gd_xlarge
  ; "x2gd.large", X2gd_large
  ; "x2gd.medium", X2gd_medium
  ; "x2iezn.metal", X2iezn_metal
  ; "x2iezn.12xlarge", X2iezn_12xlarge
  ; "x2iezn.8xlarge", X2iezn_8xlarge
  ; "x2iezn.6xlarge", X2iezn_6xlarge
  ; "x2iezn.4xlarge", X2iezn_4xlarge
  ; "x2iezn.2xlarge", X2iezn_2xlarge
  ; "x1e.32xlarge", X1e_32xlarge
  ; "x1e.16xlarge", X1e_16xlarge
  ; "x1e.8xlarge", X1e_8xlarge
  ; "x1e.4xlarge", X1e_4xlarge
  ; "x1e.2xlarge", X1e_2xlarge
  ; "x1e.xlarge", X1e_xlarge
  ; "x1.32xlarge", X1_32xlarge
  ; "x1.16xlarge", X1_16xlarge
  ; "vt1.24xlarge", Vt1_24xlarge
  ; "vt1.6xlarge", Vt1_6xlarge
  ; "vt1.3xlarge", Vt1_3xlarge
  ; "u-24tb1.metal", U_24tb1_metal
  ; "u-18tb1.metal", U_18tb1_metal
  ; "u-12tb1.metal", U_12tb1_metal
  ; "u-9tb1.metal", U_9tb1_metal
  ; "u-6tb1.metal", U_6tb1_metal
  ; "u-12tb1.112xlarge", U_12tb1_112xlarge
  ; "u-9tb1.112xlarge", U_9tb1_112xlarge
  ; "u-6tb1.112xlarge", U_6tb1_112xlarge
  ; "u-6tb1.56xlarge", U_6tb1_56xlarge
  ; "t4g.2xlarge", T4g_2xlarge
  ; "t4g.xlarge", T4g_xlarge
  ; "t4g.large", T4g_large
  ; "t4g.medium", T4g_medium
  ; "t4g.small", T4g_small
  ; "t4g.micro", T4g_micro
  ; "t4g.nano", T4g_nano
  ; "t3a.2xlarge", T3a_2xlarge
  ; "t3a.xlarge", T3a_xlarge
  ; "t3a.large", T3a_large
  ; "t3a.medium", T3a_medium
  ; "t3a.small", T3a_small
  ; "t3a.micro", T3a_micro
  ; "t3a.nano", T3a_nano
  ; "t3.2xlarge", T3_2xlarge
  ; "t3.xlarge", T3_xlarge
  ; "t3.large", T3_large
  ; "t3.medium", T3_medium
  ; "t3.small", T3_small
  ; "t3.micro", T3_micro
  ; "t3.nano", T3_nano
  ; "t2.2xlarge", T2_2xlarge
  ; "t2.xlarge", T2_xlarge
  ; "t2.large", T2_large
  ; "t2.medium", T2_medium
  ; "t2.small", T2_small
  ; "t2.micro", T2_micro
  ; "t2.nano", T2_nano
  ; "t1.micro", T1_micro
  ; "r6i.metal", R6i_metal
  ; "r6i.32xlarge", R6i_32xlarge
  ; "r6i.24xlarge", R6i_24xlarge
  ; "r6i.16xlarge", R6i_16xlarge
  ; "r6i.12xlarge", R6i_12xlarge
  ; "r6i.8xlarge", R6i_8xlarge
  ; "r6i.4xlarge", R6i_4xlarge
  ; "r6i.2xlarge", R6i_2xlarge
  ; "r6i.xlarge", R6i_xlarge
  ; "r6i.large", R6i_large
  ; "r6gd.metal", R6gd_metal
  ; "r6gd.16xlarge", R6gd_16xlarge
  ; "r6gd.12xlarge", R6gd_12xlarge
  ; "r6gd.8xlarge", R6gd_8xlarge
  ; "r6gd.4xlarge", R6gd_4xlarge
  ; "r6gd.2xlarge", R6gd_2xlarge
  ; "r6gd.xlarge", R6gd_xlarge
  ; "r6gd.large", R6gd_large
  ; "r6gd.medium", R6gd_medium
  ; "r6g.metal", R6g_metal
  ; "r6g.16xlarge", R6g_16xlarge
  ; "r6g.12xlarge", R6g_12xlarge
  ; "r6g.8xlarge", R6g_8xlarge
  ; "r6g.4xlarge", R6g_4xlarge
  ; "r6g.2xlarge", R6g_2xlarge
  ; "r6g.xlarge", R6g_xlarge
  ; "r6g.large", R6g_large
  ; "r6g.medium", R6g_medium
  ; "r5n.metal", R5n_metal
  ; "r5n.24xlarge", R5n_24xlarge
  ; "r5n.16xlarge", R5n_16xlarge
  ; "r5n.12xlarge", R5n_12xlarge
  ; "r5n.8xlarge", R5n_8xlarge
  ; "r5n.4xlarge", R5n_4xlarge
  ; "r5n.2xlarge", R5n_2xlarge
  ; "r5n.xlarge", R5n_xlarge
  ; "r5n.large", R5n_large
  ; "r5dn.metal", R5dn_metal
  ; "r5dn.24xlarge", R5dn_24xlarge
  ; "r5dn.16xlarge", R5dn_16xlarge
  ; "r5dn.12xlarge", R5dn_12xlarge
  ; "r5dn.8xlarge", R5dn_8xlarge
  ; "r5dn.4xlarge", R5dn_4xlarge
  ; "r5dn.2xlarge", R5dn_2xlarge
  ; "r5dn.xlarge", R5dn_xlarge
  ; "r5dn.large", R5dn_large
  ; "r5d.metal", R5d_metal
  ; "r5d.24xlarge", R5d_24xlarge
  ; "r5d.16xlarge", R5d_16xlarge
  ; "r5d.12xlarge", R5d_12xlarge
  ; "r5d.8xlarge", R5d_8xlarge
  ; "r5d.4xlarge", R5d_4xlarge
  ; "r5d.2xlarge", R5d_2xlarge
  ; "r5d.xlarge", R5d_xlarge
  ; "r5d.large", R5d_large
  ; "r5b.metal", R5b_metal
  ; "r5b.24xlarge", R5b_24xlarge
  ; "r5b.16xlarge", R5b_16xlarge
  ; "r5b.12xlarge", R5b_12xlarge
  ; "r5b.8xlarge", R5b_8xlarge
  ; "r5b.4xlarge", R5b_4xlarge
  ; "r5b.2xlarge", R5b_2xlarge
  ; "r5b.xlarge", R5b_xlarge
  ; "r5b.large", R5b_large
  ; "r5ad.24xlarge", R5ad_24xlarge
  ; "r5ad.16xlarge", R5ad_16xlarge
  ; "r5ad.12xlarge", R5ad_12xlarge
  ; "r5ad.8xlarge", R5ad_8xlarge
  ; "r5ad.4xlarge", R5ad_4xlarge
  ; "r5ad.2xlarge", R5ad_2xlarge
  ; "r5ad.xlarge", R5ad_xlarge
  ; "r5ad.large", R5ad_large
  ; "r5a.24xlarge", R5a_24xlarge
  ; "r5a.16xlarge", R5a_16xlarge
  ; "r5a.12xlarge", R5a_12xlarge
  ; "r5a.8xlarge", R5a_8xlarge
  ; "r5a.4xlarge", R5a_4xlarge
  ; "r5a.2xlarge", R5a_2xlarge
  ; "r5a.xlarge", R5a_xlarge
  ; "r5a.large", R5a_large
  ; "r5.metal", R5_metal
  ; "r5.24xlarge", R5_24xlarge
  ; "r5.16xlarge", R5_16xlarge
  ; "r5.12xlarge", R5_12xlarge
  ; "r5.8xlarge", R5_8xlarge
  ; "r5.4xlarge", R5_4xlarge
  ; "r5.2xlarge", R5_2xlarge
  ; "r5.xlarge", R5_xlarge
  ; "r5.large", R5_large
  ; "r4.16xlarge", R4_16xlarge
  ; "r4.8xlarge", R4_8xlarge
  ; "r4.4xlarge", R4_4xlarge
  ; "r4.2xlarge", R4_2xlarge
  ; "r4.xlarge", R4_xlarge
  ; "r4.large", R4_large
  ; "r3.8xlarge", R3_8xlarge
  ; "r3.4xlarge", R3_4xlarge
  ; "r3.2xlarge", R3_2xlarge
  ; "r3.xlarge", R3_xlarge
  ; "r3.large", R3_large
  ; "p4d.24xlarge", P4d_24xlarge
  ; "p3dn.24xlarge", P3dn_24xlarge
  ; "p3.16xlarge", P3_16xlarge
  ; "p3.8xlarge", P3_8xlarge
  ; "p3.2xlarge", P3_2xlarge
  ; "p2.16xlarge", P2_16xlarge
  ; "p2.8xlarge", P2_8xlarge
  ; "p2.xlarge", P2_xlarge
  ; "mac1.metal", Mac1_metal
  ; "m6i.metal", M6i_metal
  ; "m6i.32xlarge", M6i_32xlarge
  ; "m6i.24xlarge", M6i_24xlarge
  ; "m6i.16xlarge", M6i_16xlarge
  ; "m6i.12xlarge", M6i_12xlarge
  ; "m6i.8xlarge", M6i_8xlarge
  ; "m6i.4xlarge", M6i_4xlarge
  ; "m6i.2xlarge", M6i_2xlarge
  ; "m6i.xlarge", M6i_xlarge
  ; "m6i.large", M6i_large
  ; "m6gd.16xlarge", M6gd_16xlarge
  ; "m6gd.12xlarge", M6gd_12xlarge
  ; "m6gd.8xlarge", M6gd_8xlarge
  ; "m6gd.4xlarge", M6gd_4xlarge
  ; "m6gd.2xlarge", M6gd_2xlarge
  ; "m6gd.xlarge", M6gd_xlarge
  ; "m6gd.large", M6gd_large
  ; "m6gd.medium", M6gd_medium
  ; "m6gd.metal", M6gd_metal
  ; "m6g.16xlarge", M6g_16xlarge
  ; "m6g.12xlarge", M6g_12xlarge
  ; "m6g.8xlarge", M6g_8xlarge
  ; "m6g.4xlarge", M6g_4xlarge
  ; "m6g.2xlarge", M6g_2xlarge
  ; "m6g.xlarge", M6g_xlarge
  ; "m6g.large", M6g_large
  ; "m6g.medium", M6g_medium
  ; "m6g.metal", M6g_metal
  ; "m6a.48xlarge", M6a_48xlarge
  ; "m6a.32xlarge", M6a_32xlarge
  ; "m6a.24xlarge", M6a_24xlarge
  ; "m6a.16xlarge", M6a_16xlarge
  ; "m6a.12xlarge", M6a_12xlarge
  ; "m6a.8xlarge", M6a_8xlarge
  ; "m6a.4xlarge", M6a_4xlarge
  ; "m6a.2xlarge", M6a_2xlarge
  ; "m6a.xlarge", M6a_xlarge
  ; "m6a.large", M6a_large
  ; "m5zn.metal", M5zn_metal
  ; "m5zn.12xlarge", M5zn_12xlarge
  ; "m5zn.6xlarge", M5zn_6xlarge
  ; "m5zn.3xlarge", M5zn_3xlarge
  ; "m5zn.2xlarge", M5zn_2xlarge
  ; "m5zn.xlarge", M5zn_xlarge
  ; "m5zn.large", M5zn_large
  ; "m5n.metal", M5n_metal
  ; "m5n.24xlarge", M5n_24xlarge
  ; "m5n.16xlarge", M5n_16xlarge
  ; "m5n.12xlarge", M5n_12xlarge
  ; "m5n.8xlarge", M5n_8xlarge
  ; "m5n.4xlarge", M5n_4xlarge
  ; "m5n.2xlarge", M5n_2xlarge
  ; "m5n.xlarge", M5n_xlarge
  ; "m5n.large", M5n_large
  ; "m5dn.metal", M5dn_metal
  ; "m5dn.24xlarge", M5dn_24xlarge
  ; "m5dn.16xlarge", M5dn_16xlarge
  ; "m5dn.12xlarge", M5dn_12xlarge
  ; "m5dn.8xlarge", M5dn_8xlarge
  ; "m5dn.4xlarge", M5dn_4xlarge
  ; "m5dn.2xlarge", M5dn_2xlarge
  ; "m5dn.xlarge", M5dn_xlarge
  ; "m5dn.large", M5dn_large
  ; "m5d.metal", M5d_metal
  ; "m5d.24xlarge", M5d_24xlarge
  ; "m5d.16xlarge", M5d_16xlarge
  ; "m5d.12xlarge", M5d_12xlarge
  ; "m5d.8xlarge", M5d_8xlarge
  ; "m5d.4xlarge", M5d_4xlarge
  ; "m5d.2xlarge", M5d_2xlarge
  ; "m5d.xlarge", M5d_xlarge
  ; "m5d.large", M5d_large
  ; "m5ad.24xlarge", M5ad_24xlarge
  ; "m5ad.16xlarge", M5ad_16xlarge
  ; "m5ad.12xlarge", M5ad_12xlarge
  ; "m5ad.8xlarge", M5ad_8xlarge
  ; "m5ad.4xlarge", M5ad_4xlarge
  ; "m5ad.2xlarge", M5ad_2xlarge
  ; "m5ad.xlarge", M5ad_xlarge
  ; "m5ad.large", M5ad_large
  ; "m5a.24xlarge", M5a_24xlarge
  ; "m5a.16xlarge", M5a_16xlarge
  ; "m5a.12xlarge", M5a_12xlarge
  ; "m5a.8xlarge", M5a_8xlarge
  ; "m5a.4xlarge", M5a_4xlarge
  ; "m5a.2xlarge", M5a_2xlarge
  ; "m5a.xlarge", M5a_xlarge
  ; "m5a.large", M5a_large
  ; "m5.metal", M5_metal
  ; "m5.24xlarge", M5_24xlarge
  ; "m5.16xlarge", M5_16xlarge
  ; "m5.12xlarge", M5_12xlarge
  ; "m5.8xlarge", M5_8xlarge
  ; "m5.4xlarge", M5_4xlarge
  ; "m5.2xlarge", M5_2xlarge
  ; "m5.xlarge", M5_xlarge
  ; "m5.large", M5_large
  ; "m4.16xlarge", M4_16xlarge
  ; "m4.10xlarge", M4_10xlarge
  ; "m4.4xlarge", M4_4xlarge
  ; "m4.2xlarge", M4_2xlarge
  ; "m4.xlarge", M4_xlarge
  ; "m4.large", M4_large
  ; "m3.2xlarge", M3_2xlarge
  ; "m3.xlarge", M3_xlarge
  ; "m3.large", M3_large
  ; "m3.medium", M3_medium
  ; "m2.4xlarge", M2_4xlarge
  ; "m2.2xlarge", M2_2xlarge
  ; "m2.xlarge", M2_xlarge
  ; "m1.xlarge", M1_xlarge
  ; "m1.large", M1_large
  ; "m1.medium", M1_medium
  ; "m1.small", M1_small
  ; "is4gen.8xlarge", Is4gen_8xlarge
  ; "is4gen.4xlarge", Is4gen_4xlarge
  ; "is4gen.2xlarge", Is4gen_2xlarge
  ; "is4gen.xlarge", Is4gen_xlarge
  ; "is4gen.large", Is4gen_large
  ; "is4gen.medium", Is4gen_medium
  ; "inf1.24xlarge", Inf1_24xlarge
  ; "inf1.6xlarge", Inf1_6xlarge
  ; "inf1.2xlarge", Inf1_2xlarge
  ; "inf1.xlarge", Inf1_xlarge
  ; "im4gn.16xlarge", Im4gn_16xlarge
  ; "im4gn.8xlarge", Im4gn_8xlarge
  ; "im4gn.4xlarge", Im4gn_4xlarge
  ; "im4gn.2xlarge", Im4gn_2xlarge
  ; "im4gn.xlarge", Im4gn_xlarge
  ; "im4gn.large", Im4gn_large
  ; "i3en.metal", I3en_metal
  ; "i3en.24xlarge", I3en_24xlarge
  ; "i3en.12xlarge", I3en_12xlarge
  ; "i3en.6xlarge", I3en_6xlarge
  ; "i3en.3xlarge", I3en_3xlarge
  ; "i3en.2xlarge", I3en_2xlarge
  ; "i3en.xlarge", I3en_xlarge
  ; "i3en.large", I3en_large
  ; "i3.metal", I3_metal
  ; "i3.16xlarge", I3_16xlarge
  ; "i3.8xlarge", I3_8xlarge
  ; "i3.4xlarge", I3_4xlarge
  ; "i3.2xlarge", I3_2xlarge
  ; "i3.xlarge", I3_xlarge
  ; "i3.large", I3_large
  ; "i2.8xlarge", I2_8xlarge
  ; "i2.4xlarge", I2_4xlarge
  ; "i2.2xlarge", I2_2xlarge
  ; "i2.xlarge", I2_xlarge
  ; "h1.16xlarge", H1_16xlarge
  ; "h1.8xlarge", H1_8xlarge
  ; "h1.4xlarge", H1_4xlarge
  ; "h1.2xlarge", H1_2xlarge
  ; "hs1.8xlarge", Hs1_8xlarge
  ; "hpc6a.48xlarge", Hpc6a_48xlarge
  ; "hi1.4xlarge", Hi1_4xlarge
  ; "g5g.metal", G5g_metal
  ; "g5g.16xlarge", G5g_16xlarge
  ; "g5g.8xlarge", G5g_8xlarge
  ; "g5g.4xlarge", G5g_4xlarge
  ; "g5g.2xlarge", G5g_2xlarge
  ; "g5g.xlarge", G5g_xlarge
  ; "g5.48xlarge", G5_48xlarge
  ; "g5.24xlarge", G5_24xlarge
  ; "g5.16xlarge", G5_16xlarge
  ; "g5.12xlarge", G5_12xlarge
  ; "g5.8xlarge", G5_8xlarge
  ; "g5.4xlarge", G5_4xlarge
  ; "g5.2xlarge", G5_2xlarge
  ; "g5.xlarge", G5_xlarge
  ; "g4dn.metal", G4dn_metal
  ; "g4dn.16xlarge", G4dn_16xlarge
  ; "g4dn.12xlarge", G4dn_12xlarge
  ; "g4dn.8xlarge", G4dn_8xlarge
  ; "g4dn.4xlarge", G4dn_4xlarge
  ; "g4dn.2xlarge", G4dn_2xlarge
  ; "g4dn.xlarge", G4dn_xlarge
  ; "g4ad.16xlarge", G4ad_16xlarge
  ; "g4ad.8xlarge", G4ad_8xlarge
  ; "g4ad.4xlarge", G4ad_4xlarge
  ; "g4ad.2xlarge", G4ad_2xlarge
  ; "g4ad.xlarge", G4ad_xlarge
  ; "g3s.xlarge", G3s_xlarge
  ; "g3.16xlarge", G3_16xlarge
  ; "g3.8xlarge", G3_8xlarge
  ; "g3.4xlarge", G3_4xlarge
  ; "g2.8xlarge", G2_8xlarge
  ; "g2.2xlarge", G2_2xlarge
  ; "f1.16xlarge", F1_16xlarge
  ; "f1.4xlarge", F1_4xlarge
  ; "f1.2xlarge", F1_2xlarge
  ; "dl1.24xlarge", Dl1_24xlarge
  ; "d3en.12xlarge", D3en_12xlarge
  ; "d3en.8xlarge", D3en_8xlarge
  ; "d3en.6xlarge", D3en_6xlarge
  ; "d3en.4xlarge", D3en_4xlarge
  ; "d3en.2xlarge", D3en_2xlarge
  ; "d3en.xlarge", D3en_xlarge
  ; "d3.8xlarge", D3_8xlarge
  ; "d3.4xlarge", D3_4xlarge
  ; "d3.2xlarge", D3_2xlarge
  ; "d3.xlarge", D3_xlarge
  ; "d2.8xlarge", D2_8xlarge
  ; "d2.4xlarge", D2_4xlarge
  ; "d2.2xlarge", D2_2xlarge
  ; "d2.xlarge", D2_xlarge
  ; "cr1.8xlarge", Cr1_8xlarge
  ; "cg1.4xlarge", Cg1_4xlarge
  ; "cc2.8xlarge", Cc2_8xlarge
  ; "cc1.4xlarge", Cc1_4xlarge
  ; "c6i.metal", C6i_metal
  ; "c6i.32xlarge", C6i_32xlarge
  ; "c6i.24xlarge", C6i_24xlarge
  ; "c6i.16xlarge", C6i_16xlarge
  ; "c6i.12xlarge", C6i_12xlarge
  ; "c6i.8xlarge", C6i_8xlarge
  ; "c6i.4xlarge", C6i_4xlarge
  ; "c6i.2xlarge", C6i_2xlarge
  ; "c6i.xlarge", C6i_xlarge
  ; "c6i.large", C6i_large
  ; "c6gn.16xlarge", C6gn_16xlarge
  ; "c6gn.12xlarge", C6gn_12xlarge
  ; "c6gn.8xlarge", C6gn_8xlarge
  ; "c6gn.4xlarge", C6gn_4xlarge
  ; "c6gn.2xlarge", C6gn_2xlarge
  ; "c6gn.xlarge", C6gn_xlarge
  ; "c6gn.large", C6gn_large
  ; "c6gn.medium", C6gn_medium
  ; "c6gd.metal", C6gd_metal
  ; "c6gd.16xlarge", C6gd_16xlarge
  ; "c6gd.12xlarge", C6gd_12xlarge
  ; "c6gd.8xlarge", C6gd_8xlarge
  ; "c6gd.4xlarge", C6gd_4xlarge
  ; "c6gd.2xlarge", C6gd_2xlarge
  ; "c6gd.xlarge", C6gd_xlarge
  ; "c6gd.large", C6gd_large
  ; "c6gd.medium", C6gd_medium
  ; "c6g.metal", C6g_metal
  ; "c6g.16xlarge", C6g_16xlarge
  ; "c6g.12xlarge", C6g_12xlarge
  ; "c6g.8xlarge", C6g_8xlarge
  ; "c6g.4xlarge", C6g_4xlarge
  ; "c6g.2xlarge", C6g_2xlarge
  ; "c6g.xlarge", C6g_xlarge
  ; "c6g.large", C6g_large
  ; "c6g.medium", C6g_medium
  ; "c5n.metal", C5n_metal
  ; "c5n.18xlarge", C5n_18xlarge
  ; "c5n.9xlarge", C5n_9xlarge
  ; "c5n.4xlarge", C5n_4xlarge
  ; "c5n.2xlarge", C5n_2xlarge
  ; "c5n.xlarge", C5n_xlarge
  ; "c5n.large", C5n_large
  ; "c5d.metal", C5d_metal
  ; "c5d.24xlarge", C5d_24xlarge
  ; "c5d.18xlarge", C5d_18xlarge
  ; "c5d.12xlarge", C5d_12xlarge
  ; "c5d.9xlarge", C5d_9xlarge
  ; "c5d.4xlarge", C5d_4xlarge
  ; "c5d.2xlarge", C5d_2xlarge
  ; "c5d.xlarge", C5d_xlarge
  ; "c5d.large", C5d_large
  ; "c5ad.24xlarge", C5ad_24xlarge
  ; "c5ad.16xlarge", C5ad_16xlarge
  ; "c5ad.12xlarge", C5ad_12xlarge
  ; "c5ad.8xlarge", C5ad_8xlarge
  ; "c5ad.4xlarge", C5ad_4xlarge
  ; "c5ad.2xlarge", C5ad_2xlarge
  ; "c5ad.xlarge", C5ad_xlarge
  ; "c5ad.large", C5ad_large
  ; "c5a.24xlarge", C5a_24xlarge
  ; "c5a.16xlarge", C5a_16xlarge
  ; "c5a.12xlarge", C5a_12xlarge
  ; "c5a.8xlarge", C5a_8xlarge
  ; "c5a.4xlarge", C5a_4xlarge
  ; "c5a.2xlarge", C5a_2xlarge
  ; "c5a.xlarge", C5a_xlarge
  ; "c5a.large", C5a_large
  ; "c5.metal", C5_metal
  ; "c5.24xlarge", C5_24xlarge
  ; "c5.18xlarge", C5_18xlarge
  ; "c5.12xlarge", C5_12xlarge
  ; "c5.9xlarge", C5_9xlarge
  ; "c5.4xlarge", C5_4xlarge
  ; "c5.2xlarge", C5_2xlarge
  ; "c5.xlarge", C5_xlarge
  ; "c5.large", C5_large
  ; "c4.8xlarge", C4_8xlarge
  ; "c4.4xlarge", C4_4xlarge
  ; "c4.2xlarge", C4_2xlarge
  ; "c4.xlarge", C4_xlarge
  ; "c4.large", C4_large
  ; "c3.8xlarge", C3_8xlarge
  ; "c3.4xlarge", C3_4xlarge
  ; "c3.2xlarge", C3_2xlarge
  ; "c3.xlarge", C3_xlarge
  ; "c3.large", C3_large
  ; "c1.xlarge", C1_xlarge
  ; "c1.medium", C1_medium
  ; "a1.metal", A1_metal
  ; "a1.4xlarge", A1_4xlarge
  ; "a1.2xlarge", A1_2xlarge
  ; "a1.xlarge", A1_xlarge
  ; "a1.large", A1_large
  ; "a1.medium", A1_medium
  ]

let t_to_str =
  [ Hpc8a_96xlarge, "hpc8a.96xlarge"
  ; M8id_metal_96xl, "m8id.metal-96xl"
  ; M8id_metal_48xl, "m8id.metal-48xl"
  ; M8id_96xlarge, "m8id.96xlarge"
  ; M8id_48xlarge, "m8id.48xlarge"
  ; M8id_32xlarge, "m8id.32xlarge"
  ; M8id_24xlarge, "m8id.24xlarge"
  ; M8id_16xlarge, "m8id.16xlarge"
  ; M8id_12xlarge, "m8id.12xlarge"
  ; M8id_8xlarge, "m8id.8xlarge"
  ; M8id_4xlarge, "m8id.4xlarge"
  ; M8id_2xlarge, "m8id.2xlarge"
  ; M8id_xlarge, "m8id.xlarge"
  ; M8id_large, "m8id.large"
  ; C8id_metal_96xl, "c8id.metal-96xl"
  ; C8id_metal_48xl, "c8id.metal-48xl"
  ; C8id_96xlarge, "c8id.96xlarge"
  ; C8id_48xlarge, "c8id.48xlarge"
  ; C8id_32xlarge, "c8id.32xlarge"
  ; C8id_24xlarge, "c8id.24xlarge"
  ; C8id_16xlarge, "c8id.16xlarge"
  ; C8id_12xlarge, "c8id.12xlarge"
  ; C8id_8xlarge, "c8id.8xlarge"
  ; C8id_4xlarge, "c8id.4xlarge"
  ; C8id_2xlarge, "c8id.2xlarge"
  ; C8id_xlarge, "c8id.xlarge"
  ; C8id_large, "c8id.large"
  ; R8id_metal_96xl, "r8id.metal-96xl"
  ; R8id_metal_48xl, "r8id.metal-48xl"
  ; R8id_96xlarge, "r8id.96xlarge"
  ; R8id_48xlarge, "r8id.48xlarge"
  ; R8id_32xlarge, "r8id.32xlarge"
  ; R8id_24xlarge, "r8id.24xlarge"
  ; R8id_16xlarge, "r8id.16xlarge"
  ; R8id_12xlarge, "r8id.12xlarge"
  ; R8id_8xlarge, "r8id.8xlarge"
  ; R8id_4xlarge, "r8id.4xlarge"
  ; R8id_2xlarge, "r8id.2xlarge"
  ; R8id_xlarge, "r8id.xlarge"
  ; R8id_large, "r8id.large"
  ; G7e_48xlarge, "g7e.48xlarge"
  ; G7e_24xlarge, "g7e.24xlarge"
  ; G7e_12xlarge, "g7e.12xlarge"
  ; G7e_8xlarge, "g7e.8xlarge"
  ; G7e_4xlarge, "g7e.4xlarge"
  ; G7e_2xlarge, "g7e.2xlarge"
  ; Mac_m4max_metal, "mac-m4max.metal"
  ; X8i_metal_96xl, "x8i.metal-96xl"
  ; X8i_metal_48xl, "x8i.metal-48xl"
  ; X8i_96xlarge, "x8i.96xlarge"
  ; X8i_64xlarge, "x8i.64xlarge"
  ; X8i_48xlarge, "x8i.48xlarge"
  ; X8i_32xlarge, "x8i.32xlarge"
  ; X8i_24xlarge, "x8i.24xlarge"
  ; X8i_16xlarge, "x8i.16xlarge"
  ; X8i_12xlarge, "x8i.12xlarge"
  ; X8i_8xlarge, "x8i.8xlarge"
  ; X8i_4xlarge, "x8i.4xlarge"
  ; X8i_2xlarge, "x8i.2xlarge"
  ; X8i_xlarge, "x8i.xlarge"
  ; X8i_large, "x8i.large"
  ; M8azn_metal_24xl, "m8azn.metal-24xl"
  ; M8azn_metal_12xl, "m8azn.metal-12xl"
  ; M8azn_24xlarge, "m8azn.24xlarge"
  ; M8azn_12xlarge, "m8azn.12xlarge"
  ; M8azn_6xlarge, "m8azn.6xlarge"
  ; M8azn_3xlarge, "m8azn.3xlarge"
  ; M8azn_xlarge, "m8azn.xlarge"
  ; M8azn_large, "m8azn.large"
  ; M8azn_medium, "m8azn.medium"
  ; X8aedz_xlarge, "x8aedz.xlarge"
  ; X8aedz_metal_24xl, "x8aedz.metal-24xl"
  ; X8aedz_metal_12xl, "x8aedz.metal-12xl"
  ; X8aedz_large, "x8aedz.large"
  ; X8aedz_6xlarge, "x8aedz.6xlarge"
  ; X8aedz_3xlarge, "x8aedz.3xlarge"
  ; X8aedz_24xlarge, "x8aedz.24xlarge"
  ; X8aedz_12xlarge, "x8aedz.12xlarge"
  ; M8gn_metal_48xl, "m8gn.metal-48xl"
  ; M8gn_metal_24xl, "m8gn.metal-24xl"
  ; M8gn_xlarge, "m8gn.xlarge"
  ; M8gn_medium, "m8gn.medium"
  ; M8gn_large, "m8gn.large"
  ; M8gn_8xlarge, "m8gn.8xlarge"
  ; M8gn_4xlarge, "m8gn.4xlarge"
  ; M8gn_48xlarge, "m8gn.48xlarge"
  ; M8gn_2xlarge, "m8gn.2xlarge"
  ; M8gn_24xlarge, "m8gn.24xlarge"
  ; M8gn_16xlarge, "m8gn.16xlarge"
  ; M8gn_12xlarge, "m8gn.12xlarge"
  ; M8gb_metal_48xl, "m8gb.metal-48xl"
  ; M8gb_metal_24xl, "m8gb.metal-24xl"
  ; M8gb_48xlarge, "m8gb.48xlarge"
  ; M8gb_xlarge, "m8gb.xlarge"
  ; M8gb_medium, "m8gb.medium"
  ; M8gb_large, "m8gb.large"
  ; M8gb_8xlarge, "m8gb.8xlarge"
  ; M8gb_4xlarge, "m8gb.4xlarge"
  ; M8gb_2xlarge, "m8gb.2xlarge"
  ; M8gb_24xlarge, "m8gb.24xlarge"
  ; M8gb_16xlarge, "m8gb.16xlarge"
  ; M8gb_12xlarge, "m8gb.12xlarge"
  ; C8gb_metal_48xl, "c8gb.metal-48xl"
  ; C8gb_48xlarge, "c8gb.48xlarge"
  ; C8gb_xlarge, "c8gb.xlarge"
  ; C8gb_metal_24xl, "c8gb.metal-24xl"
  ; C8gb_medium, "c8gb.medium"
  ; C8gb_large, "c8gb.large"
  ; C8gb_8xlarge, "c8gb.8xlarge"
  ; C8gb_4xlarge, "c8gb.4xlarge"
  ; C8gb_2xlarge, "c8gb.2xlarge"
  ; C8gb_24xlarge, "c8gb.24xlarge"
  ; C8gb_16xlarge, "c8gb.16xlarge"
  ; C8gb_12xlarge, "c8gb.12xlarge"
  ; C8a_metal_48xl, "c8a.metal-48xl"
  ; C8a_metal_24xl, "c8a.metal-24xl"
  ; C8a_48xlarge, "c8a.48xlarge"
  ; C8a_24xlarge, "c8a.24xlarge"
  ; C8a_16xlarge, "c8a.16xlarge"
  ; C8a_12xlarge, "c8a.12xlarge"
  ; C8a_8xlarge, "c8a.8xlarge"
  ; C8a_4xlarge, "c8a.4xlarge"
  ; C8a_2xlarge, "c8a.2xlarge"
  ; C8a_xlarge, "c8a.xlarge"
  ; C8a_large, "c8a.large"
  ; C8a_medium, "c8a.medium"
  ; P6_b300_48xlarge, "p6-b300.48xlarge"
  ; R8a_metal_48xl, "r8a.metal-48xl"
  ; R8a_metal_24xl, "r8a.metal-24xl"
  ; R8a_48xlarge, "r8a.48xlarge"
  ; R8a_24xlarge, "r8a.24xlarge"
  ; R8a_16xlarge, "r8a.16xlarge"
  ; R8a_12xlarge, "r8a.12xlarge"
  ; R8a_8xlarge, "r8a.8xlarge"
  ; R8a_4xlarge, "r8a.4xlarge"
  ; R8a_2xlarge, "r8a.2xlarge"
  ; R8a_xlarge, "r8a.xlarge"
  ; R8a_large, "r8a.large"
  ; R8a_medium, "r8a.medium"
  ; Trn2_3xlarge, "trn2.3xlarge"
  ; M8a_metal_48xl, "m8a.metal-48xl"
  ; M8a_metal_24xl, "m8a.metal-24xl"
  ; M8a_48xlarge, "m8a.48xlarge"
  ; M8a_24xlarge, "m8a.24xlarge"
  ; M8a_16xlarge, "m8a.16xlarge"
  ; M8a_12xlarge, "m8a.12xlarge"
  ; M8a_8xlarge, "m8a.8xlarge"
  ; M8a_4xlarge, "m8a.4xlarge"
  ; M8a_2xlarge, "m8a.2xlarge"
  ; M8a_xlarge, "m8a.xlarge"
  ; M8a_large, "m8a.large"
  ; M8a_medium, "m8a.medium"
  ; R8gb_metal_24xl, "r8gb.metal-24xl"
  ; R8gb_24xlarge, "r8gb.24xlarge"
  ; R8gb_16xlarge, "r8gb.16xlarge"
  ; R8gb_12xlarge, "r8gb.12xlarge"
  ; R8gb_8xlarge, "r8gb.8xlarge"
  ; R8gb_4xlarge, "r8gb.4xlarge"
  ; R8gb_2xlarge, "r8gb.2xlarge"
  ; R8gb_xlarge, "r8gb.xlarge"
  ; R8gb_large, "r8gb.large"
  ; R8gb_medium, "r8gb.medium"
  ; C8i_flex_16xlarge, "c8i-flex.16xlarge"
  ; C8i_flex_12xlarge, "c8i-flex.12xlarge"
  ; C8i_flex_8xlarge, "c8i-flex.8xlarge"
  ; C8i_flex_4xlarge, "c8i-flex.4xlarge"
  ; C8i_flex_2xlarge, "c8i-flex.2xlarge"
  ; C8i_flex_xlarge, "c8i-flex.xlarge"
  ; C8i_flex_large, "c8i-flex.large"
  ; C8i_metal_96xl, "c8i.metal-96xl"
  ; C8i_metal_48xl, "c8i.metal-48xl"
  ; C8i_96xlarge, "c8i.96xlarge"
  ; C8i_48xlarge, "c8i.48xlarge"
  ; C8i_32xlarge, "c8i.32xlarge"
  ; C8i_24xlarge, "c8i.24xlarge"
  ; C8i_16xlarge, "c8i.16xlarge"
  ; C8i_12xlarge, "c8i.12xlarge"
  ; C8i_8xlarge, "c8i.8xlarge"
  ; C8i_4xlarge, "c8i.4xlarge"
  ; C8i_2xlarge, "c8i.2xlarge"
  ; C8i_xlarge, "c8i.xlarge"
  ; C8i_large, "c8i.large"
  ; R8gn_metal_48xl, "r8gn.metal-48xl"
  ; R8gn_metal_24xl, "r8gn.metal-24xl"
  ; R8gn_48xlarge, "r8gn.48xlarge"
  ; R8gn_24xlarge, "r8gn.24xlarge"
  ; R8gn_16xlarge, "r8gn.16xlarge"
  ; R8gn_12xlarge, "r8gn.12xlarge"
  ; R8gn_8xlarge, "r8gn.8xlarge"
  ; R8gn_4xlarge, "r8gn.4xlarge"
  ; R8gn_2xlarge, "r8gn.2xlarge"
  ; R8gn_xlarge, "r8gn.xlarge"
  ; R8gn_large, "r8gn.large"
  ; R8gn_medium, "r8gn.medium"
  ; Mac_m4pro_metal, "mac-m4pro.metal"
  ; Mac_m4_metal, "mac-m4.metal"
  ; I8ge_metal_48xl, "i8ge.metal-48xl"
  ; I8ge_metal_24xl, "i8ge.metal-24xl"
  ; I8ge_48xlarge, "i8ge.48xlarge"
  ; I8ge_24xlarge, "i8ge.24xlarge"
  ; I8ge_18xlarge, "i8ge.18xlarge"
  ; I8ge_12xlarge, "i8ge.12xlarge"
  ; I8ge_6xlarge, "i8ge.6xlarge"
  ; I8ge_3xlarge, "i8ge.3xlarge"
  ; I8ge_2xlarge, "i8ge.2xlarge"
  ; I8ge_xlarge, "i8ge.xlarge"
  ; I8ge_large, "i8ge.large"
  ; M8i_flex_16xlarge, "m8i-flex.16xlarge"
  ; M8i_flex_12xlarge, "m8i-flex.12xlarge"
  ; M8i_flex_8xlarge, "m8i-flex.8xlarge"
  ; M8i_flex_4xlarge, "m8i-flex.4xlarge"
  ; M8i_flex_2xlarge, "m8i-flex.2xlarge"
  ; M8i_flex_xlarge, "m8i-flex.xlarge"
  ; M8i_flex_large, "m8i-flex.large"
  ; M8i_metal_96xl, "m8i.metal-96xl"
  ; M8i_metal_48xl, "m8i.metal-48xl"
  ; M8i_96xlarge, "m8i.96xlarge"
  ; M8i_48xlarge, "m8i.48xlarge"
  ; M8i_32xlarge, "m8i.32xlarge"
  ; M8i_24xlarge, "m8i.24xlarge"
  ; M8i_16xlarge, "m8i.16xlarge"
  ; M8i_12xlarge, "m8i.12xlarge"
  ; M8i_8xlarge, "m8i.8xlarge"
  ; M8i_4xlarge, "m8i.4xlarge"
  ; M8i_2xlarge, "m8i.2xlarge"
  ; M8i_xlarge, "m8i.xlarge"
  ; M8i_large, "m8i.large"
  ; R8i_flex_16xlarge, "r8i-flex.16xlarge"
  ; R8i_flex_12xlarge, "r8i-flex.12xlarge"
  ; R8i_flex_8xlarge, "r8i-flex.8xlarge"
  ; R8i_flex_4xlarge, "r8i-flex.4xlarge"
  ; R8i_flex_2xlarge, "r8i-flex.2xlarge"
  ; R8i_flex_xlarge, "r8i-flex.xlarge"
  ; R8i_flex_large, "r8i-flex.large"
  ; R8i_metal_96xl, "r8i.metal-96xl"
  ; R8i_metal_48xl, "r8i.metal-48xl"
  ; R8i_96xlarge, "r8i.96xlarge"
  ; R8i_48xlarge, "r8i.48xlarge"
  ; R8i_32xlarge, "r8i.32xlarge"
  ; R8i_24xlarge, "r8i.24xlarge"
  ; R8i_16xlarge, "r8i.16xlarge"
  ; R8i_12xlarge, "r8i.12xlarge"
  ; R8i_8xlarge, "r8i.8xlarge"
  ; R8i_4xlarge, "r8i.4xlarge"
  ; R8i_2xlarge, "r8i.2xlarge"
  ; R8i_xlarge, "r8i.xlarge"
  ; R8i_large, "r8i.large"
  ; P5_4xlarge, "p5.4xlarge"
  ; Gr6f_4xlarge, "gr6f.4xlarge"
  ; G6f_4xlarge, "g6f.4xlarge"
  ; G6f_2xlarge, "g6f.2xlarge"
  ; G6f_xlarge, "g6f.xlarge"
  ; G6f_large, "g6f.large"
  ; P6e_gb200_36xlarge, "p6e-gb200.36xlarge"
  ; F2_6xlarge, "f2.6xlarge"
  ; C8gn_metal_48xl, "c8gn.metal-48xl"
  ; C8gn_metal_24xl, "c8gn.metal-24xl"
  ; C8gn_48xlarge, "c8gn.48xlarge"
  ; C8gn_24xlarge, "c8gn.24xlarge"
  ; C8gn_16xlarge, "c8gn.16xlarge"
  ; C8gn_12xlarge, "c8gn.12xlarge"
  ; C8gn_8xlarge, "c8gn.8xlarge"
  ; C8gn_4xlarge, "c8gn.4xlarge"
  ; C8gn_2xlarge, "c8gn.2xlarge"
  ; C8gn_xlarge, "c8gn.xlarge"
  ; C8gn_large, "c8gn.large"
  ; C8gn_medium, "c8gn.medium"
  ; R8gd_metal_48xl, "r8gd.metal-48xl"
  ; R8gd_metal_24xl, "r8gd.metal-24xl"
  ; R8gd_48xlarge, "r8gd.48xlarge"
  ; R8gd_24xlarge, "r8gd.24xlarge"
  ; R8gd_16xlarge, "r8gd.16xlarge"
  ; R8gd_12xlarge, "r8gd.12xlarge"
  ; R8gd_8xlarge, "r8gd.8xlarge"
  ; R8gd_4xlarge, "r8gd.4xlarge"
  ; R8gd_2xlarge, "r8gd.2xlarge"
  ; R8gd_xlarge, "r8gd.xlarge"
  ; R8gd_large, "r8gd.large"
  ; R8gd_medium, "r8gd.medium"
  ; M8gd_metal_48xl, "m8gd.metal-48xl"
  ; M8gd_metal_24xl, "m8gd.metal-24xl"
  ; M8gd_48xlarge, "m8gd.48xlarge"
  ; M8gd_24xlarge, "m8gd.24xlarge"
  ; M8gd_16xlarge, "m8gd.16xlarge"
  ; M8gd_12xlarge, "m8gd.12xlarge"
  ; M8gd_8xlarge, "m8gd.8xlarge"
  ; M8gd_4xlarge, "m8gd.4xlarge"
  ; M8gd_2xlarge, "m8gd.2xlarge"
  ; M8gd_xlarge, "m8gd.xlarge"
  ; M8gd_large, "m8gd.large"
  ; M8gd_medium, "m8gd.medium"
  ; P6_b200_48xlarge, "p6-b200.48xlarge"
  ; I7i_metal_48xl, "i7i.metal-48xl"
  ; I7i_metal_24xl, "i7i.metal-24xl"
  ; I7i_48xlarge, "i7i.48xlarge"
  ; I7i_24xlarge, "i7i.24xlarge"
  ; I7i_16xlarge, "i7i.16xlarge"
  ; I7i_12xlarge, "i7i.12xlarge"
  ; I7i_8xlarge, "i7i.8xlarge"
  ; I7i_4xlarge, "i7i.4xlarge"
  ; I7i_2xlarge, "i7i.2xlarge"
  ; I7i_xlarge, "i7i.xlarge"
  ; I7i_large, "i7i.large"
  ; C8gd_metal_48xl, "c8gd.metal-48xl"
  ; C8gd_metal_24xl, "c8gd.metal-24xl"
  ; C8gd_48xlarge, "c8gd.48xlarge"
  ; C8gd_24xlarge, "c8gd.24xlarge"
  ; C8gd_16xlarge, "c8gd.16xlarge"
  ; C8gd_12xlarge, "c8gd.12xlarge"
  ; C8gd_8xlarge, "c8gd.8xlarge"
  ; C8gd_4xlarge, "c8gd.4xlarge"
  ; C8gd_2xlarge, "c8gd.2xlarge"
  ; C8gd_xlarge, "c8gd.xlarge"
  ; C8gd_large, "c8gd.large"
  ; C8gd_medium, "c8gd.medium"
  ; I8g_48xlarge, "i8g.48xlarge"
  ; I7ie_metal_48xl, "i7ie.metal-48xl"
  ; I7ie_metal_24xl, "i7ie.metal-24xl"
  ; M7i_flex_16xlarge, "m7i-flex.16xlarge"
  ; M7i_flex_12xlarge, "m7i-flex.12xlarge"
  ; C7i_flex_16xlarge, "c7i-flex.16xlarge"
  ; C7i_flex_12xlarge, "c7i-flex.12xlarge"
  ; Trn2_48xlarge, "trn2.48xlarge"
  ; F2_48xlarge, "f2.48xlarge"
  ; F2_12xlarge, "f2.12xlarge"
  ; P5en_48xlarge, "p5en.48xlarge"
  ; P5e_48xlarge, "p5e.48xlarge"
  ; U7inh_32tb_480xlarge, "u7inh-32tb.480xlarge"
  ; U7i_8tb_112xlarge, "u7i-8tb.112xlarge"
  ; U7i_6tb_112xlarge, "u7i-6tb.112xlarge"
  ; I8g_metal_24xl, "i8g.metal-24xl"
  ; I8g_24xlarge, "i8g.24xlarge"
  ; I8g_16xlarge, "i8g.16xlarge"
  ; I8g_12xlarge, "i8g.12xlarge"
  ; I8g_8xlarge, "i8g.8xlarge"
  ; I8g_4xlarge, "i8g.4xlarge"
  ; I8g_2xlarge, "i8g.2xlarge"
  ; I8g_xlarge, "i8g.xlarge"
  ; I8g_large, "i8g.large"
  ; I7ie_48xlarge, "i7ie.48xlarge"
  ; I7ie_24xlarge, "i7ie.24xlarge"
  ; I7ie_18xlarge, "i7ie.18xlarge"
  ; I7ie_12xlarge, "i7ie.12xlarge"
  ; I7ie_6xlarge, "i7ie.6xlarge"
  ; I7ie_3xlarge, "i7ie.3xlarge"
  ; I7ie_2xlarge, "i7ie.2xlarge"
  ; I7ie_xlarge, "i7ie.xlarge"
  ; I7ie_large, "i7ie.large"
  ; X8g_metal_48xl, "x8g.metal-48xl"
  ; X8g_metal_24xl, "x8g.metal-24xl"
  ; X8g_48xlarge, "x8g.48xlarge"
  ; X8g_24xlarge, "x8g.24xlarge"
  ; X8g_16xlarge, "x8g.16xlarge"
  ; X8g_12xlarge, "x8g.12xlarge"
  ; X8g_8xlarge, "x8g.8xlarge"
  ; X8g_4xlarge, "x8g.4xlarge"
  ; X8g_2xlarge, "x8g.2xlarge"
  ; X8g_xlarge, "x8g.xlarge"
  ; X8g_large, "x8g.large"
  ; X8g_medium, "x8g.medium"
  ; M8g_metal_48xl, "m8g.metal-48xl"
  ; M8g_metal_24xl, "m8g.metal-24xl"
  ; M8g_48xlarge, "m8g.48xlarge"
  ; M8g_24xlarge, "m8g.24xlarge"
  ; M8g_16xlarge, "m8g.16xlarge"
  ; M8g_12xlarge, "m8g.12xlarge"
  ; M8g_8xlarge, "m8g.8xlarge"
  ; M8g_4xlarge, "m8g.4xlarge"
  ; M8g_2xlarge, "m8g.2xlarge"
  ; M8g_xlarge, "m8g.xlarge"
  ; M8g_large, "m8g.large"
  ; M8g_medium, "m8g.medium"
  ; C8g_metal_48xl, "c8g.metal-48xl"
  ; C8g_metal_24xl, "c8g.metal-24xl"
  ; C8g_48xlarge, "c8g.48xlarge"
  ; C8g_24xlarge, "c8g.24xlarge"
  ; C8g_16xlarge, "c8g.16xlarge"
  ; C8g_12xlarge, "c8g.12xlarge"
  ; C8g_8xlarge, "c8g.8xlarge"
  ; C8g_4xlarge, "c8g.4xlarge"
  ; C8g_2xlarge, "c8g.2xlarge"
  ; C8g_xlarge, "c8g.xlarge"
  ; C8g_large, "c8g.large"
  ; C8g_medium, "c8g.medium"
  ; G6e_48xlarge, "g6e.48xlarge"
  ; G6e_24xlarge, "g6e.24xlarge"
  ; G6e_16xlarge, "g6e.16xlarge"
  ; G6e_12xlarge, "g6e.12xlarge"
  ; G6e_8xlarge, "g6e.8xlarge"
  ; G6e_4xlarge, "g6e.4xlarge"
  ; G6e_2xlarge, "g6e.2xlarge"
  ; G6e_xlarge, "g6e.xlarge"
  ; Mac2_m1ultra_metal, "mac2-m1ultra.metal"
  ; R8g_metal_48xl, "r8g.metal-48xl"
  ; R8g_metal_24xl, "r8g.metal-24xl"
  ; R8g_48xlarge, "r8g.48xlarge"
  ; R8g_24xlarge, "r8g.24xlarge"
  ; R8g_16xlarge, "r8g.16xlarge"
  ; R8g_12xlarge, "r8g.12xlarge"
  ; R8g_8xlarge, "r8g.8xlarge"
  ; R8g_4xlarge, "r8g.4xlarge"
  ; R8g_2xlarge, "r8g.2xlarge"
  ; R8g_xlarge, "r8g.xlarge"
  ; R8g_large, "r8g.large"
  ; R8g_medium, "r8g.medium"
  ; C7gn_metal, "c7gn.metal"
  ; U7ib_12tb_224xlarge, "u7ib-12tb.224xlarge"
  ; U7in_32tb_224xlarge, "u7in-32tb.224xlarge"
  ; U7in_24tb_224xlarge, "u7in-24tb.224xlarge"
  ; U7in_16tb_224xlarge, "u7in-16tb.224xlarge"
  ; U7i_12tb_224xlarge, "u7i-12tb.224xlarge"
  ; C7i_flex_8xlarge, "c7i-flex.8xlarge"
  ; C7i_flex_4xlarge, "c7i-flex.4xlarge"
  ; C7i_flex_2xlarge, "c7i-flex.2xlarge"
  ; C7i_flex_xlarge, "c7i-flex.xlarge"
  ; C7i_flex_large, "c7i-flex.large"
  ; Gr6_8xlarge, "gr6.8xlarge"
  ; Gr6_4xlarge, "gr6.4xlarge"
  ; G6_48xlarge, "g6.48xlarge"
  ; G6_24xlarge, "g6.24xlarge"
  ; G6_16xlarge, "g6.16xlarge"
  ; G6_12xlarge, "g6.12xlarge"
  ; G6_8xlarge, "g6.8xlarge"
  ; G6_4xlarge, "g6.4xlarge"
  ; G6_2xlarge, "g6.2xlarge"
  ; G6_xlarge, "g6.xlarge"
  ; R7gd_metal, "r7gd.metal"
  ; M7gd_metal, "m7gd.metal"
  ; C7gd_metal, "c7gd.metal"
  ; R7iz_metal_32xl, "r7iz.metal-32xl"
  ; R7iz_metal_16xl, "r7iz.metal-16xl"
  ; R7i_metal_48xl, "r7i.metal-48xl"
  ; R7i_metal_24xl, "r7i.metal-24xl"
  ; M7i_metal_48xl, "m7i.metal-48xl"
  ; M7i_metal_24xl, "m7i.metal-24xl"
  ; C7i_metal_48xl, "c7i.metal-48xl"
  ; C7i_metal_24xl, "c7i.metal-24xl"
  ; I4i_24xlarge, "i4i.24xlarge"
  ; I4i_12xlarge, "i4i.12xlarge"
  ; Mac2_m2_metal, "mac2-m2.metal"
  ; Dl2q_24xlarge, "dl2q.24xlarge"
  ; R7i_48xlarge, "r7i.48xlarge"
  ; R7i_24xlarge, "r7i.24xlarge"
  ; R7i_16xlarge, "r7i.16xlarge"
  ; R7i_12xlarge, "r7i.12xlarge"
  ; R7i_8xlarge, "r7i.8xlarge"
  ; R7i_4xlarge, "r7i.4xlarge"
  ; R7i_2xlarge, "r7i.2xlarge"
  ; R7i_xlarge, "r7i.xlarge"
  ; R7i_large, "r7i.large"
  ; R7a_metal_48xl, "r7a.metal-48xl"
  ; C7a_metal_48xl, "c7a.metal-48xl"
  ; C7a_48xlarge, "c7a.48xlarge"
  ; C7a_32xlarge, "c7a.32xlarge"
  ; C7a_24xlarge, "c7a.24xlarge"
  ; C7a_16xlarge, "c7a.16xlarge"
  ; C7a_12xlarge, "c7a.12xlarge"
  ; C7a_8xlarge, "c7a.8xlarge"
  ; C7a_4xlarge, "c7a.4xlarge"
  ; C7a_2xlarge, "c7a.2xlarge"
  ; C7a_xlarge, "c7a.xlarge"
  ; C7a_large, "c7a.large"
  ; C7a_medium, "c7a.medium"
  ; R7iz_32xlarge, "r7iz.32xlarge"
  ; R7iz_16xlarge, "r7iz.16xlarge"
  ; R7iz_12xlarge, "r7iz.12xlarge"
  ; R7iz_8xlarge, "r7iz.8xlarge"
  ; R7iz_4xlarge, "r7iz.4xlarge"
  ; R7iz_2xlarge, "r7iz.2xlarge"
  ; R7iz_xlarge, "r7iz.xlarge"
  ; R7iz_large, "r7iz.large"
  ; Mac2_m2pro_metal, "mac2-m2pro.metal"
  ; C7i_48xlarge, "c7i.48xlarge"
  ; C7i_24xlarge, "c7i.24xlarge"
  ; C7i_16xlarge, "c7i.16xlarge"
  ; C7i_12xlarge, "c7i.12xlarge"
  ; C7i_8xlarge, "c7i.8xlarge"
  ; C7i_4xlarge, "c7i.4xlarge"
  ; C7i_2xlarge, "c7i.2xlarge"
  ; C7i_xlarge, "c7i.xlarge"
  ; C7i_large, "c7i.large"
  ; R7a_48xlarge, "r7a.48xlarge"
  ; R7a_32xlarge, "r7a.32xlarge"
  ; R7a_24xlarge, "r7a.24xlarge"
  ; R7a_16xlarge, "r7a.16xlarge"
  ; R7a_12xlarge, "r7a.12xlarge"
  ; R7a_8xlarge, "r7a.8xlarge"
  ; R7a_4xlarge, "r7a.4xlarge"
  ; R7a_2xlarge, "r7a.2xlarge"
  ; R7a_xlarge, "r7a.xlarge"
  ; R7a_large, "r7a.large"
  ; R7a_medium, "r7a.medium"
  ; R7gd_16xlarge, "r7gd.16xlarge"
  ; R7gd_12xlarge, "r7gd.12xlarge"
  ; R7gd_8xlarge, "r7gd.8xlarge"
  ; R7gd_4xlarge, "r7gd.4xlarge"
  ; R7gd_2xlarge, "r7gd.2xlarge"
  ; R7gd_xlarge, "r7gd.xlarge"
  ; R7gd_large, "r7gd.large"
  ; R7gd_medium, "r7gd.medium"
  ; M7gd_16xlarge, "m7gd.16xlarge"
  ; M7gd_12xlarge, "m7gd.12xlarge"
  ; M7gd_8xlarge, "m7gd.8xlarge"
  ; M7gd_4xlarge, "m7gd.4xlarge"
  ; M7gd_2xlarge, "m7gd.2xlarge"
  ; M7gd_xlarge, "m7gd.xlarge"
  ; M7gd_large, "m7gd.large"
  ; M7gd_medium, "m7gd.medium"
  ; C7gd_16xlarge, "c7gd.16xlarge"
  ; C7gd_12xlarge, "c7gd.12xlarge"
  ; C7gd_8xlarge, "c7gd.8xlarge"
  ; C7gd_4xlarge, "c7gd.4xlarge"
  ; C7gd_2xlarge, "c7gd.2xlarge"
  ; C7gd_xlarge, "c7gd.xlarge"
  ; C7gd_large, "c7gd.large"
  ; C7gd_medium, "c7gd.medium"
  ; Hpc7a_96xlarge, "hpc7a.96xlarge"
  ; Hpc7a_48xlarge, "hpc7a.48xlarge"
  ; Hpc7a_24xlarge, "hpc7a.24xlarge"
  ; Hpc7a_12xlarge, "hpc7a.12xlarge"
  ; M7a_metal_48xl, "m7a.metal-48xl"
  ; M7a_48xlarge, "m7a.48xlarge"
  ; M7a_32xlarge, "m7a.32xlarge"
  ; M7a_24xlarge, "m7a.24xlarge"
  ; M7a_16xlarge, "m7a.16xlarge"
  ; M7a_12xlarge, "m7a.12xlarge"
  ; M7a_8xlarge, "m7a.8xlarge"
  ; M7a_4xlarge, "m7a.4xlarge"
  ; M7a_2xlarge, "m7a.2xlarge"
  ; M7a_xlarge, "m7a.xlarge"
  ; M7a_large, "m7a.large"
  ; M7a_medium, "m7a.medium"
  ; M7i_flex_8xlarge, "m7i-flex.8xlarge"
  ; M7i_flex_4xlarge, "m7i-flex.4xlarge"
  ; M7i_flex_2xlarge, "m7i-flex.2xlarge"
  ; M7i_flex_xlarge, "m7i-flex.xlarge"
  ; M7i_flex_large, "m7i-flex.large"
  ; M7i_48xlarge, "m7i.48xlarge"
  ; M7i_24xlarge, "m7i.24xlarge"
  ; M7i_16xlarge, "m7i.16xlarge"
  ; M7i_12xlarge, "m7i.12xlarge"
  ; M7i_8xlarge, "m7i.8xlarge"
  ; M7i_4xlarge, "m7i.4xlarge"
  ; M7i_2xlarge, "m7i.2xlarge"
  ; M7i_xlarge, "m7i.xlarge"
  ; M7i_large, "m7i.large"
  ; P5_48xlarge, "p5.48xlarge"
  ; C7gn_16xlarge, "c7gn.16xlarge"
  ; C7gn_12xlarge, "c7gn.12xlarge"
  ; C7gn_8xlarge, "c7gn.8xlarge"
  ; C7gn_4xlarge, "c7gn.4xlarge"
  ; C7gn_2xlarge, "c7gn.2xlarge"
  ; C7gn_xlarge, "c7gn.xlarge"
  ; C7gn_large, "c7gn.large"
  ; C7gn_medium, "c7gn.medium"
  ; Hpc7g_16xlarge, "hpc7g.16xlarge"
  ; Hpc7g_8xlarge, "hpc7g.8xlarge"
  ; Hpc7g_4xlarge, "hpc7g.4xlarge"
  ; I4g_16xlarge, "i4g.16xlarge"
  ; I4g_8xlarge, "i4g.8xlarge"
  ; I4g_4xlarge, "i4g.4xlarge"
  ; I4g_2xlarge, "i4g.2xlarge"
  ; I4g_xlarge, "i4g.xlarge"
  ; I4g_large, "i4g.large"
  ; Trn1n_32xlarge, "trn1n.32xlarge"
  ; Inf2_48xlarge, "inf2.48xlarge"
  ; Inf2_24xlarge, "inf2.24xlarge"
  ; Inf2_8xlarge, "inf2.8xlarge"
  ; Inf2_xlarge, "inf2.xlarge"
  ; R6idn_metal, "r6idn.metal"
  ; R6in_metal, "r6in.metal"
  ; M6idn_metal, "m6idn.metal"
  ; M6in_metal, "m6in.metal"
  ; C6in_metal, "c6in.metal"
  ; R7g_metal, "r7g.metal"
  ; R7g_16xlarge, "r7g.16xlarge"
  ; R7g_12xlarge, "r7g.12xlarge"
  ; R7g_8xlarge, "r7g.8xlarge"
  ; R7g_4xlarge, "r7g.4xlarge"
  ; R7g_2xlarge, "r7g.2xlarge"
  ; R7g_xlarge, "r7g.xlarge"
  ; R7g_large, "r7g.large"
  ; R7g_medium, "r7g.medium"
  ; M7g_metal, "m7g.metal"
  ; M7g_16xlarge, "m7g.16xlarge"
  ; M7g_12xlarge, "m7g.12xlarge"
  ; M7g_8xlarge, "m7g.8xlarge"
  ; M7g_4xlarge, "m7g.4xlarge"
  ; M7g_2xlarge, "m7g.2xlarge"
  ; M7g_xlarge, "m7g.xlarge"
  ; M7g_large, "m7g.large"
  ; M7g_medium, "m7g.medium"
  ; C7g_metal, "c7g.metal"
  ; R6idn_32xlarge, "r6idn.32xlarge"
  ; R6idn_24xlarge, "r6idn.24xlarge"
  ; R6idn_16xlarge, "r6idn.16xlarge"
  ; R6idn_12xlarge, "r6idn.12xlarge"
  ; R6idn_8xlarge, "r6idn.8xlarge"
  ; R6idn_4xlarge, "r6idn.4xlarge"
  ; R6idn_2xlarge, "r6idn.2xlarge"
  ; R6idn_xlarge, "r6idn.xlarge"
  ; R6idn_large, "r6idn.large"
  ; R6in_32xlarge, "r6in.32xlarge"
  ; R6in_24xlarge, "r6in.24xlarge"
  ; R6in_16xlarge, "r6in.16xlarge"
  ; R6in_12xlarge, "r6in.12xlarge"
  ; R6in_8xlarge, "r6in.8xlarge"
  ; R6in_4xlarge, "r6in.4xlarge"
  ; R6in_2xlarge, "r6in.2xlarge"
  ; R6in_xlarge, "r6in.xlarge"
  ; R6in_large, "r6in.large"
  ; M6idn_32xlarge, "m6idn.32xlarge"
  ; M6idn_24xlarge, "m6idn.24xlarge"
  ; M6idn_16xlarge, "m6idn.16xlarge"
  ; M6idn_12xlarge, "m6idn.12xlarge"
  ; M6idn_8xlarge, "m6idn.8xlarge"
  ; M6idn_4xlarge, "m6idn.4xlarge"
  ; M6idn_2xlarge, "m6idn.2xlarge"
  ; M6idn_xlarge, "m6idn.xlarge"
  ; M6idn_large, "m6idn.large"
  ; M6in_32xlarge, "m6in.32xlarge"
  ; M6in_24xlarge, "m6in.24xlarge"
  ; M6in_16xlarge, "m6in.16xlarge"
  ; M6in_12xlarge, "m6in.12xlarge"
  ; M6in_8xlarge, "m6in.8xlarge"
  ; M6in_4xlarge, "m6in.4xlarge"
  ; M6in_2xlarge, "m6in.2xlarge"
  ; M6in_xlarge, "m6in.xlarge"
  ; M6in_large, "m6in.large"
  ; C6in_32xlarge, "c6in.32xlarge"
  ; C6in_24xlarge, "c6in.24xlarge"
  ; C6in_16xlarge, "c6in.16xlarge"
  ; C6in_12xlarge, "c6in.12xlarge"
  ; C6in_8xlarge, "c6in.8xlarge"
  ; C6in_4xlarge, "c6in.4xlarge"
  ; C6in_2xlarge, "c6in.2xlarge"
  ; C6in_xlarge, "c6in.xlarge"
  ; C6in_large, "c6in.large"
  ; Hpc6id_32xlarge, "hpc6id.32xlarge"
  ; Trn1_32xlarge, "trn1.32xlarge"
  ; Trn1_2xlarge, "trn1.2xlarge"
  ; U_24tb1_112xlarge, "u-24tb1.112xlarge"
  ; U_18tb1_112xlarge, "u-18tb1.112xlarge"
  ; U_3tb1_56xlarge, "u-3tb1.56xlarge"
  ; P4de_24xlarge, "p4de.24xlarge"
  ; R6a_metal, "r6a.metal"
  ; R6a_48xlarge, "r6a.48xlarge"
  ; R6a_32xlarge, "r6a.32xlarge"
  ; R6a_24xlarge, "r6a.24xlarge"
  ; R6a_16xlarge, "r6a.16xlarge"
  ; R6a_12xlarge, "r6a.12xlarge"
  ; R6a_8xlarge, "r6a.8xlarge"
  ; R6a_4xlarge, "r6a.4xlarge"
  ; R6a_2xlarge, "r6a.2xlarge"
  ; R6a_xlarge, "r6a.xlarge"
  ; R6a_large, "r6a.large"
  ; R6id_metal, "r6id.metal"
  ; R6id_32xlarge, "r6id.32xlarge"
  ; R6id_24xlarge, "r6id.24xlarge"
  ; R6id_16xlarge, "r6id.16xlarge"
  ; R6id_12xlarge, "r6id.12xlarge"
  ; R6id_8xlarge, "r6id.8xlarge"
  ; R6id_4xlarge, "r6id.4xlarge"
  ; R6id_2xlarge, "r6id.2xlarge"
  ; R6id_xlarge, "r6id.xlarge"
  ; R6id_large, "r6id.large"
  ; M6id_metal, "m6id.metal"
  ; M6id_32xlarge, "m6id.32xlarge"
  ; M6id_24xlarge, "m6id.24xlarge"
  ; M6id_16xlarge, "m6id.16xlarge"
  ; M6id_12xlarge, "m6id.12xlarge"
  ; M6id_8xlarge, "m6id.8xlarge"
  ; M6id_4xlarge, "m6id.4xlarge"
  ; M6id_2xlarge, "m6id.2xlarge"
  ; M6id_xlarge, "m6id.xlarge"
  ; M6id_large, "m6id.large"
  ; C6id_metal, "c6id.metal"
  ; C6id_32xlarge, "c6id.32xlarge"
  ; C6id_24xlarge, "c6id.24xlarge"
  ; C6id_16xlarge, "c6id.16xlarge"
  ; C6id_12xlarge, "c6id.12xlarge"
  ; C6id_8xlarge, "c6id.8xlarge"
  ; C6id_4xlarge, "c6id.4xlarge"
  ; C6id_2xlarge, "c6id.2xlarge"
  ; C6id_xlarge, "c6id.xlarge"
  ; C6id_large, "c6id.large"
  ; Mac2_metal, "mac2.metal"
  ; C7g_16xlarge, "c7g.16xlarge"
  ; C7g_12xlarge, "c7g.12xlarge"
  ; C7g_8xlarge, "c7g.8xlarge"
  ; C7g_4xlarge, "c7g.4xlarge"
  ; C7g_2xlarge, "c7g.2xlarge"
  ; C7g_xlarge, "c7g.xlarge"
  ; C7g_large, "c7g.large"
  ; C7g_medium, "c7g.medium"
  ; X2iedn_metal, "x2iedn.metal"
  ; X2idn_metal, "x2idn.metal"
  ; I4i_metal, "i4i.metal"
  ; I4i_32xlarge, "i4i.32xlarge"
  ; I4i_16xlarge, "i4i.16xlarge"
  ; I4i_8xlarge, "i4i.8xlarge"
  ; I4i_4xlarge, "i4i.4xlarge"
  ; I4i_2xlarge, "i4i.2xlarge"
  ; I4i_xlarge, "i4i.xlarge"
  ; I4i_large, "i4i.large"
  ; M6a_metal, "m6a.metal"
  ; C6a_metal, "c6a.metal"
  ; C6a_48xlarge, "c6a.48xlarge"
  ; C6a_32xlarge, "c6a.32xlarge"
  ; C6a_24xlarge, "c6a.24xlarge"
  ; C6a_16xlarge, "c6a.16xlarge"
  ; C6a_12xlarge, "c6a.12xlarge"
  ; C6a_8xlarge, "c6a.8xlarge"
  ; C6a_4xlarge, "c6a.4xlarge"
  ; C6a_2xlarge, "c6a.2xlarge"
  ; C6a_xlarge, "c6a.xlarge"
  ; C6a_large, "c6a.large"
  ; X2iedn_32xlarge, "x2iedn.32xlarge"
  ; X2iedn_24xlarge, "x2iedn.24xlarge"
  ; X2iedn_16xlarge, "x2iedn.16xlarge"
  ; X2iedn_8xlarge, "x2iedn.8xlarge"
  ; X2iedn_4xlarge, "x2iedn.4xlarge"
  ; X2iedn_2xlarge, "x2iedn.2xlarge"
  ; X2iedn_xlarge, "x2iedn.xlarge"
  ; X2idn_32xlarge, "x2idn.32xlarge"
  ; X2idn_24xlarge, "x2idn.24xlarge"
  ; X2idn_16xlarge, "x2idn.16xlarge"
  ; Z1d_metal, "z1d.metal"
  ; Z1d_12xlarge, "z1d.12xlarge"
  ; Z1d_6xlarge, "z1d.6xlarge"
  ; Z1d_3xlarge, "z1d.3xlarge"
  ; Z1d_2xlarge, "z1d.2xlarge"
  ; Z1d_xlarge, "z1d.xlarge"
  ; Z1d_large, "z1d.large"
  ; X2gd_metal, "x2gd.metal"
  ; X2gd_16xlarge, "x2gd.16xlarge"
  ; X2gd_12xlarge, "x2gd.12xlarge"
  ; X2gd_8xlarge, "x2gd.8xlarge"
  ; X2gd_4xlarge, "x2gd.4xlarge"
  ; X2gd_2xlarge, "x2gd.2xlarge"
  ; X2gd_xlarge, "x2gd.xlarge"
  ; X2gd_large, "x2gd.large"
  ; X2gd_medium, "x2gd.medium"
  ; X2iezn_metal, "x2iezn.metal"
  ; X2iezn_12xlarge, "x2iezn.12xlarge"
  ; X2iezn_8xlarge, "x2iezn.8xlarge"
  ; X2iezn_6xlarge, "x2iezn.6xlarge"
  ; X2iezn_4xlarge, "x2iezn.4xlarge"
  ; X2iezn_2xlarge, "x2iezn.2xlarge"
  ; X1e_32xlarge, "x1e.32xlarge"
  ; X1e_16xlarge, "x1e.16xlarge"
  ; X1e_8xlarge, "x1e.8xlarge"
  ; X1e_4xlarge, "x1e.4xlarge"
  ; X1e_2xlarge, "x1e.2xlarge"
  ; X1e_xlarge, "x1e.xlarge"
  ; X1_32xlarge, "x1.32xlarge"
  ; X1_16xlarge, "x1.16xlarge"
  ; Vt1_24xlarge, "vt1.24xlarge"
  ; Vt1_6xlarge, "vt1.6xlarge"
  ; Vt1_3xlarge, "vt1.3xlarge"
  ; U_24tb1_metal, "u-24tb1.metal"
  ; U_18tb1_metal, "u-18tb1.metal"
  ; U_12tb1_metal, "u-12tb1.metal"
  ; U_9tb1_metal, "u-9tb1.metal"
  ; U_6tb1_metal, "u-6tb1.metal"
  ; U_12tb1_112xlarge, "u-12tb1.112xlarge"
  ; U_9tb1_112xlarge, "u-9tb1.112xlarge"
  ; U_6tb1_112xlarge, "u-6tb1.112xlarge"
  ; U_6tb1_56xlarge, "u-6tb1.56xlarge"
  ; T4g_2xlarge, "t4g.2xlarge"
  ; T4g_xlarge, "t4g.xlarge"
  ; T4g_large, "t4g.large"
  ; T4g_medium, "t4g.medium"
  ; T4g_small, "t4g.small"
  ; T4g_micro, "t4g.micro"
  ; T4g_nano, "t4g.nano"
  ; T3a_2xlarge, "t3a.2xlarge"
  ; T3a_xlarge, "t3a.xlarge"
  ; T3a_large, "t3a.large"
  ; T3a_medium, "t3a.medium"
  ; T3a_small, "t3a.small"
  ; T3a_micro, "t3a.micro"
  ; T3a_nano, "t3a.nano"
  ; T3_2xlarge, "t3.2xlarge"
  ; T3_xlarge, "t3.xlarge"
  ; T3_large, "t3.large"
  ; T3_medium, "t3.medium"
  ; T3_small, "t3.small"
  ; T3_micro, "t3.micro"
  ; T3_nano, "t3.nano"
  ; T2_2xlarge, "t2.2xlarge"
  ; T2_xlarge, "t2.xlarge"
  ; T2_large, "t2.large"
  ; T2_medium, "t2.medium"
  ; T2_small, "t2.small"
  ; T2_micro, "t2.micro"
  ; T2_nano, "t2.nano"
  ; T1_micro, "t1.micro"
  ; R6i_metal, "r6i.metal"
  ; R6i_32xlarge, "r6i.32xlarge"
  ; R6i_24xlarge, "r6i.24xlarge"
  ; R6i_16xlarge, "r6i.16xlarge"
  ; R6i_12xlarge, "r6i.12xlarge"
  ; R6i_8xlarge, "r6i.8xlarge"
  ; R6i_4xlarge, "r6i.4xlarge"
  ; R6i_2xlarge, "r6i.2xlarge"
  ; R6i_xlarge, "r6i.xlarge"
  ; R6i_large, "r6i.large"
  ; R6gd_metal, "r6gd.metal"
  ; R6gd_16xlarge, "r6gd.16xlarge"
  ; R6gd_12xlarge, "r6gd.12xlarge"
  ; R6gd_8xlarge, "r6gd.8xlarge"
  ; R6gd_4xlarge, "r6gd.4xlarge"
  ; R6gd_2xlarge, "r6gd.2xlarge"
  ; R6gd_xlarge, "r6gd.xlarge"
  ; R6gd_large, "r6gd.large"
  ; R6gd_medium, "r6gd.medium"
  ; R6g_metal, "r6g.metal"
  ; R6g_16xlarge, "r6g.16xlarge"
  ; R6g_12xlarge, "r6g.12xlarge"
  ; R6g_8xlarge, "r6g.8xlarge"
  ; R6g_4xlarge, "r6g.4xlarge"
  ; R6g_2xlarge, "r6g.2xlarge"
  ; R6g_xlarge, "r6g.xlarge"
  ; R6g_large, "r6g.large"
  ; R6g_medium, "r6g.medium"
  ; R5n_metal, "r5n.metal"
  ; R5n_24xlarge, "r5n.24xlarge"
  ; R5n_16xlarge, "r5n.16xlarge"
  ; R5n_12xlarge, "r5n.12xlarge"
  ; R5n_8xlarge, "r5n.8xlarge"
  ; R5n_4xlarge, "r5n.4xlarge"
  ; R5n_2xlarge, "r5n.2xlarge"
  ; R5n_xlarge, "r5n.xlarge"
  ; R5n_large, "r5n.large"
  ; R5dn_metal, "r5dn.metal"
  ; R5dn_24xlarge, "r5dn.24xlarge"
  ; R5dn_16xlarge, "r5dn.16xlarge"
  ; R5dn_12xlarge, "r5dn.12xlarge"
  ; R5dn_8xlarge, "r5dn.8xlarge"
  ; R5dn_4xlarge, "r5dn.4xlarge"
  ; R5dn_2xlarge, "r5dn.2xlarge"
  ; R5dn_xlarge, "r5dn.xlarge"
  ; R5dn_large, "r5dn.large"
  ; R5d_metal, "r5d.metal"
  ; R5d_24xlarge, "r5d.24xlarge"
  ; R5d_16xlarge, "r5d.16xlarge"
  ; R5d_12xlarge, "r5d.12xlarge"
  ; R5d_8xlarge, "r5d.8xlarge"
  ; R5d_4xlarge, "r5d.4xlarge"
  ; R5d_2xlarge, "r5d.2xlarge"
  ; R5d_xlarge, "r5d.xlarge"
  ; R5d_large, "r5d.large"
  ; R5b_metal, "r5b.metal"
  ; R5b_24xlarge, "r5b.24xlarge"
  ; R5b_16xlarge, "r5b.16xlarge"
  ; R5b_12xlarge, "r5b.12xlarge"
  ; R5b_8xlarge, "r5b.8xlarge"
  ; R5b_4xlarge, "r5b.4xlarge"
  ; R5b_2xlarge, "r5b.2xlarge"
  ; R5b_xlarge, "r5b.xlarge"
  ; R5b_large, "r5b.large"
  ; R5ad_24xlarge, "r5ad.24xlarge"
  ; R5ad_16xlarge, "r5ad.16xlarge"
  ; R5ad_12xlarge, "r5ad.12xlarge"
  ; R5ad_8xlarge, "r5ad.8xlarge"
  ; R5ad_4xlarge, "r5ad.4xlarge"
  ; R5ad_2xlarge, "r5ad.2xlarge"
  ; R5ad_xlarge, "r5ad.xlarge"
  ; R5ad_large, "r5ad.large"
  ; R5a_24xlarge, "r5a.24xlarge"
  ; R5a_16xlarge, "r5a.16xlarge"
  ; R5a_12xlarge, "r5a.12xlarge"
  ; R5a_8xlarge, "r5a.8xlarge"
  ; R5a_4xlarge, "r5a.4xlarge"
  ; R5a_2xlarge, "r5a.2xlarge"
  ; R5a_xlarge, "r5a.xlarge"
  ; R5a_large, "r5a.large"
  ; R5_metal, "r5.metal"
  ; R5_24xlarge, "r5.24xlarge"
  ; R5_16xlarge, "r5.16xlarge"
  ; R5_12xlarge, "r5.12xlarge"
  ; R5_8xlarge, "r5.8xlarge"
  ; R5_4xlarge, "r5.4xlarge"
  ; R5_2xlarge, "r5.2xlarge"
  ; R5_xlarge, "r5.xlarge"
  ; R5_large, "r5.large"
  ; R4_16xlarge, "r4.16xlarge"
  ; R4_8xlarge, "r4.8xlarge"
  ; R4_4xlarge, "r4.4xlarge"
  ; R4_2xlarge, "r4.2xlarge"
  ; R4_xlarge, "r4.xlarge"
  ; R4_large, "r4.large"
  ; R3_8xlarge, "r3.8xlarge"
  ; R3_4xlarge, "r3.4xlarge"
  ; R3_2xlarge, "r3.2xlarge"
  ; R3_xlarge, "r3.xlarge"
  ; R3_large, "r3.large"
  ; P4d_24xlarge, "p4d.24xlarge"
  ; P3dn_24xlarge, "p3dn.24xlarge"
  ; P3_16xlarge, "p3.16xlarge"
  ; P3_8xlarge, "p3.8xlarge"
  ; P3_2xlarge, "p3.2xlarge"
  ; P2_16xlarge, "p2.16xlarge"
  ; P2_8xlarge, "p2.8xlarge"
  ; P2_xlarge, "p2.xlarge"
  ; Mac1_metal, "mac1.metal"
  ; M6i_metal, "m6i.metal"
  ; M6i_32xlarge, "m6i.32xlarge"
  ; M6i_24xlarge, "m6i.24xlarge"
  ; M6i_16xlarge, "m6i.16xlarge"
  ; M6i_12xlarge, "m6i.12xlarge"
  ; M6i_8xlarge, "m6i.8xlarge"
  ; M6i_4xlarge, "m6i.4xlarge"
  ; M6i_2xlarge, "m6i.2xlarge"
  ; M6i_xlarge, "m6i.xlarge"
  ; M6i_large, "m6i.large"
  ; M6gd_16xlarge, "m6gd.16xlarge"
  ; M6gd_12xlarge, "m6gd.12xlarge"
  ; M6gd_8xlarge, "m6gd.8xlarge"
  ; M6gd_4xlarge, "m6gd.4xlarge"
  ; M6gd_2xlarge, "m6gd.2xlarge"
  ; M6gd_xlarge, "m6gd.xlarge"
  ; M6gd_large, "m6gd.large"
  ; M6gd_medium, "m6gd.medium"
  ; M6gd_metal, "m6gd.metal"
  ; M6g_16xlarge, "m6g.16xlarge"
  ; M6g_12xlarge, "m6g.12xlarge"
  ; M6g_8xlarge, "m6g.8xlarge"
  ; M6g_4xlarge, "m6g.4xlarge"
  ; M6g_2xlarge, "m6g.2xlarge"
  ; M6g_xlarge, "m6g.xlarge"
  ; M6g_large, "m6g.large"
  ; M6g_medium, "m6g.medium"
  ; M6g_metal, "m6g.metal"
  ; M6a_48xlarge, "m6a.48xlarge"
  ; M6a_32xlarge, "m6a.32xlarge"
  ; M6a_24xlarge, "m6a.24xlarge"
  ; M6a_16xlarge, "m6a.16xlarge"
  ; M6a_12xlarge, "m6a.12xlarge"
  ; M6a_8xlarge, "m6a.8xlarge"
  ; M6a_4xlarge, "m6a.4xlarge"
  ; M6a_2xlarge, "m6a.2xlarge"
  ; M6a_xlarge, "m6a.xlarge"
  ; M6a_large, "m6a.large"
  ; M5zn_metal, "m5zn.metal"
  ; M5zn_12xlarge, "m5zn.12xlarge"
  ; M5zn_6xlarge, "m5zn.6xlarge"
  ; M5zn_3xlarge, "m5zn.3xlarge"
  ; M5zn_2xlarge, "m5zn.2xlarge"
  ; M5zn_xlarge, "m5zn.xlarge"
  ; M5zn_large, "m5zn.large"
  ; M5n_metal, "m5n.metal"
  ; M5n_24xlarge, "m5n.24xlarge"
  ; M5n_16xlarge, "m5n.16xlarge"
  ; M5n_12xlarge, "m5n.12xlarge"
  ; M5n_8xlarge, "m5n.8xlarge"
  ; M5n_4xlarge, "m5n.4xlarge"
  ; M5n_2xlarge, "m5n.2xlarge"
  ; M5n_xlarge, "m5n.xlarge"
  ; M5n_large, "m5n.large"
  ; M5dn_metal, "m5dn.metal"
  ; M5dn_24xlarge, "m5dn.24xlarge"
  ; M5dn_16xlarge, "m5dn.16xlarge"
  ; M5dn_12xlarge, "m5dn.12xlarge"
  ; M5dn_8xlarge, "m5dn.8xlarge"
  ; M5dn_4xlarge, "m5dn.4xlarge"
  ; M5dn_2xlarge, "m5dn.2xlarge"
  ; M5dn_xlarge, "m5dn.xlarge"
  ; M5dn_large, "m5dn.large"
  ; M5d_metal, "m5d.metal"
  ; M5d_24xlarge, "m5d.24xlarge"
  ; M5d_16xlarge, "m5d.16xlarge"
  ; M5d_12xlarge, "m5d.12xlarge"
  ; M5d_8xlarge, "m5d.8xlarge"
  ; M5d_4xlarge, "m5d.4xlarge"
  ; M5d_2xlarge, "m5d.2xlarge"
  ; M5d_xlarge, "m5d.xlarge"
  ; M5d_large, "m5d.large"
  ; M5ad_24xlarge, "m5ad.24xlarge"
  ; M5ad_16xlarge, "m5ad.16xlarge"
  ; M5ad_12xlarge, "m5ad.12xlarge"
  ; M5ad_8xlarge, "m5ad.8xlarge"
  ; M5ad_4xlarge, "m5ad.4xlarge"
  ; M5ad_2xlarge, "m5ad.2xlarge"
  ; M5ad_xlarge, "m5ad.xlarge"
  ; M5ad_large, "m5ad.large"
  ; M5a_24xlarge, "m5a.24xlarge"
  ; M5a_16xlarge, "m5a.16xlarge"
  ; M5a_12xlarge, "m5a.12xlarge"
  ; M5a_8xlarge, "m5a.8xlarge"
  ; M5a_4xlarge, "m5a.4xlarge"
  ; M5a_2xlarge, "m5a.2xlarge"
  ; M5a_xlarge, "m5a.xlarge"
  ; M5a_large, "m5a.large"
  ; M5_metal, "m5.metal"
  ; M5_24xlarge, "m5.24xlarge"
  ; M5_16xlarge, "m5.16xlarge"
  ; M5_12xlarge, "m5.12xlarge"
  ; M5_8xlarge, "m5.8xlarge"
  ; M5_4xlarge, "m5.4xlarge"
  ; M5_2xlarge, "m5.2xlarge"
  ; M5_xlarge, "m5.xlarge"
  ; M5_large, "m5.large"
  ; M4_16xlarge, "m4.16xlarge"
  ; M4_10xlarge, "m4.10xlarge"
  ; M4_4xlarge, "m4.4xlarge"
  ; M4_2xlarge, "m4.2xlarge"
  ; M4_xlarge, "m4.xlarge"
  ; M4_large, "m4.large"
  ; M3_2xlarge, "m3.2xlarge"
  ; M3_xlarge, "m3.xlarge"
  ; M3_large, "m3.large"
  ; M3_medium, "m3.medium"
  ; M2_4xlarge, "m2.4xlarge"
  ; M2_2xlarge, "m2.2xlarge"
  ; M2_xlarge, "m2.xlarge"
  ; M1_xlarge, "m1.xlarge"
  ; M1_large, "m1.large"
  ; M1_medium, "m1.medium"
  ; M1_small, "m1.small"
  ; Is4gen_8xlarge, "is4gen.8xlarge"
  ; Is4gen_4xlarge, "is4gen.4xlarge"
  ; Is4gen_2xlarge, "is4gen.2xlarge"
  ; Is4gen_xlarge, "is4gen.xlarge"
  ; Is4gen_large, "is4gen.large"
  ; Is4gen_medium, "is4gen.medium"
  ; Inf1_24xlarge, "inf1.24xlarge"
  ; Inf1_6xlarge, "inf1.6xlarge"
  ; Inf1_2xlarge, "inf1.2xlarge"
  ; Inf1_xlarge, "inf1.xlarge"
  ; Im4gn_16xlarge, "im4gn.16xlarge"
  ; Im4gn_8xlarge, "im4gn.8xlarge"
  ; Im4gn_4xlarge, "im4gn.4xlarge"
  ; Im4gn_2xlarge, "im4gn.2xlarge"
  ; Im4gn_xlarge, "im4gn.xlarge"
  ; Im4gn_large, "im4gn.large"
  ; I3en_metal, "i3en.metal"
  ; I3en_24xlarge, "i3en.24xlarge"
  ; I3en_12xlarge, "i3en.12xlarge"
  ; I3en_6xlarge, "i3en.6xlarge"
  ; I3en_3xlarge, "i3en.3xlarge"
  ; I3en_2xlarge, "i3en.2xlarge"
  ; I3en_xlarge, "i3en.xlarge"
  ; I3en_large, "i3en.large"
  ; I3_metal, "i3.metal"
  ; I3_16xlarge, "i3.16xlarge"
  ; I3_8xlarge, "i3.8xlarge"
  ; I3_4xlarge, "i3.4xlarge"
  ; I3_2xlarge, "i3.2xlarge"
  ; I3_xlarge, "i3.xlarge"
  ; I3_large, "i3.large"
  ; I2_8xlarge, "i2.8xlarge"
  ; I2_4xlarge, "i2.4xlarge"
  ; I2_2xlarge, "i2.2xlarge"
  ; I2_xlarge, "i2.xlarge"
  ; H1_16xlarge, "h1.16xlarge"
  ; H1_8xlarge, "h1.8xlarge"
  ; H1_4xlarge, "h1.4xlarge"
  ; H1_2xlarge, "h1.2xlarge"
  ; Hs1_8xlarge, "hs1.8xlarge"
  ; Hpc6a_48xlarge, "hpc6a.48xlarge"
  ; Hi1_4xlarge, "hi1.4xlarge"
  ; G5g_metal, "g5g.metal"
  ; G5g_16xlarge, "g5g.16xlarge"
  ; G5g_8xlarge, "g5g.8xlarge"
  ; G5g_4xlarge, "g5g.4xlarge"
  ; G5g_2xlarge, "g5g.2xlarge"
  ; G5g_xlarge, "g5g.xlarge"
  ; G5_48xlarge, "g5.48xlarge"
  ; G5_24xlarge, "g5.24xlarge"
  ; G5_16xlarge, "g5.16xlarge"
  ; G5_12xlarge, "g5.12xlarge"
  ; G5_8xlarge, "g5.8xlarge"
  ; G5_4xlarge, "g5.4xlarge"
  ; G5_2xlarge, "g5.2xlarge"
  ; G5_xlarge, "g5.xlarge"
  ; G4dn_metal, "g4dn.metal"
  ; G4dn_16xlarge, "g4dn.16xlarge"
  ; G4dn_12xlarge, "g4dn.12xlarge"
  ; G4dn_8xlarge, "g4dn.8xlarge"
  ; G4dn_4xlarge, "g4dn.4xlarge"
  ; G4dn_2xlarge, "g4dn.2xlarge"
  ; G4dn_xlarge, "g4dn.xlarge"
  ; G4ad_16xlarge, "g4ad.16xlarge"
  ; G4ad_8xlarge, "g4ad.8xlarge"
  ; G4ad_4xlarge, "g4ad.4xlarge"
  ; G4ad_2xlarge, "g4ad.2xlarge"
  ; G4ad_xlarge, "g4ad.xlarge"
  ; G3s_xlarge, "g3s.xlarge"
  ; G3_16xlarge, "g3.16xlarge"
  ; G3_8xlarge, "g3.8xlarge"
  ; G3_4xlarge, "g3.4xlarge"
  ; G2_8xlarge, "g2.8xlarge"
  ; G2_2xlarge, "g2.2xlarge"
  ; F1_16xlarge, "f1.16xlarge"
  ; F1_4xlarge, "f1.4xlarge"
  ; F1_2xlarge, "f1.2xlarge"
  ; Dl1_24xlarge, "dl1.24xlarge"
  ; D3en_12xlarge, "d3en.12xlarge"
  ; D3en_8xlarge, "d3en.8xlarge"
  ; D3en_6xlarge, "d3en.6xlarge"
  ; D3en_4xlarge, "d3en.4xlarge"
  ; D3en_2xlarge, "d3en.2xlarge"
  ; D3en_xlarge, "d3en.xlarge"
  ; D3_8xlarge, "d3.8xlarge"
  ; D3_4xlarge, "d3.4xlarge"
  ; D3_2xlarge, "d3.2xlarge"
  ; D3_xlarge, "d3.xlarge"
  ; D2_8xlarge, "d2.8xlarge"
  ; D2_4xlarge, "d2.4xlarge"
  ; D2_2xlarge, "d2.2xlarge"
  ; D2_xlarge, "d2.xlarge"
  ; Cr1_8xlarge, "cr1.8xlarge"
  ; Cg1_4xlarge, "cg1.4xlarge"
  ; Cc2_8xlarge, "cc2.8xlarge"
  ; Cc1_4xlarge, "cc1.4xlarge"
  ; C6i_metal, "c6i.metal"
  ; C6i_32xlarge, "c6i.32xlarge"
  ; C6i_24xlarge, "c6i.24xlarge"
  ; C6i_16xlarge, "c6i.16xlarge"
  ; C6i_12xlarge, "c6i.12xlarge"
  ; C6i_8xlarge, "c6i.8xlarge"
  ; C6i_4xlarge, "c6i.4xlarge"
  ; C6i_2xlarge, "c6i.2xlarge"
  ; C6i_xlarge, "c6i.xlarge"
  ; C6i_large, "c6i.large"
  ; C6gn_16xlarge, "c6gn.16xlarge"
  ; C6gn_12xlarge, "c6gn.12xlarge"
  ; C6gn_8xlarge, "c6gn.8xlarge"
  ; C6gn_4xlarge, "c6gn.4xlarge"
  ; C6gn_2xlarge, "c6gn.2xlarge"
  ; C6gn_xlarge, "c6gn.xlarge"
  ; C6gn_large, "c6gn.large"
  ; C6gn_medium, "c6gn.medium"
  ; C6gd_metal, "c6gd.metal"
  ; C6gd_16xlarge, "c6gd.16xlarge"
  ; C6gd_12xlarge, "c6gd.12xlarge"
  ; C6gd_8xlarge, "c6gd.8xlarge"
  ; C6gd_4xlarge, "c6gd.4xlarge"
  ; C6gd_2xlarge, "c6gd.2xlarge"
  ; C6gd_xlarge, "c6gd.xlarge"
  ; C6gd_large, "c6gd.large"
  ; C6gd_medium, "c6gd.medium"
  ; C6g_metal, "c6g.metal"
  ; C6g_16xlarge, "c6g.16xlarge"
  ; C6g_12xlarge, "c6g.12xlarge"
  ; C6g_8xlarge, "c6g.8xlarge"
  ; C6g_4xlarge, "c6g.4xlarge"
  ; C6g_2xlarge, "c6g.2xlarge"
  ; C6g_xlarge, "c6g.xlarge"
  ; C6g_large, "c6g.large"
  ; C6g_medium, "c6g.medium"
  ; C5n_metal, "c5n.metal"
  ; C5n_18xlarge, "c5n.18xlarge"
  ; C5n_9xlarge, "c5n.9xlarge"
  ; C5n_4xlarge, "c5n.4xlarge"
  ; C5n_2xlarge, "c5n.2xlarge"
  ; C5n_xlarge, "c5n.xlarge"
  ; C5n_large, "c5n.large"
  ; C5d_metal, "c5d.metal"
  ; C5d_24xlarge, "c5d.24xlarge"
  ; C5d_18xlarge, "c5d.18xlarge"
  ; C5d_12xlarge, "c5d.12xlarge"
  ; C5d_9xlarge, "c5d.9xlarge"
  ; C5d_4xlarge, "c5d.4xlarge"
  ; C5d_2xlarge, "c5d.2xlarge"
  ; C5d_xlarge, "c5d.xlarge"
  ; C5d_large, "c5d.large"
  ; C5ad_24xlarge, "c5ad.24xlarge"
  ; C5ad_16xlarge, "c5ad.16xlarge"
  ; C5ad_12xlarge, "c5ad.12xlarge"
  ; C5ad_8xlarge, "c5ad.8xlarge"
  ; C5ad_4xlarge, "c5ad.4xlarge"
  ; C5ad_2xlarge, "c5ad.2xlarge"
  ; C5ad_xlarge, "c5ad.xlarge"
  ; C5ad_large, "c5ad.large"
  ; C5a_24xlarge, "c5a.24xlarge"
  ; C5a_16xlarge, "c5a.16xlarge"
  ; C5a_12xlarge, "c5a.12xlarge"
  ; C5a_8xlarge, "c5a.8xlarge"
  ; C5a_4xlarge, "c5a.4xlarge"
  ; C5a_2xlarge, "c5a.2xlarge"
  ; C5a_xlarge, "c5a.xlarge"
  ; C5a_large, "c5a.large"
  ; C5_metal, "c5.metal"
  ; C5_24xlarge, "c5.24xlarge"
  ; C5_18xlarge, "c5.18xlarge"
  ; C5_12xlarge, "c5.12xlarge"
  ; C5_9xlarge, "c5.9xlarge"
  ; C5_4xlarge, "c5.4xlarge"
  ; C5_2xlarge, "c5.2xlarge"
  ; C5_xlarge, "c5.xlarge"
  ; C5_large, "c5.large"
  ; C4_8xlarge, "c4.8xlarge"
  ; C4_4xlarge, "c4.4xlarge"
  ; C4_2xlarge, "c4.2xlarge"
  ; C4_xlarge, "c4.xlarge"
  ; C4_large, "c4.large"
  ; C3_8xlarge, "c3.8xlarge"
  ; C3_4xlarge, "c3.4xlarge"
  ; C3_2xlarge, "c3.2xlarge"
  ; C3_xlarge, "c3.xlarge"
  ; C3_large, "c3.large"
  ; C1_xlarge, "c1.xlarge"
  ; C1_medium, "c1.medium"
  ; A1_metal, "a1.metal"
  ; A1_4xlarge, "a1.4xlarge"
  ; A1_2xlarge, "a1.2xlarge"
  ; A1_xlarge, "a1.xlarge"
  ; A1_large, "a1.large"
  ; A1_medium, "a1.medium"
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
