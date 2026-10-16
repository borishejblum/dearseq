# PBT gene sets related to kidney transplant

9 Pathogenesis Based Transcripts (PBT) gene sets specifically related to
kidney transplant

## Usage

``` r
data(PBT_gmt)
```

## Format

a `gmt` object containing 9 gene sets specific to kidney transplant (see
[`GSA.read.gmt`](https://rdrr.io/pkg/GSA/man/GSA.read.gmt.html)), which
is simply a `list` with the 3 following components:

- `genesets`: a `list` of `n` gene identifiers vectors composing
  eachgene set (each gene set is represented as the vector of the gene
  identifiers composing it)

- `geneset.names`: a vector of length `n` containing the gene set names
  (i.e. gene sets identifiers)

- geneset.descriptions: a vector of length `n` containing gene set
  descriptions (e.g. textual information on their biological function)

## Source

<http://atagc.med.ualberta.ca/Research/GeneLists>

## References

Halloran PF, De Freitas DG, Einecke G, *et al.*, The molecular phenotype
of kidney transplants: Personal viewpoint, *Am J Transplant*, 10:
2215-2222, 2010. .

Sellares J, Reeve J, Loupy A, et al., Molecular diagnosis of
antibody-mediated rejection in human kidney transplants, *Am J
Transplant*, 13:971-983, 2013.

Broin PO, Hayde N, Bao Y, et al., A pathogenesis-based transcript
signature in donor-specific antibody-positive kidney transplant patients
with normal biopsies, *Genomics Data* 2: 357-60, 2014.

## Examples

``` r
data('PBT_gmt')
PBT_gmt
#> $genesets
#> $genesets[[1]]
#>   [1] "ABCB4"     "ADAM28"    "ALS2CR13"  "AREG"      "ARHGAP24"  "BACE2"    
#>   [7] "BANK1"     "BCL11A"    "BCL2L11"   "BCNP1"     "BIRC3"     "BLK"      
#>  [13] "BTLA"      "BTNL9"     "C13orf18"  "C14orf110" "C3orf6"    "C9orf45"  
#>  [19] "CCR6"      "CD19"      "CD1C"      "CD22"      "CD72"      "CD79A"    
#>  [25] "CD79B"     "CHPT1"     "COCH"      "CPNE5"     "CR2"       "DD5"      
#>  [31] "E2F5"      "EAF2"      "EBF"       "ESRRBL1"   "FCER2"     "FCRH1"    
#>  [37] "FLJ12363"  "FLJ13984"  "FLJ20202"  "FLJ32028"  "FREB"      "GABBR1"   
#>  [43] "GNG7"      "GPR18"     "HDAC9"     "HERPUD1"   "HLA-DOA"   "HLA-DOB"  
#>  [49] "HTPAP"     "IGKC"      "IL28RA"    "IRTA2"     "ITPR1"     "KIAA0125" 
#>  [55] "KIAA0746"  "KIAA0819"  "KLHL14"    "LAF4"      "LCN6"      "LOC283663"
#>  [61] "LOC284749" "LY9"       "MEF2C"     "MGC24039"  "MGC40368"  "MGC48332" 
#>  [67] "MOX2"      "MS4A1"     "NAP1L"     "NR4A2"     "OSBPL10"   "P2RX5"    
#>  [73] "PAWR"      "PAX5"      "PCDH9"     "PDLIM1"    "PEG10"     "PKIG"     
#>  [79] "PLEKHG1"   "PNOC"      "POU2AF1"   "PRICKLE1"  "RALGPS2"   "RAM2"     
#>  [85] "RNF141"    "SCN3A"     "SEMA4B"    "SLC15A2"   "SNARK"     "SPAP1"    
#>  [91] "SPIB"      "STRBP"     "TCF4"      "TCF8"      "TCL1A"     "TLR10"    
#>  [97] "TM4SF13"   "VPREB3"    "ZDHHC23"   "ZNF238"   
#> 
#> $genesets[[2]]
#>  [1] "CDH13"    "CDH5"     "COL13A1"  "CX3CR1"   "DARC"     "FGFBP2"  
#>  [7] "GNG11"    "GNLY"     "HLA-DRB3" "ICAM2"    "KLRF1"    "MALL"    
#> [13] "MYBL1"    "PGM5"     "PLA1A"    "PLAT"     "ROBO4"    "SH2D1B"  
#> [19] "SOX7"     "TEK"      "TM4SF18" 
#> 
#> $genesets[[3]]
#>   [1] "ACE"      "ADCY4"    "ADRA2A"   "ALPL"     "ANGPT1"   "ANGPT2"  
#>   [7] "BMP6"     "BMX"      "CARD4"    "CASP1"    "CAV1"     "CD34"    
#>  [13] "CD36"     "CDH26"    "CDH5"     "CETP"     "CMAH"     "CSAH"    
#>  [19] "COL18A1"  "COL4A1"   "COL4A2"   "CRIM1"    "CTGF"     "CYYR1"   
#>  [25] "DAB2"     "DLC1"     "DNAJB1"   "EDG1"     "EDN1"     "EDN1"    
#>  [31] "ET1"      "EFEMP1"   "EHD4"     "EMCN"     "ENPEP"    "ENPP2"   
#>  [37] "ESM1"     "EVA1"     "FABP4"    "FGD5"     "FHOD1"    "FLI1"    
#>  [43] "FOSB"     "FOXF2"    "GEM"      "GJA4"     "GMFG"     "HEYL"    
#>  [49] "HHIP"     "HOXD1"    "HOXD4"    "HSPA6"    "HSPG2"    "HSU24186"
#>  [55] "ICAM2"    "IGFBP7"   "JUNB"     "KDR"      "KLF4"     "KMO"     
#>  [61] "LAMB1"    "LAMC1"    "LDB2"     "LOXL2"    "MAOB"     "MCAM"    
#>  [67] "MEOX1"    "MFNG"     "MGP"      "MLL3"     "MSL3L1"   "NFATC4"  
#>  [73] "NID2"     "NOS3"     "NOX4"     "NP"       "NPR1"     "NR4A1"   
#>  [79] "NRF1"     "NTN4"     "NXF1"     "OPN3"     "PAK2"     "PALMD"   
#>  [85] "PDGFB"    "PDGFRB"   "PECAM1"   "PLSCR4"   "PLVAP"    "PODXL"   
#>  [91] "PPP1R16B" "PPP1R16B" "TIMAP"    "ANKRD4"   "KIAA0823" "PRG1"    
#>  [97] "PRKACA"   "PTGIR"    "RAI14"    "RAMP2"    "RASGRP2"  "RASIP1"  
#> [103] "RGS5"     "RHOJ"     "RPGR"     "SDPR"     "SEC14L1"  "SELE"    
#> [109] "SELP"     "SERPINE1" "SNAI1"    "SOX18"    "TARSH"    "TBX2"    
#> [115] "TCF4"     "TDO2"     "TEK"      "THBS1"    "TRPV2"    "TXNDC5"  
#> [121] "UACA"     "VCAM1"    "VWF"      "ZNF521"  
#> 
#> $genesets[[4]]
#> [1] "CPA3"   "FCER1A" "TPSAB1" "TPSB2" 
#> 
#> $genesets[[5]]
#>   [1] "SEPT4"              "ACSL5"              "AKAP2"             
#>   [4] "PALM2-AKAP2"        "ANGPTL2"            "ANKRD22"           
#>   [7] "APOBEC3F"           "APOBEC3G"           "APOBEC3G"          
#>  [10] "APOL1"              "APOL2"              "APOL3"             
#>  [13] "APOL4"              "APOL6"              "AQP1"              
#>  [16] "ARNT2"              "ASPHD2"             "ATF3"              
#>  [19] "BATF2"              "BST2"               "BTN3A1"            
#>  [22] "BTN3A2"             "BTN3A2"             "BTN3A3"            
#>  [25] "BTN3A3"             "C19orf66"           "C1orf21"           
#>  [28] "C1QB"               "C1QC"               "C1R"               
#>  [31] "C1S"                "C2"                 "C4A"               
#>  [34] "C4B"                "C4B_2"              "C4orf32"           
#>  [37] "C5orf56"            "C8orf4"             "CARD16"            
#>  [40] "CASP1"              "CASP1"              "CCL2"              
#>  [43] "CCL20"              "CCL8"               "CCRL1"             
#>  [46] "CD274"              "CD38"               "CD40"              
#>  [49] "CD74"               "CEBPD"              "CFH"               
#>  [52] "CFHR1"              "CLIC2"              "CMPK2"             
#>  [55] "COL8A1"             "CTSS"               "CXCL10"            
#>  [58] "CXCL11"             "CXCL2"              "CXCL9"             
#>  [61] "DDIT4L"             "DDX58"              "DDX60"             
#>  [64] "DTX3L"              "EDNRA"              "EFCAB2"            
#>  [67] "EPSTI1"             "F3"                 "FAM107A"           
#>  [70] "LOC101059937"       "FAM117B"            "FAM26F"            
#>  [73] "FBXO6"              "FCGR1A"             "FCGR1B"            
#>  [76] "FCGR1C"             "FCGR1B"             "GABBR1"            
#>  [79] "UBD"                "GBP1"               "GBP2"              
#>  [82] "GBP3"               "GBP4"               "GBP5"              
#>  [85] "GCH1"               "GZMA"               "HCAR3"             
#>  [88] "HCP5"               "HLA-A"              "HLA-B"             
#>  [91] "HLA-C"              "HLA-DMA"            "HLA-DMB"           
#>  [94] "HLA-DOA"            "HLA-DPA1"           "HLA-DPB1"          
#>  [97] "HLA-DQA1"           "HLA-DQA2"           "LOC100509457"      
#> [100] "HLA-DQA1"           "LOC100509457"       "HLA-DQB1"          
#> [103] "HLA-DQB1"           "LOC101060835"       "HLA-DRA"           
#> [106] "HLA-DRB1"           "HLA-DRB3"           "HLA-DRB4"          
#> [109] "LOC100507709"       "LOC100507714"       "HLA-DRB1"          
#> [112] "HLA-DRB4"           "HLA-DRB5"           "LOC100507709"      
#> [115] "LOC100507714"       "HLA-DRB1"           "HLA-DRB4"          
#> [118] "LOC100507709"       "LOC100507714"       "HLA-DRB1"          
#> [121] "LOC100507709"       "LOC100507714"       "HLA-DRB4"          
#> [124] "HLA-E"              "HLA-F"              "HLA-G"             
#> [127] "HLA-J"              "ICAM1"              "IDO1"              
#> [130] "IDO2"               "IFI27"              "IFI30"             
#> [133] "PIK3R2"             "IFI35"              "IFI44"             
#> [136] "IFI44L"             "IFI6"               "IFIH1"             
#> [139] "IFIT1"              "IFIT2"              "IFIT3"             
#> [142] "IFIT5"              "IFITM1"             "IFITM1"            
#> [145] "IFITM2"             "IL15"               "IL15RA"            
#> [148] "IL18BP"             "IL32"               "IRF1"              
#> [151] "IRF8"               "IRF9"               "ISG15"             
#> [154] "ISG20"              "ITGA6"              "JAK2"              
#> [157] "KCNJ2"              "KCTD14"             "NDUFC2-KCTD14"     
#> [160] "KLF4"               "LAP3"               "LGALS3BP"          
#> [163] "LGALS9"             "LIMK2"              "LMNB1"             
#> [166] "LOC100129518"       "SOD2"               "LOC154761"         
#> [169] "LRRK2"              "MAP3K7CL"           "MDK"               
#> [172] "MICB"               "MIR5193"            "UBA7"              
#> [175] "MX1"                "N4BP2L1"            "NLRC5"             
#> [178] "NME7"               "NRK"                "NTN4"              
#> [181] "OAS1"               "OAS2"               "OAS3"              
#> [184] "OTTHUMG00000159342" "RP1-93H18.6"        "P2RY14"            
#> [187] "PARP12"             "PARP14"             "PARP9"             
#> [190] "PCDH7"              "PCLO"               "PDE4B"             
#> [193] "PIGR"               "PIK3AP1"            "PLA1A"             
#> [196] "PLSCR1"             "PMAIP1"             "PSMB10"            
#> [199] "PSMB8"              "PSMB9"              "PSME2"             
#> [202] "PSTPIP2"            "RARRES1"            "RARRES3"           
#> [205] "RELN"               "RHOBTB3"            "RIPK2"             
#> [208] "RND3"               "RNF19B"             "RNF213"            
#> [211] "RTP4"               "SAA1"               "SAA2"              
#> [214] "SAA2-SAA4"          "SAMD9L"             "SAMHD1"            
#> [217] "SECTM1"             "SERPING1"           "SLAMF7"            
#> [220] "SLC15A3"            "SLC27A2"            "SLC2A5"            
#> [223] "SLC6A12"            "SOCS1"              "SOCS3"             
#> [226] "SOD2"               "SP110"              "SP140L"            
#> [229] "SRGN"               "STAMBPL1"           "STAT1"             
#> [232] "STAT2"              "SUCNR1"             "TAGAP"             
#> [235] "TAP1"               "TAP2"               "TAPBPL"            
#> [238] "TGM2"               "TIFA"               "TNC"               
#> [241] "TNFAIP2"            "TNFAIP6"            "TNFSF10"           
#> [244] "TNFSF13B"           "TNFSF15"            "TOX3"              
#> [247] "TRIM21"             "TRIM22"             "TRIM69"            
#> [250] "TYMP"               "UBE2L6"             "USP6NL"            
#> [253] "VAMP5"              "VRK2"               "WARS"              
#> [256] "XAF1"              
#> 
#> $genesets[[6]]
#> [1] "CX3CR1" "KLRF1"  "MYBL1"  "SH2D1B"
#> 
#> $genesets[[7]]
#>  [1] "CD2"      "CD3D"     "CD8A"     "CST7"     "CXCR6"    "GNLY"    
#>  [7] "GZMA"     "GZMB"     "GZMK"     "H963"     "HOP"      "IFNG"    
#> [13] "IL2RB"    "ITK"      "KIAA0101" "KLRK1"    "LCK"      "NELL2"   
#> [19] "NKG7"     "NUSAP1"   "PRF1"     "RRM2"     "STAT4"    "TRA@"    
#> [25] "TRGV9"   
#> 
#> $genesets[[8]]
#>  [1] "ADAMDEC1" "APOBEC3A" "C14orf58" "C1orf38"  "CCL18"    "CCRL2"   
#>  [7] "CD36"     "CD68"     "CD86"     "CEBPA"    "CENTA2"   "CES1"    
#> [13] "CSF1R"    "CSF2RA"   "CSTA"     "CXCL16"   "ECGF1"    "EMILIN2" 
#> [19] "FCER1G"   "FCGR2A"   "FLJ10116" "FLJ13213" "FLJ22570" "FPR1"    
#> [25] "FPRL2"    "GCA"      "GJB2"     "HID"      "KID"      "PPK"     
#> [31] "CX26"     "DFNA3"    "DFNB1"    "NSRD1"    "GM2A"     "GPR77"   
#> [37] "HK3"      "IBRDC3"   "IFIT4"    "IGSF6"    "IL1RN"    "IL4I1"   
#> [43] "ILT8"     "LILRB6"   "IRF2"     "KYNU"     "LILRB2"   "LILRB3"  
#> [49] "LILRB4"   "LYZ"      "MCOLN1"   "MICL"     "MNDA"     "MS4A7"   
#> [55] "NCF2"     "NKIR"     "NMES1"    "PDE4DIP"  "PILRA"    "PLA2G7"  
#> [61] "PRKCD"    "PTAFR"    "PAFR"     "RC3"      "S100A9"   "SIGLEC7" 
#> [67] "SLAMF8"   "SLC15A3"  "SLC1A3"   "SLC29A3"  "SLC38A6"  "SN"      
#> [73] "TFEC"     "TLR2"     "TLR8"     "UNC93B1" 
#> 
#> $genesets[[9]]
#> [1] "CD3D"   "CXCR6"  "GPR171" "NELL2"  "TCRA"  
#> 
#> 
#> $geneset.name
#> $geneset.name[[1]]
#> [1] "BAT"
#> 
#> $geneset.name[[2]]
#> [1] "DSAST"
#> 
#> $geneset.name[[3]]
#> [1] "ENDATs"
#> 
#> $geneset.name[[4]]
#> [1] "MAT"
#> 
#> $geneset.name[[5]]
#> [1] "nGRIT"
#> 
#> $geneset.name[[6]]
#> [1] "NKB"
#> 
#> $geneset.name[[7]]
#> [1] "QCAT"
#> 
#> $geneset.name[[8]]
#> [1] "QCMAT"
#> 
#> $geneset.name[[9]]
#> [1] "TCB"
#> 
#> 
#> $geneset.description
#> $geneset.description[[1]]
#> [1] "B-cell associated transcripts"
#> 
#> $geneset.description[[2]]
#> [1] "Donor-specific antibody (DSA) selective transcripts"
#> 
#> $geneset.description[[3]]
#> [1] "Endothelial cell associated transcripts"
#> 
#> $geneset.description[[4]]
#> [1] "Mast cell associated transcripts"
#> 
#> $geneset.description[[5]]
#> [1] "New Gamma-IFN and Rejection Induced Transcripts"
#> 
#> $geneset.description[[6]]
#> [1] "NK cell transcript burden"
#> 
#> $geneset.description[[7]]
#> [1] "Quantitative CTL-Associated Transcripts"
#> 
#> $geneset.description[[8]]
#> [1] "Quantitative Constitutive Macrophage-Associated Transcripts"
#> 
#> $geneset.description[[9]]
#> [1] "T cell transcript burden"
#> 
#> 

```
