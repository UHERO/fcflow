# '*************************'
#  List macro forecasts to plot ----
# '*************************'

# JOBS LISTS ----
eag   <- c("EAG_HI", "EAG_NBI", "EAG_HON")
eaf   <- c("EAF_HI", "EAF_NBI", "EAF_HON")
ect   <- c("ECT_HI", "ECT_NBI", "ECT_HON")
eelse <- c("E_ELSE_HI",  "E_ELSE_NBI", "E_ELSE_HON")
efir  <- c("E_FIR_HI", "E_FIR_NBI", "E_FIR_HON")
ehc   <- c("EHC_HI", "EHC_HON", "EHC_NBI")
egvfd <- c("EGVFD_HI", "EGVFD_HON", "EGVFD_NBI")
egvsl <- c("E_GVSL_HI", "E_GVSL_HON", "E_GVSL_NBI")
emn   <- c("EMN_HI", "EMN_HON", "EMN_NBI")
etrade <- c("E_TRADE_HI", "E_TRADE_NBI", "E_TRADE_HON")
etu    <- c("E_TU_HI", "E_TU_NBI", "E_TU_HON")
alljob <- c(eag,eaf,ect,eelse,efir,ehc,egvfd,egvsl,emn,etrade,etu)

# INC LISTS ----
yag <- c("YLAG_R_HI", "YLAG_R_NBI", "YLAG_R_HON", "YPJAG_R_HI", "YPJAG_R_NBI", "YPJAG_R_HON")

yaf		<- c("YLAF_R_HI", "YPJAF_R_HI", "YLAF_HI")
ytrade 	<- c("YL_TRADE_R_HI", "YPJ_TRADE_R_HI", "YL_TRADE_HI")
ytu 	<- c("YL_TU_R_HI", "YPJ_TU_R_HI", "YL_TU_HI")
yct		<- c("YL_CTMI_R_HI", "YPJCT_R_HI", "YL_CTMI_HI")
ymn 	<- c("YLMN_R_HI", "YPJMN_R_HI", "YLMN_HI")
yelse	<- c("YL_ELSE_R_HI", "YPJ_ELSE_R_HI", "YL_ELSE_HI")
yfir	<- c("YL_FIR_R_HI", "YPJ_FIR_R_HI", "YL_FIR_HI")
yhc		<- c("YLHC_R_HI", "YPJHC_R_HI", "YLHC_HI")
ygvfd	<- c("YLGVFD_R_HI", "YPJGVFD_R_HI", "YLGVFD_HI")
ygvsl	<- c("YL_GVSL_R_HI", "YPJ_GVSL_R_HI", "YL_GVSL_HI")
ygvml	<- c("YLGVML_R_HI", "YPJGVML_R_HI", "YLGVML_HI")
ygv		<- c("YLGV_R_HI", "YPJGV_R_HI", "YLGV_HI")

allinc	<- c(yaf, ytrade, ytu, yct, ymn, yelse, yfir, yhc, ygvfd, ygvsl, ygvml, ygv, yag)

ypjag	<- c("YPJAG_R_HI", "YPJAG_R_NBI", "YPJAG_R_HON")
ypjaf	<- c("YPJAF_R_HI")
ypjct	<- c("YPJCT_R_HI")
ypjelse	<- c("YPJ_ELSE_R_HI")
ypjfir	<- c("YPJ_FIR_R_HI")
ypjhc	<- c("YPJHC_R_HI")
ypjgvfd	<- c("YPJGVFD_R_HI")
ypjgvsl	<- c("YPJ_GVSL_R_HI")
ypjgml	<- c("YPJGVML_R_HI")



# Population Lists ----
pop <- c("NMIG_HI", "NBIR_HI", "NBIRR_HI", "NDEA_HI", "NDEAR_HI", "NR_HI") 
popcnty <- c("NR_HAW", "NR_KAU", "NR_MAU", "NR_HON")

#LFEMPUR ----
lfempur <- c("UR_HI", "UR_NBI", "UR_HON", "EMPL_HI", "EMPL_NBI", "EMPL_HON", "LFPR_HI","LFPR_NBI","LFPR_HON", "LF_HI", "LF_NBI", "LF_HON")

plot_list <- eaf
plot_list <- c(alljob, allinc, pop, popcnty, lfempur)
