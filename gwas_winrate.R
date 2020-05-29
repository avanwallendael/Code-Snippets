library(switchgrassGWAS)
library(bigsnpr)
library(tidyverse)

setwd("~/Desktop/GWAS")

states_gwas <- read_delim("~/Desktop/GWAS/states_gwas.txt", 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
colnames(states_gwas)<-c("state", "tax", "years", "points", "winrate")

master_meta$winrate<-states_gwas$winrate[match(master_meta$STATE, states_gwas$state)]

winpheno<-phenos[[1]]
winpheno$pointrate<-master_meta$winrate[match(winpheno$PLANT_ID, master_meta$PLANT_ID)]
winpheno$wax<-NULL
phenos<-winpheno

#snp<-snp_attach("pvdiv_gwas.rds")
#load("obj.svd.rda")
NCORES<-nb_cores()

gwas_winpheno2<- pvdiv_gwas(df = phenos, type = "linear",
             snp = snp, covar = obj.svd, ncores = NCORES)

pvals<-stats::predict(gwas_winpheno2)

  gwas_trim<-as.data.frame(gwas_winpheno2[which(pvals<(-3)),])
  gwas_trim$POS<-POS[which(pvals<(-3))]
  gwas_trim$CHR<-CHRN[which(pvals<(-3))]
  gwas_trim$pval<-pvals[which(pvals<(-3))]
  
  high_thresh<-.05/length(CHRN)

ggplot(gwas_trim, aes(x=POS, y=(-pval)))+
  geom_point(aes(col=CHR))+
  scale_color_manual(values = rep(c("black","grey40"),9), guide="none")+
  labs(x="Position", y="-log10 pval", title="Championship rate of Switchgrass")+
  geom_hline(aes(yintercept=-log(high_thresh)))+
  facet_grid(.~CHR, scales = "free", space="free")+
  theme_classic()+
  theme(axis.text.x = element_blank())