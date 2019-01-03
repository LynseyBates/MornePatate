# set the working directory to P
#setwd("P:/")
setwd("E:/MP/MornePatate-master/MornePatate-master/AI")

#load the library
require(RPostgreSQL)
library(tidyr)
library(dplyr)

# tell DBI which driver to use
pgSQL <- dbDriver("PostgreSQL")
# establish the connection
DRCcon<-dbConnect(pgSQL, host='drc.iath.virginia.edu', port='5432',
                  dbname='daacs-production',
                  user='drcquery', password='!queryacct!')

### A. Get Glass Totals ###############
Glass <-dbGetQuery(DRCcon,'
                   SELECT
                   "public"."tblContext"."ProjectID",
"public"."tblContext"."QuadratID",
                   "public"."tblContext"."DAACSPhase",
                   "public"."tblContext"."MasterContextNumber",
                   "public"."tblGlass"."Quantity" as "WBGSum",
                   "public"."tblGlassForm"."GlassForm",
                   "public"."tblBasicColor"."BasicColor"
                   
                   FROM
                   "public"."tblContext"
                   INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                   INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                   INNER JOIN "public"."tblGlass" ON "public"."tblGlass"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                   INNER JOIN "public"."tblGlassForm" ON "public"."tblGlassForm"."GlassFormID" = "public"."tblGlass"."GlassFormID"
                   INNER JOIN "public"."tblBasicColor" ON "public"."tblGlass"."GlassBasicColorID" = "public"."tblBasicColor"."BasicColorID"
                   WHERE
                   "public"."tblContext"."ProjectID" = \'1243\'
                   OR
                   "public"."tblContext"."ProjectID" = \'1250\'
                   OR
                   "public"."tblContext"."ProjectID" = \'1251\'
                   
                   ')    

# Split Quad ID N and E
GlassSTP <- Glass %>% filter(MasterContextNumber=='STP')

# Remove Ns
GlassSTP <- GlassSTP %>% separate(QuadratID, c("N", "E"), "E")
GlassSTP2 <- GlassSTP %>% separate(N, c("X", "N"), "N")

GlassSTP2$Block <- NA

GlassSTP2$Block[GlassSTP2$N > 4590 & GlassSTP2$E >6554 ] <- 'BlockA'

GlassSTP2$Block[GlassSTP2$E >6660 ] <- 'BlockC'

GlassSTP2$Block[GlassSTP2$N < 4594 & GlassSTP2$E < 6596 ] <- 'BlockFG'

GlassSTP2$Block[(GlassSTP2$N < 4594 & GlassSTP2$N > 4560) & (GlassSTP2$E < 6660 & GlassSTP2$E > 6590) ] <- 'BlockB'

GlassSTP2$Block[(GlassSTP2$N < 4550) & (GlassSTP2$E < 6660 & GlassSTP2$E > 6596) ] <- 'BlockD'

GlassSTP3 <- GlassSTP2 %>% select(ProjectID, DAACSPhase, WBGSum, GlassForm, BasicColor, Block)

Glass$Block <- Glass$MasterContextNumber
Glass <- filter(Glass, ! Block=='STP')
GlassY <- Glass %>% select(ProjectID, DAACSPhase, WBGSum, GlassForm, BasicColor, Block)

GlassX <- rbind(GlassY,GlassSTP3)

# Filter by only green color 
GlassX <- GlassX %>% filter(BasicColor =='Green/Olive Green')

# Remove non-bottle forms
GlassX <- GlassX %>% filter(!GlassForm %in% c('Flask', 'Unidentifiable','Not Recorded'))

GlassX$Block[GlassX$Block == 'BlockF'] <- 'BlockFG'
GlassX$Block[GlassX$Block == 'BlockG'] <- 'BlockFG'

# Summarize by unit
GlassSum <- GlassX %>% group_by(ProjectID,Block,DAACSPhase) %>% summarise_at("WBGSum", sum)


### B. Ware Data ############
# submit a SQL query: note the use of \ as an escape sequence
# note the LEFT JOIN on the Feature table retains non-feature contexts
#Fill in your appropriate projectID

WaresX<-dbGetQuery(DRCcon,'
                          SELECT
                          "public"."tblCeramic"."Quantity",
                          "public"."tblCeramicWare"."Ware",
                          "public"."tblCeramicWare"."BeginDate",
                          "public"."tblCeramicWare"."EndDate",
                          "public"."tblContextFeatureType"."FeatureType",
                          "public"."tblCeramicGenre"."CeramicGenre",
  "public"."tblCeramicCEWType"."CeramicCEWType",
                          "public"."tblContext"."ProjectID",
                          "public"."tblContext"."Context",
"public"."tblContext"."QuadratID",
                          "public"."tblContextDepositType"."DepositType",
                          "public"."tblContext"."DAACSStratigraphicGroup",
                          "public"."tblContext"."FeatureNumber",
 "public"."tblContext"."DAACSPhase",
  "public"."tblContext"."MasterContextNumber"
                          FROM
                          "public"."tblContext"
                          INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                          INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                          LEFT JOIN "public"."tblContextDepositType" ON "public"."tblContext"."DepositTypeID" = "public"."tblContextDepositType"."DepositTypeID"
                          INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                          INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                          LEFT JOIN "public"."tblContextFeatureType" ON "public"."tblContext"."FeatureTypeID" = "public"."tblContextFeatureType"."FeatureTypeID" 
                          LEFT JOIN "public"."tblCeramicGenre" ON "public"."tblCeramic"."CeramicGenreID" = "public"."tblCeramicGenre"."CeramicGenreID"
 LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" =
  "public"."tblCeramicCEWType"."CeramicCEWTypeID"                         
WHERE
                   "public"."tblContext"."ProjectID" = \'1243\'
                         OR
                         "public"."tblContext"."ProjectID" = \'1250\'
                         OR
                         "public"."tblContext"."ProjectID" = \'1251\'
                         ')

# Replace 'Caribbean CEW' ware type with applicable CEW Type
WaresX <- mutate(WaresX, Ware= ifelse((Ware %in% 
                                                     c('Caribbean Coarse Earthenware, unid.',
                                                       'Caribbean Coarse Earthenware, wheel thrown',
                                                       'Caribbean Coarse Earthenware, hand built')), CeramicCEWType, 
                                                  Ware))

# Replace Unid. Carib CEW ware type with 'Caribbean CEW Unid.'
WaresX <- mutate(WaresX, Ware=ifelse(Ware == 'Unidentifiable', 
                                                 'Carribean CEW Unid.', Ware))

# Replace Unid. Carib CEW ware type with 'Caribbean CEW Unid.'
WaresX <- mutate(WaresX, Ware=ifelse(Ware == 'Morne Patate Type 1b', 
                                                 'Morne Patate Type 1 Combined', Ware))
# Replace Unid. Carib CEW ware type with 'Caribbean CEW Unid.'
WaresX <- mutate(WaresX, Ware=ifelse(Ware == 'Morne Patate Type 1a', 
                                                 'Morne Patate Type 1 Combined', Ware))
# Replace Unid. Carib CEW ware type with 'Caribbean CEW Unid.'
WaresX <- mutate(WaresX, Ware=ifelse(Ware == 'Morne Patate Type 1', 
                                                 'Morne Patate Type 1 Combined', Ware))

# Split Quad ID N and E
WaresSTP <- WaresX %>% filter(MasterContextNumber=='STP')

# Remove Ns
WaresSTP <- WaresSTP %>% separate(QuadratID, c("N", "E"), "E")
WaresSTP2 <- WaresSTP %>% separate(N, c("X", "N"), "N")

WaresSTP2$Block <- NA

WaresSTP2$Block[WaresSTP2$N > 4590 & WaresSTP2$E >6554 ] <- 'BlockA'

WaresSTP2$Block[WaresSTP2$E >6660 ] <- 'BlockC'

WaresSTP2$Block[WaresSTP2$N < 4594 & WaresSTP2$E < 6596 ] <- 'BlockFG'

WaresSTP2$Block[(WaresSTP2$N < 4594 & WaresSTP2$N > 4560) & (WaresSTP2$E < 6660 & WaresSTP2$E > 6590) ] <- 'BlockB'

WaresSTP2$Block[(WaresSTP2$N < 4550) & (WaresSTP2$E < 6660 & WaresSTP2$E > 6596) ] <- 'BlockD'


WaresSTP3 <- WaresSTP2 %>% select(ProjectID, DAACSPhase, Quantity, Ware, CeramicGenre, Block)

WaresX$Block <- WaresX$MasterContextNumber
WaresX <- filter(WaresX, ! Block=='STP')
WaresY <- WaresX %>% select(ProjectID, DAACSPhase, Quantity, Ware, CeramicGenre, Block)

Wares2 <- rbind(WaresY,WaresSTP3)

# Replace F and G
Wares2$Block[Wares2$Block == 'BlockF'] <- 'BlockFG'
Wares2$Block[Wares2$Block == 'BlockG'] <- 'BlockFG'

# Use case_when to cycle through QuadID, Feature, SG, and Context to assign 
# aggregration unit. YOu will need to cutomize this logic for YOUR site.
#Wares2 <- Wares2 %>% filter(! Block == '') %>% filter(! DAACSPhase == '') %>%
 # mutate( unit = paste(Block,"_",DAACSPhase))

# Summarize by unit
WaresUnit <- Wares2 %>% group_by(Ware, ProjectID, Block, DAACSPhase)

#Create new field for N and S
WaresUnit$type <- WaresUnit$Ware

summary2 <- WaresUnit %>% 
  group_by(Ware) %>% 
  summarise(count = sum(Quantity))
summary2

#Assign N and S based on quad IDs
WaresUnit$type[WaresUnit$type == 'Creamware'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Tin-Enameled, unidentified'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Faience'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Nottingham'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Red Agate, refined'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'White Salt Glaze'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Astbury Type'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Black Basalt'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Delftware, Dutch/British'] <- 'EarlyRefined'
WaresUnit$type[WaresUnit$type == 'Porcelain, Chinese'] <- 'EarlyRefined'

WaresUnit$type[WaresUnit$type == 'Yellow Ware'] <- 'LateRefined'
WaresUnit$type[WaresUnit$type == 'Whiteware'] <- 'LateRefined'
WaresUnit$type[WaresUnit$type == 'Ironstone/White Granite'] <- 'LateRefined'
WaresUnit$type[WaresUnit$type == 'Porcelain, English Bone China'] <- 'LateRefined'
WaresUnit$type[WaresUnit$type == 'Porcellaneous/Hard Paste'] <- 'LateRefined'
WaresUnit$type[WaresUnit$type == 'Redware, refined'] <- 'LateRefined'
WaresUnit$type[WaresUnit$type == 'Pearlware'] <- 'LateRefined'

WaresUnit$type[WaresUnit$type == 'Morne Patate Type 1 Combined'] <- 'LocalUtil'
WaresUnit$type[WaresUnit$type == 'Morne Patate Type 2'] <- 'LocalUtil'
WaresUnit$type[WaresUnit$type == 'Morne Patate Type 3'] <- 'LocalUtil'
WaresUnit$type[WaresUnit$type == 'Carribean CEW Unid.'] <- 'LocalUtil'

WaresUnit$type[WaresUnit$type == 'Vallauris'] <- 'FrenchUtil'
WaresUnit$type[WaresUnit$type == 'French Coarse Earthenware'] <- 'FrenchUtil'
WaresUnit$type[WaresUnit$type == 'Huveaune'] <- 'FrenchUtil'
WaresUnit$type[WaresUnit$type == 'Albisola'] <- 'FrenchUtil'
WaresUnit$type[WaresUnit$type == 'Saintonge'] <- 'FrenchUtil'

WaresUnit$type[WaresUnit$type == 'Coarse Earthenware, unidentified'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'British Stoneware'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'Redware'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'British Stoneware'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'American Stoneware'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'Slipware, North Midlands/Staffordshire'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'Frechen Brown'] <- 'OtherUtil'
WaresUnit$type[WaresUnit$type == 'Fulham Type'] <- 'OtherUtil'

### C. Add 0 counts
# Create DF for calculating AIs
WaresUnitAI <-WaresUnit %>% group_by(ProjectID, type, Block, DAACSPhase) %>% summarise(count = sum(Quantity))

WaresUnitAI_type <- filter(WaresUnitAI, type %in% c('FrenchUtil', 'OtherUtil', 'LocalUtil', 'LateRefined','EarlyRefined' ))

WaresUnitAI_type2 <- WaresUnitAI_type %>% expand(ProjectID, type, Block, DAACSPhase)

WaresUnitAI_type3 <- merge(WaresUnitAI_type2, WaresUnitAI_type, by=c("Block","DAACSPhase", "ProjectID", "type"))

### C. Merge Glass and Cerm data

# Merge with Glass data
WaresAI_glass <- merge(GlassSum, WaresUnitAI_type, by=c("Block","DAACSPhase", "ProjectID"))

#write.csv(WaresAI_glass, file='CeramicAIdata.csv')

### D. Bring in MCD data
WaresAI_glass$ProjPhase <- paste(WaresAI_glass$ProjectID, WaresAI_glass$DAACSPhase, sep="_")
dates <- read.csv("projMCD.csv", header=TRUE, stringsAsFactors = FALSE)

WaresAI_mcd <- merge(dates, WaresAI_glass, by="ProjPhase")


### E. Calculate AIs
WaresAI_mcd <- WaresAI_mcd %>% mutate(total = count+WBGSum) %>% mutate(AI = count/total)

adjustedWaldCI<-function(count,total,alpha){
  nTilde <- total+4
  pTilde <- (count+2)/(total+4)
  se <- sqrt((pTilde*(1-pTilde))/(nTilde))
  upperCL <- pTilde + se * qnorm(1-(alpha/2))
  lowerCL <- pTilde + se * qnorm(alpha/2) 
  upperCL<-ifelse ( upperCL > 1, 1, upperCL)
  lowerCL <-ifelse ( lowerCL < 0, 0, lowerCL)                               
  return(data.frame(pTilde,upperCL,lowerCL))
}

#run function on all data
wbgCI <- adjustedWaldCI(WaresAI_mcd$count,WaresAI_mcd$total,0.05)
WaresAI_mcd$gCIUpper <- wbgCI$upperCL
WaresAI_mcd$gCILower <- wbgCI$lowerCL
WaresAI_mcd$gp <- wbgCI$pTilde

### F. Plot Data

early <- WaresAI_mcd %>% filter(type=='EarlyRefined')

set.seed(42)
a<-ggplot(early, aes(x=early$blueMCD, y=early$gp, fill=early$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = early$gCILower,ymax = early$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Early Refined/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  #scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
    scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
a
ggsave("MPAI_ByBlock_EarlyRefWBG.png", a, width=10, height=7.5, dpi=300)


late <- WaresAI_mcd %>% filter(type=='LateRefined')

set.seed(42)
b<-ggplot(late, aes(x=late$blueMCD, y=late$gp, fill=late$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = late$gCILower,ymax = late$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Late Refined/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1), breaks=seq(0, 1, 0.25))+
  scale_x_continuous(limits=c(1770,1830), breaks=seq(1770,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
b
ggsave("MPAI_ByBlock_LateRefWBG.png", b, width=10, height=7.5, dpi=300)

french <- WaresAI_mcd %>% filter(type=='FrenchUtil')

set.seed(42)
c<-ggplot(french, aes(x=french$blueMCD, y=french$gp, fill=french$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = french$gCILower,ymax = french$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("French Utilitarian/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1), breaks=seq(0, 1, 0.25))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
c
ggsave("MPAI_ByBlock_FrenchUtilWBG.png", c, width=10, height=7.5, dpi=300)

local <- WaresAI_mcd %>% filter(type=='LocalUtil')

set.seed(42)
d<-ggplot(local, aes(x=local$blueMCD, y=local$gp, fill=local$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = local$gCILower,ymax = local$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Caribbean Coarse Earthenware/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1), breaks=seq(0, 1, 0.25))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
d
ggsave("MPAI_ByBlock_CaribWBG.png", d, width=10, height=7.5, dpi=300)

other <- WaresAI_mcd %>% filter(type=='OtherUtil')

set.seed(42)
e<-ggplot(other, aes(x=other$blueMCD, y=other$gp, fill=other$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = other$gCILower,ymax = other$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Other Utilitarian/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1), breaks=seq(0, 1, 0.25))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
e
ggsave("MPAI_ByBlock_OtherUtilWBG.png", e, width=10, height=7.5, dpi=300)
