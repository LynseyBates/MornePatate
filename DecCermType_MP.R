# set the working directory to P
#setwd("P:/")
setwd("E:/MP/MornePatate-master/MornePatate-master/AI")

#load the library
require(RPostgreSQL)
library(tidyr)
library(dplyr)
library(ggplot2)

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
                   "public"."tblCeramicGenre"."CeramicGenre" as "Genre",
                   "public"."tblContext"."ProjectID",
                   "public"."tblContext"."Context",
                   "public"."tblContext"."QuadratID",
                   "public"."tblContext"."DAACSPhase",
                   "public"."tblContext"."MasterContextNumber" as "Block"
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

#Take out "not applicable" genre
justgenreX <- subset(WaresX, ! WaresX$Genre  %in%  c('Not Applicable'))

summary1 <- justgenreX %>% 
  group_by(Genre) %>% 
  summarise(count = sum(Quantity))
summary1

#Combine Genres into broader groupings

justgenreX$Category[justgenreX$Genre == 'Barley'] <- 'MoldedEdge'
justgenreX$Category[justgenreX$Genre == 'Blue and Gray'] <- 'Blue&Gray'
justgenreX$Category[justgenreX$Genre == 'Feather Edge'] <- 'MoldedEdge'
justgenreX$Category[justgenreX$Genre == 'Handpainted Blue'] <- 'Handpainted'
justgenreX$Category[justgenreX$Genre == 'Molded Edge Decoration, other'] <- 'MoldedEdge'
justgenreX$Category[justgenreX$Genre == 'Dot/Diaper/Basketweave'] <- 'MoldedEdge'
justgenreX$Category[justgenreX$Genre == 'Overglaze, handpainted'] <- 'Overglaze'
justgenreX$Category[justgenreX$Genre == 'Handpainted, Polychrome Cool'] <- 'Handpainted'
justgenreX$Category[justgenreX$Genre == 'Handpainted, Polychrome Other'] <- 'Handpainted'
justgenreX$Category[justgenreX$Genre == 'Handpainted, Polychrome Warm'] <- 'Handpainted'
justgenreX$Category[justgenreX$Genre == 'Royal Pattern'] <- 'MoldedEdge'
justgenreX$Category[justgenreX$Genre == 'Scratch Blue'] <- 'ScratchBlue'
justgenreX$Category[justgenreX$Genre == 'Slipware, factory made'] <- 'FactorySlip'
justgenreX$Category[justgenreX$Genre == 'Sponge/Spatter'] <- 'Sponged'
justgenreX$Category[justgenreX$Genre == 'Transfer Print Over'] <- 'PrintedOver'
justgenreX$Category[justgenreX$Genre == 'Decalcomania'] <- 'PrintedOver'
justgenreX$Category[justgenreX$Genre == 'Luster Decoration'] <- 'Handpainted'

justgenreX$Category[grepl("Transfer Print Under*", justgenreX$Genre)]<-'PrintedUnder'
justgenreX$Category[grepl("Flow*", justgenreX$Genre)]<-'Flow'
justgenreX$Category[grepl("Shell*", justgenreX$Genre)]<-'ShellEdge'

summary2 <- justgenreX %>% 
  group_by(Category) %>% 
  summarise(count = sum(Quantity))
summary2

justgenreY <- filter(justgenreX, ! is.na(Category))

justgenreZ <- justgenreY %>% select(ProjectID, QuadratID, DAACSPhase, Quantity, Category, Block)

# Split Quad ID N and E
WaresSTP <- justgenreZ %>% filter(Block=='STP')

# Remove Ns
WaresSTP <- WaresSTP %>% separate(QuadratID, c("N", "E"), "E")
WaresSTP2 <- WaresSTP %>% separate(N, c("X", "N"), "N")

WaresSTP2$Block <- NA

WaresSTP2$Block[WaresSTP2$N > 4590 & WaresSTP2$E >6554 ] <- 'BlockA'

WaresSTP2$Block[WaresSTP2$E >6660 ] <- 'BlockC'

WaresSTP2$Block[WaresSTP2$N < 4594 & WaresSTP2$E < 6596 ] <- 'BlockFG'

WaresSTP2$Block[(WaresSTP2$N < 4594 & WaresSTP2$N > 4560) & (WaresSTP2$E < 6660 & WaresSTP2$E > 6590) ] <- 'BlockB'

WaresSTP2$Block[(WaresSTP2$N < 4550) & (WaresSTP2$E < 6660 & WaresSTP2$E > 6596) ] <- 'BlockD'


GenreX <- WaresSTP2 %>% select(ProjectID, DAACSPhase, Quantity, Category, Block)

GenreY <- justgenreZ %>% filter(! Block=='STP') %>% select(ProjectID, DAACSPhase, Quantity, Category, Block)

GenreZ <- rbind(GenreY,GenreX)

# Replace F and G
GenreZ$Block[GenreZ$Block == 'BlockF'] <- 'BlockFG'
GenreZ$Block[GenreZ$Block == 'BlockG'] <- 'BlockFG'

### C. Add 0 counts
# Create DF for calculating AIs

GenreUnit <-GenreZ %>% group_by(ProjectID, Category, Block, DAACSPhase) %>%
  
  summarise(count = sum(Quantity)) %>%
  
  spread(key=Category, value=count,  fill=0, drop=F)

### C. Merge Glass and Cerm data ##########

# Merge with Glass data
GenreAI_glass <- merge(GlassSum, GenreUnit, by=c("Block","DAACSPhase", "ProjectID"))

GenreAI_glass$DAACSPhase[GenreAI_glass$DAACSPhase == ''] <- NA
GenreAI_glass$Block[GenreAI_glass$Block == ''] <- NA

GenreAI_glass2 <- filter(GenreAI_glass, ! is.na(Block))
GenreAI_glass3 <- filter(GenreAI_glass2, ! is.na(DAACSPhase))

### D. Bring in MCD data ##########
GenreAI_glass3$ProjPhase <- paste(GenreAI_glass3$ProjectID, GenreAI_glass3$DAACSPhase, sep="_")
dates <- read.csv("projMCD.csv", header=TRUE, stringsAsFactors = FALSE)

GenreAI_mcd <- merge(dates, GenreAI_glass3, by="ProjPhase")

### E. Shell Edge ##########

shell <- GenreAI_mcd %>% select(blueMCD, Block,DAACSPhase,WBGSum, ShellEdge)

shell <- shell %>% mutate(total = ShellEdge+WBGSum) %>% mutate(AI = ShellEdge/total)

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
wbgCI <- adjustedWaldCI(shell$ShellEdge,shell$total,0.05)
shell$gCIUpper <- wbgCI$upperCL
shell$gCILower <- wbgCI$lowerCL
shell$gp <- wbgCI$pTilde

shell$gp[shell$ShellEdge == 0] <- 0


#write.csv(shell, "shellRef.csv")

# Plot Data

set.seed(42)
a<-ggplot(shell, aes(x=shell$blueMCD, y=shell$gp, fill=shell$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = shell$gCILower,ymax = shell$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Shell Edge/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1.0), breaks=seq(0, 1.0, 0.1))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
a
ggsave("MPAI_ByBlock_ShellEdgeWBG.png", a, width=10, height=7.5, dpi=300)

### F. PrintedUnder ##########

print <- GenreAI_mcd %>% select(blueMCD, Block,DAACSPhase,WBGSum, PrintedUnder)

print <- print %>% mutate(total = PrintedUnder+WBGSum) %>% mutate(AI = PrintedUnder/total)

#run function on all data
wbgCI <- adjustedWaldCI(print$PrintedUnder,print$total,0.05)
print$gCIUpper <- wbgCI$upperCL
print$gCILower <- wbgCI$lowerCL
print$gp <- wbgCI$pTilde

print$gp[print$PrintedUnder == 0] <- 0


#write.csv(shell, "shellRef.csv")

# Plot Data

set.seed(42)
b<-ggplot(shell, aes(x=print$blueMCD, y=print$gp, fill=print$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = print$gCILower,ymax = print$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Printed Under/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1.0), breaks=seq(0, 1.0, 0.1))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
b
ggsave("MPAI_ByBlock_PrintedWBG.png", b, width=10, height=7.5, dpi=300)

### G. Handpainted ##########

paint <- GenreAI_mcd %>% select(blueMCD, Block,DAACSPhase,WBGSum, Handpainted)

paint <- paint %>% mutate(total = Handpainted+WBGSum) %>% mutate(AI = Handpainted/total)

#run function on all data
wbgCI <- adjustedWaldCI(paint$Handpainted,paint$total,0.05)
paint$gCIUpper <- wbgCI$upperCL
paint$gCILower <- wbgCI$lowerCL
paint$gp <- wbgCI$pTilde

paint$gp[paint$Handpainted == 0] <- 0

# Plot Data

set.seed(42)
c<-ggplot(paint, aes(x=paint$blueMCD, y=paint$gp, fill=paint$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = paint$gCILower,ymax = paint$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Handpainted/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1.0), breaks=seq(0, 1.0, 0.1))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
c
ggsave("MPAI_ByBlock_HandpaintedWBG.png", c, width=10, height=7.5, dpi=300)

### H. Factory Slip ##########

slip <- GenreAI_mcd %>% select(blueMCD, Block,DAACSPhase,WBGSum, FactorySlip)

slip <- slip %>% mutate(total = FactorySlip+WBGSum) %>% mutate(AI = FactorySlip/total)

#run function on all data
wbgCI <- adjustedWaldCI(slip$FactorySlip,slip$total,0.05)
slip$gCIUpper <- wbgCI$upperCL
slip$gCILower <- wbgCI$lowerCL
slip$gp <- wbgCI$pTilde

slip$gp[slip$FactorySlip == 0] <- 0

# Plot Data

set.seed(42)
d<-ggplot(slip, aes(x=slip$blueMCD, y=slip$gp, fill=slip$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = slip$gCILower,ymax = slip$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Factory Slip/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  scale_y_continuous(limits=c(0.0,1.0), breaks=seq(0, 1.0, 0.1))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
d
ggsave("MPAI_ByBlock_FactorySlipWBG.png", d, width=10, height=7.5, dpi=300)
