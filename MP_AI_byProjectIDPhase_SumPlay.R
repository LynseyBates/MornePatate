# Calculates AI values for artifact types
# Denominators are Green Bottle Glass sum for all contexts 
#first draft Lynsey Bates 2017
#revised Jg 1/2/2019
#revised LB 1/3/2019, adding in phase MCDs and STP assignments

# setwd
setwd("E:/MP/MornePatate-master/MornePatate-master/AI")

#load the library
require(RPostgreSQL)
library(dplyr)
require(tidyr)
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
                   "public"."tblGlass"."Quantity" ,
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

# Filter by only green color 
GlassX <- Glass %>% filter(BasicColor =='Green/Olive Green')

# Remove non-bottle forms
GlassX <- GlassX %>% filter(!GlassForm %in% c('Flask', 'Unidentifiable','Not Recorded'))
GlassX$Block <- GlassX$MasterContextNumber
GlassX$Block[GlassX$Block == 'BlockF'] <- 'BlockFG'
GlassX$Block[GlassX$Block == 'BlockG'] <- 'BlockFG'

# Summarize by unit
GlassSum <- GlassX %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

GlassSum$Category <- 'WBG'

###C. Gen Arts ##############

GenArts <-dbGetQuery(DRCcon,'
                 SELECT
                 "public"."tblContext"."ProjectID",
                 "public"."tblGenArtifact"."Quantity",
                 "public"."tblGenArtifactForm"."GenArtifactForm",
                 "public"."tblContext"."DAACSPhase",
                  "public"."tblContext"."MasterContextNumber",
                 "public"."tblContext"."Context",
"public"."tblContext"."QuadratID"
                 FROM
                 "public"."tblContext"
                 INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                 INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                 INNER JOIN "public"."tblProject" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"                       
                 INNER JOIN "public"."tblProjectName" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"                       
                 INNER JOIN "public"."tblGenArtifact" ON "public"."tblGenArtifact"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                 INNER JOIN "public"."tblGenArtifactForm" ON "public"."tblGenArtifactForm"."GenArtifactFormID" = "public"."tblGenArtifact"."GenArtifactFormID"
                 WHERE                     
                 "public"."tblContext"."ProjectID" = \'1243\'
                  OR
                  "public"."tblContext"."ProjectID" = \'1250\'
                  OR
                  "public"."tblContext"."ProjectID" = \'1251\'

                 ') 

GenArts <- GenArts %>% mutate(Block=MasterContextNumber)

# Summarize Forms by ProjectID, DAACS Phase
GenArtsSum <-  GenArts %>% group_by(ProjectID, QuadratID, DAACSPhase, Block, GenArtifactForm) %>% 
  summarise(count = sum(Quantity))

###C1. Category Assignments ##############

#Assign categories to different gen art forms
GenArtsSum <-
  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Doll, Frozen Charlotte',
                                                        'Doll, head','Doll, limb', 'Doll, other', 
                                                        'Doll, eye', 'Gaming Piece', 'Gaming Piece, die',
                                                        'Domino', 'Marble, toy','Toy, car','Toy, cannon',
                                                        'Toy, dish','Toy, figurine','Toy, other'), 'Toy_Game', GenArtifactForm))

GenArtsSum <-
  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Figurine','Plate, trunk','Chandelier/Epergne Pendant',
                                                        'Tack, antimacassar'),
                                 'DecorativeHome', Category))

GenArtsSum <-
  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Fan Blade/Part','Spectacles','Eye Glass','Aiglet',
                                                        'Crinoline, clamp','Eye, clothing','Rivet, clothing',
                                                        'Fastener, clothing','Purse Part, clasp','Purse Part, mesh',
                                                        'Fastener, corset','Parasol/Umbrella, other',
                                                        'Parasol/Umbrella, stretcher/rib','Hook, clothing',
                                                        'Rivet, clothing', 'Hair Clasp','Suspender Brace',
                                                        'Suspender, hook',
                                                        'Shoe Sole','Shoe Upper','Shoe, tip','Shoe, tap',
                                                        'Shoe, heel','Rivet, shoe',
                                                        'Brooch','Chain, watch','Charm, hand','Cuff Link','Jewel',
                                                        'Jewelry, earring','Jewelry, other','Jewelry, pendant',
                                                        'Jewelry, Pin', 'Medal, religious', 'Pendant',
                                                        'Ring, jewelry', 'Clock/Watch part','Key, watch','Watch Gear',
                                                        'Watch Part'), 
                                 'Adornment', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Slate, writing', 'Pencil, slate'), 
                                          'Education', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Musical Instrument, key',
                                                                 'Musical Instrument, unidentifiable',
                                                                 'Harmonica Plate and Reed',
                                                                 'Harmonica Reed','Accordion Plate and Reed',
                                                                 'Accordion Plate','Harmonica Plate','Jews/Jaw Harp',
                                                                 'Musical Instrument'), 
                                          'Music', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Gun Part, unidentified',
                                                                 'Gun, hammer',
                                                                 'Gun, plate',
                                                                 'Flask, powder','Gunflint','Fish Hook',
                                                                 'Weight, Net'), 
                                          'FoodProcurement', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Comb, folding',
                                                                 'Comb, hair',
                                                                 'Comb, nit/lice','Curling Iron','Toothbrush',
                                                                 'Razor'), 
                                          'Hygiene', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Needle','Needle Case','Pin, straight',
                                                                 'Scissors','Thimble',
                                                                 'Blank, button','Bodkin','Bobbin, lace',
                                                                 'Darning Egg','Hook, crochet','Hook, tambour',
                                                                 'Knitting Needle Guards','Needle, mattress','Thread Spool',
                                                                 'Sewing Equipment, unidentified'), 
                                          'Sewing', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Fork, toasting'), 
                                          'Utensil', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Horse Furniture','Stirrup','Spur'), 
                                          'Horse', Category))

GenArtsSum <-  mutate(GenArtsSum, Category=ifelse(GenArtifactForm %in% c('Plate, printing'), 
                                          'Social', Category))

# Create dataframe by block and category
GenArtSum<-GenArtsSum %>% group_by(ProjectID, QuadratID, DAACSPhase, Block, Category) %>%  summarise(count = sum(count))

# Filter by Categories created above
GenArtSum2 <- filter(GenArtSum, Category %in% c('Adornment', 'Hygiene',
                                            'Medical','Utensil', 'DecorativeHome',
                                            'Sewing', 'Horse', 'Music', 
                                            'GunParts_Ammunition_FoodProcurement',
                                            'Social', 'Education',
                                            'Toy_Game'))

### D. Pipes ######################
TobPipes <-dbGetQuery(DRCcon,'
                    SELECT
                    "public"."tblContext"."ProjectID",
                    "public"."tblTobaccoPipe"."Quantity",
                    "public"."tblContext"."DAACSPhase",
                    "public"."tblContext"."MasterContextNumber",
                    "public"."tblContext"."Context",
 "public"."tblContext"."QuadratID"
                    FROM
                    "public"."tblContext"
                    INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                    INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                    INNER JOIN "public"."tblTobaccoPipe" ON "public"."tblTobaccoPipe"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                    WHERE
                    "public"."tblContext"."ProjectID" = \'1243\'

                     OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                    ') 

TobPipes <- TobPipes %>% mutate(Block=MasterContextNumber)

# Summarize by ProjectID, DACSPHase
TobPipeSum <-  TobPipes %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

# Add Category
TobPipeSum$Category <- 'Pipe'

### E. Beads, Buckles, Buttons ############

### E1. Beads ############

Beads<-dbGetQuery(DRCcon,'
                   SELECT
                   "public"."tblContext"."ProjectID",
                   "public"."tblBeadMaterial"."BeadMaterial",
                   "public"."tblContext"."DAACSPhase",
                  "public"."tblContext"."MasterContextNumber",
 "public"."tblContext"."QuadratID",
                   "public"."tblBead"."Quantity"
                   FROM
                   "public"."tblContext"
                   INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                   INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                   INNER JOIN "public"."tblBead" ON "public"."tblBead"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                   INNER JOIN "public"."tblBeadMaterial" ON "public"."tblBead"."BeadMaterialID" = "public"."tblBeadMaterial"."BeadMaterialID"
                   WHERE
                   "public"."tblContext"."ProjectID" = \'1243\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                     OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                   ') 

Beads <- Beads %>% mutate(Block=MasterContextNumber)

# Summarize by ProjctID, DAACSPHase
BeadSum <-  Beads %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

# Add category assignment
BeadSum$Category <- 'Beads'

###E2. Buttons############

Buttons<-dbGetQuery(DRCcon,'
                     SELECT
                     "public"."tblContext"."ProjectID",
                     "public"."tblButtonMaterial"."ButtonMaterial",
                     "public"."tblButton"."Quantity",
                     "public"."tblContext"."DAACSPhase",
                     "public"."tblContext"."MasterContextNumber",
                     "public"."tblContext"."Context",
 "public"."tblContext"."QuadratID"
                     FROM
                     "public"."tblContext"
                     INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                     INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                     INNER JOIN "public"."tblButton" ON "public"."tblButton"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                     INNER JOIN "public"."tblButtonMaterial" ON "public"."tblButton"."ButtonMaterialID" = "public"."tblButtonMaterial"."ButtonMaterialID"
                     WHERE
                     "public"."tblContext"."ProjectID" = \'1243\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                     ') 

Buttons <- Buttons %>% mutate(Block=MasterContextNumber)

# Summarize by ProjectID, DAACSPhase 
ButtonSUM <-  Buttons %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

# Add category assignment
ButtonSUM$Category <- 'Buttons'

###E3. Buckle############
Buckles<-dbGetQuery(DRCcon,'
                     SELECT
                     "public"."tblContext"."ProjectID",
                     "public"."tblBuckle"."Quantity",
                     "public"."tblBuckleType"."BuckleType",
                     "public"."tblContext"."DAACSPhase",
                     "public"."tblContext"."MasterContextNumber",
                     "public"."tblContext"."Context",
"public"."tblContext"."QuadratID"
                     FROM
                     "public"."tblContext"
                     INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                     INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                     INNER JOIN "public"."tblBuckle" ON "public"."tblBuckle"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                     INNER JOIN "public"."tblBuckleType" ON "public"."tblBuckle"."BuckleTypeID" = "public"."tblBuckleType"."BuckleTypeID"
                     WHERE
                     "public"."tblContext"."ProjectID" = \'1243\'

                    OR
                    "public"."tblContext"."ProjectID" = \'1250\'
                    OR
                    "public"."tblContext"."ProjectID" = \'1251\'
                     ') 

Buckles <- Buckles %>% mutate(Block=MasterContextNumber)

#Summarize by ProjectID, DAACSPhase
BuckleSUM <-  Buckles %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

# Assign category
BuckleSUM$Category <- 'Adornment'

###F. Utensils #############

Utensils <-dbGetQuery(DRCcon,'
                    SELECT
                    "public"."tblContext"."ProjectID",
                    "public"."tblContext"."Context",
                    "public"."tblUtensil"."Quantity",
                    "public"."tblContext"."DAACSPhase",
                    "public"."tblContext"."MasterContextNumber",
 "public"."tblContext"."QuadratID"
                    FROM
                    "public"."tblContext"
                    INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                    INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                    INNER JOIN "public"."tblUtensil" ON "public"."tblUtensil"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                    WHERE
                    "public"."tblContext"."ProjectID" = \'1243\'
                    OR
                      "public"."tblContext"."ProjectID" = \'1250\'
                      OR
                      "public"."tblContext"."ProjectID" = \'1251\'
                    ') 
Utensils <- Utensils %>% mutate(Block=MasterContextNumber)

# Summarize by ProjectID, DAACSPhase
UtensilSum <-  Utensils %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

# Assign category
UtensilSum$Category <- 'Utensil'

###G. Ceramic Discs #############

Ceramic<-dbGetQuery(DRCcon,'
SELECT
                         "public"."tblCeramic"."Quantity",
                         "public"."tblCeramicWare"."Ware",
                         "public"."tblCeramicCEWType"."CeramicCEWType",
                         "public"."tblContext"."ProjectID",
                         "public"."tblContext"."DAACSPhase",
"public"."tblContext"."QuadratID",
                         "public"."tblContext"."MasterContextNumber",
                          "public"."tblCeramicForm"."CeramicForm",
                          "public"."tblCeramicMaterial"."CeramicMaterial"
                         FROM
                         "public"."tblProjectName"
                         INNER JOIN "public"."tblProject" ON "public"."tblProject"."ProjectNameID" = "public"."tblProjectName"."ProjectNameID"
                         INNER JOIN "public"."tblContext" ON "public"."tblContext"."ProjectID" = "public"."tblProject"."ProjectID"
                         INNER JOIN "public"."tblContextSample" ON "public"."tblContextSample"."ContextAutoID" = "public"."tblContext"."ContextAutoID"
                         INNER JOIN "public"."tblGenerateContextArtifactID" ON "public"."tblContextSample"."ContextSampleID" = "public"."tblGenerateContextArtifactID"."ContextSampleID"
                         INNER JOIN "public"."tblCeramic" ON "public"."tblCeramic"."GenerateContextArtifactID" = "public"."tblGenerateContextArtifactID"."GenerateContextArtifactID"
                         INNER JOIN "public"."tblCeramicWare" ON "public"."tblCeramic"."WareID" = "public"."tblCeramicWare"."WareID"
                         LEFT JOIN "public"."tblCeramicCEWType" ON "public"."tblCeramic"."CeramicCEWTypeID" = "public"."tblCeramicCEWType"."CeramicCEWTypeID"
INNER JOIN "public"."tblCeramicForm" ON "public"."tblCeramic"."CeramicFormID" = "public"."tblCeramicForm"."CeramicFormID"
INNER JOIN "public"."tblCeramicMaterial" ON "public"."tblCeramic"."CeramicMaterialID" = "public"."tblCeramicMaterial"."CeramicMaterialID"
                         WHERE
                         "public"."tblContext"."ProjectID" = \'1243\'
                          OR
                         "public"."tblContext"."ProjectID" = \'1250\'
                           OR
                          "public"."tblContext"."ProjectID" = \'1251\'
                          ')

### G2. Ceramic Gaming Discs ##########

# Filter by Form
CeramicDiscs <- Ceramic %>% filter(grepl('Gaming',CeramicForm))

CeramicDiscs <- CeramicDiscs %>% mutate(Block=MasterContextNumber)

# Summarize by ProjectID, DAACSPhase
CeramicDiscSUM <-  CeramicDiscs %>% group_by(ProjectID, QuadratID, DAACSPhase, Block) %>% summarise(count = sum(Quantity))

# Assign category
CeramicDiscSUM$Category <- 'CeramicDisc'

## H. One Table ######################

# Bind together all artifact category tables
smallFindsX <- bind_rows(GlassSum, TobPipeSum, UtensilSum, GenArtSum2, BuckleSUM, BeadSum, ButtonSUM, CeramicDiscSUM)

# Split Quad ID N and E
smallFinds <- smallFindsX %>% filter(Block=='STP')

# Remove Ns
smallFinds <- smallFinds %>% separate(QuadratID, c("N", "E"), "E")
smallFinds2 <- smallFinds %>% separate(N, c("X", "N"), "N")

smallFinds2$Block <- NA

smallFinds2$Block[smallFinds2$N > 4590 & smallFinds2$E >6554 ] <- 'BlockA'

smallFinds2$Block[smallFinds2$E >6660 ] <- 'BlockC'

smallFinds2$Block[smallFinds2$N < 4594 & smallFinds2$E < 6596 ] <- 'BlockFG'

smallFinds2$Block[(smallFinds2$N < 4594 & smallFinds2$N > 4560) & (smallFinds2$E < 6660 & smallFinds2$E > 6590) ] <- 'BlockB'

smallFinds2$Block[(smallFinds2$N < 4550) & (smallFinds2$E < 6660 & smallFinds2$E > 6596) ] <- 'BlockD'

smallFinds3 <- smallFinds2 %>% select(ProjectID, DAACSPhase, Block, count, Category)

smallFindsY <- filter(smallFindsX, ! Block=='STP')

smallFinds4 <- rbind(smallFindsY,smallFinds3)

smallFinds4$Block[smallFinds4$Block == 'BlockF'] <- 'BlockFG'
smallFinds4$Block[smallFinds4$Block == 'BlockG'] <- 'BlockFG'

smallFinds4$DAACSPhase[smallFinds4$DAACSPhase == ''] <- NA
smallFinds4$Block[smallFinds4$Block == ''] <- NA

smallFinds5 <- smallFinds4 %>% filter(! is.na(Block)) %>%
filter(! is.na(DAACSPhase)) 

# Summarize file by Category
smallFindSum <- smallFinds5 %>% group_by(ProjectID, DAACSPhase, Block, Category) %>%

  summarise(count = sum(count)) %>%
  
  spread(key=Category, value=count,  fill=0, drop=F)

smallFindSum2 <- filter(smallFindSum, ! WBG == 0)

### D. Bring in MCD data
smallFindSum2$ProjPhase <- paste(smallFindSum2$ProjectID, smallFindSum2$DAACSPhase, sep="_")
dates <- read.csv("projMCD.csv", header=TRUE, stringsAsFactors = FALSE)

smallFindSum3 <- merge(smallFindSum2, dates, by="ProjPhase")


## I. Adornment ######################

adorn <- smallFindSum3 %>% select(blueMCD, Block,DAACSPhase,WBG,Adornment)

adorn <- adorn %>% mutate(total = Adornment+WBG) %>% mutate(AI = Adornment/total)

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


# run function on data
wbgCI <- adjustedWaldCI(adorn$Adornment,adorn$total,0.05)
adorn$gCIUpper <- wbgCI$upperCL
adorn$gCILower <- wbgCI$lowerCL
adorn$gp <- wbgCI$pTilde

adorn <- adorn %>% mutate(gp=ifelse(Adornment==0, 0, gp))

write.csv(adorn, "Adornment_MP.csv")

# Plot Data

set.seed(42)
a<-ggplot(adorn, aes(x=adorn$blueMCD, y=adorn$gp, fill=adorn$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = adorn$gCILower,ymax = adorn$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Adornment/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  #scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
a
ggsave("MPAI_ByBlock_AdornmentWBG.png", a, width=10, height=7.5, dpi=300)

## J. Beads ######################

bead <- smallFindSum3 %>% select(blueMCD, Block,DAACSPhase,WBG,Beads)

bead <- bead %>% mutate(total = Beads+WBG) %>% mutate(AI = Beads/total)


# run function on data
wbgCI <- adjustedWaldCI(bead$Beads,bead$total,0.05)
bead$gCIUpper <- wbgCI$upperCL
bead$gCILower <- wbgCI$lowerCL
bead$gp <- wbgCI$pTilde

bead <- bead %>% mutate(gp=ifelse(Beads==0, 0, gp))

write.csv(bead, "Beads_MP.csv")


# Plot Data
set.seed(42)
b<-ggplot(bead, aes(x=bead$blueMCD, y=bead$gp, fill=bead$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = bead$gCILower,ymax = bead$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Beads/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  #scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
b
ggsave("MPAI_ByBlock_BeadsWBG.png", b, width=10, height=7.5, dpi=300)

## K. Buttons ######################

button <- smallFindSum3 %>% select(blueMCD, Block,DAACSPhase,WBG,Buttons)

button <- button %>% mutate(total = Buttons+WBG) %>% mutate(AI = Buttons/total)

# run function on data
wbgCI <- adjustedWaldCI(button$Buttons,button$total,0.05)
button$gCIUpper <- wbgCI$upperCL
button$gCILower <- wbgCI$lowerCL
button$gp <- wbgCI$pTilde

button <- button %>% mutate(gp=ifelse(Buttons==0, 0, gp))

write.csv(button, "Buttons_MP.csv")


# Plot Data
set.seed(42)
c<-ggplot(button, aes(x=button$blueMCD, y=button$gp, fill=button$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = button$gCILower,ymax = button$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("buttons/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  #scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
c
ggsave("MPAI_ByBlock_buttonsWBG.png", c, width=10, height=7.5, dpi=300)

## L. Disc ######################

disc <- smallFindSum3 %>% select(blueMCD, Block,DAACSPhase,WBG,CeramicDisc)

disc <- disc %>% mutate(total = CeramicDisc+WBG) %>% mutate(AI = CeramicDisc/total)

# run function on data
wbgCI <- adjustedWaldCI(disc$CeramicDisc,disc$total,0.05)
disc$gCIUpper <- wbgCI$upperCL
disc$gCILower <- wbgCI$lowerCL
disc$gp <- wbgCI$pTilde

disc <- disc %>% mutate(gp=ifelse(CeramicDisc==0, 0, gp))

write.csv(disc, "Discs_MP.csv")


# Plot Data
set.seed(42)
d<-ggplot(disc, aes(x=disc$blueMCD, y=disc$gp, fill=disc$Block))+
  geom_point(shape=21, size=5, alpha = .75, colour="black")+
  geom_errorbar(aes(ymin = disc$gCILower,ymax = disc$gCIUpper), color="black", width=0.5) + 
  xlab("Phase BlueMCD") + ylab("Abundance Index") + #set axis labels
  ggtitle(expression(atop("Discs/WBG Index", atop(italic("95% Confidence Intervals"), "")))) + #set title
  theme_classic() +
  #scale_y_continuous(limits=c(0.0,0.3), breaks=seq(0, 0.3, 0.05))+
  scale_x_continuous(limits=c(1750,1830), breaks=seq(1750,1830,10)) +
  #  geom_text_repel(aes(label=ProjectName), size=5, color="black") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=16,face="bold")) + theme(plot.title = element_text(size=18,face="bold")) +
  scale_fill_manual(name="Block", values=c("blue", "pink", "green", "red", "yellow", "orange", "black", "grey"))
d
ggsave("MPAI_ByBlock_discsWBG.png", d, width=10, height=7.5, dpi=300)
