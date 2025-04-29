

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))

rm ( list = ls ( )) 
#########################################################################################################################
#########################################################################################################################
################################ R SCRIPT FOR ANALYZING F1 GENOTYPE DATA FROM LGC  ######################################
### Author: Luis Posadas
### Date: July 2016

### The following script allows for the analysis of F1 SNP genotype 
### The data analyzed represents biallelic SNP calls of previously known marker loci
### This script is intended to help with the F1 screening to identify true hybrid plants and plant that are the result of a selfing event
### This script works with genotyping files produced by LGC Genomics
### The script also needs an input file with information as shown in example below


#########################################################################################################################
#########################################################################################################################
####################### Script section requires manual input #########################################

### NOTE as of 09.17.18: Consider a way to substract the probability of "True self" from the probability of "True hybrid" at the end of script
### Also make the count of "trueHeterogenenousSNPs" true, because now it is just a subtraction, which is not accurate always
### Eskridge talked about weighing the probabilities


inputFile <- "Hybrid_Genotypes.csv"
inputFile2 <- "Polymorphic_SNPs.csv"
Type_of_Crosses <- "Single_crosses"
inputFile3 <- "Parental_Genotypes.csv"


inputDir <- "~/Downloads/Hybrid_Screening_Manuscript/"
inputDir2 <- "~/Downloads/Hybrid_Screening_Manuscript/"
inputDir3 <- "~/Downloads/Hybrid_Screening_Manuscript/"
outputDir <- "~/Downloads/Hybrid_Screening_Manuscript/"


####################### First input file #########################################

#### This file contains the F1 hybrid genotype data that was obtained from F1 plants (also contains the selfs)

setwd ( inputDir )
list.files()
HybridData00 <- read.csv ( inputFile , header = T , stringsAsFactors = F )
dim ( HybridData00 )
# HybridData00 [ 1:5 ,   ]

## Convert any empty cells (unlikely) to NA's
HybridData00 [  HybridData00 == "" ] <- NA


###  Two lines added on Sep 2024 because found out that if genotype data comes in with full NA columns, then there is an error when collecting hybrids and selfs below (strsplit ":")
columnsWzeroData <- which ( apply ( HybridData00 , 2 , function ( x ) sum ( is.na ( x ) ) ) == dim ( HybridData00 ) [ 1 ] )

HybridData <- HybridData00 [ , !colnames ( HybridData00 ) %in% names ( columnsWzeroData ) ] 
dim ( HybridData00 ) 
dim ( HybridData )



########################### Second input file #########################################

### The minimal columns for this file to be useful are:

#   FEMALE       MALE       Plate_ID Well_ID    UX_DASH_NUMBER              MATERIAL            STATUS Shipping_Plate_ID Shipping_Well_ID BARC.013211.00449 BARC.042403.08252 BARC.017957.02482 BARC.047715.10388
#1  Parent_1 Parent_2     Lincoln_Plate_4      D7         UX3358-2 SCN Yield Crosses 2016   Active                NA               NA                 0                 0 BARC.017957.02482 BARC.047715.10388
#2  Parent_1 Parent_2     Lincoln_Plate_4      D6         UX3358-6 SCN Yield Crosses 2016   Active                NA               NA                 0                 0 BARC.017957.02482 BARC.047715.10388
#3  Parent_1 Parent_2     Lincoln_Plate_4      C7         UX3358-1 SCN Yield Crosses 2016   Active                NA               NA                 0                 0 BARC.017957.02482 BARC.047715.10388

### The "STATUS" column above refers to whether there was tissue collected from an F1 "Active"
### Or whether the F1 plant was dead or didn't germinate "Empty"


####### IMPORTANT....
setwd ( inputDir )
list.files()

### For single crosses
Consolidated00 <- read.csv ( inputFile2 , header = T , stringsAsFactors = F )
dim ( Consolidated00 )
# names ( Consolidated00 )
# names ( HybridData )

### IMPORTANT: THIS STEP BELOW, pulls out SNP IDs with the text pattern specified below
### If ever, SNP IDs change, then, this would have to be edited. Say MIPs. 
### As of 10.12.19, there are only 4 main patterns as part of the F1 screening and MAS work
### Updated as of 01.20.2021 with new SNP IDs including Gm* and KS*
### As of 10.15.23, there are additional patterns. The "Glyma" types. 
Consolidated01 <- names ( Consolidated00 [ c ( grep ( "BARC" , names ( Consolidated00 ) ) , grep ( "Rhg" , names ( Consolidated00 ) ) , grep ( "GSM" , names ( Consolidated00 ) ) , grep ( "P34" , names ( Consolidated00 ) ) , grep ( "Gm" , names ( Consolidated00 ) ) , grep ( "KS" , names ( Consolidated00 ) ) , grep ( "SNAP" , names ( Consolidated00 ) ) ,  grep ( "Glyma" , names ( Consolidated00 ) ) ) ] )
# Consolidated01

Consolidated02 <- Consolidated01 [ !Consolidated01 %in% names ( HybridData ) ]

Consolidated <- Consolidated00 [ , !names ( Consolidated00 ) %in%  Consolidated02 ]
# names ( Consolidated )
dim ( Consolidated )
# Consolidated [ 1 : 5 , ]


####################### Third input file #########################################

#### This file contains the parental SNP genotypes:

setwd ( inputDir3 )
list.files()

data00  <- read.csv ( inputFile3 , header = T , stringsAsFactors = F )
dim ( data00 )
 ### IMPORTANT: THIS STEP BELOW, pulls out SNP IDs with the text pattern specified below. Basically a sub-setting step
 ### This help in loops below in case there are unwanted IDs that are lacking in one object but present in another
 ### If ever, SNP IDs change, then, this would have to be edited. Say MIPs. 
 ### As of 10.12.19, there are only 4 main patterns as part of the F1 screening and MAS work
 
 ### Updated as of 01.20.2021 with new SNP IDs including Gm* and KS*
 ### As of 10.15.23, there are additional patterns. The "Glyma" types. 
 data01 <- colnames ( data00 [ c ( grep ( "BARC" , names ( data00 ) ) , grep ( "Rhg" , names ( data00 ) ) , grep ( "GSM" , names ( data00 ) ) , grep ( "P34" , names ( data00 ) ) , grep ( "Gm" , names ( data00 ) ) , grep ( "KS" , names ( data00 ) ) , grep ( "SNAP" , names ( data00 ) ) , grep ( "Glyma" , names ( Consolidated00 ) ) ) ] )
 data02 <- data01 [ !data01 %in% names ( HybridData ) ]
 data15 <- data00 [ , !names ( data00 ) %in% data02 ]
 # names ( data15 )
 dim ( data15 )

## Removing duplicate sample IDs. This may not occur often, so may not need this step but it does not hurt running the lines.
dups <- data15 [ duplicated ( data15 ) , ]
dupsOut <- data15 [ !duplicated ( data15 ) , ]
dim( dupsOut )

## Rename back to object name "data15" to simplify written code below
data15 <- dupsOut
rownames (data15) <- data15$DNA...Assay
# str(data15)


## Modification as of 09.11.18
data15Dummy <- data15 [ , names ( data15 ) %in% names ( Consolidated ) ]
# head ( data15Dummy )
### Extract column names from LGC results file
SNPs <- as.vector ( colnames ( data15Dummy ) )
## Removing the first column from LGC file to obtain SNP names only. The first column contains the sample names
SNPs1 <- SNPs
# length ( SNPs1 )



####################### Fourth input file #########################################

## Note on 02.15.18: The file entered below is the equivalent of that imported above in File 1 (above)
## The file below is the excel version of it, to which I added "Shipping_Plate_ID"	and "Shipping_Well_ID" information manually.
## As of 09.06.18, File 1 imported above has the shipping plate and well information, so importing this file is no longer necessary.
## Hence, object will simple be renamed.

UX_all <- Consolidated
dim ( UX_all )
# UX_all [ 1:5 ,   ]


## As of 09.06.18 carry on with below step
## Permanent checkpoint step as of 02.13.19

if ( any ( grep ( "plate" , names ( UX_all ) , ignore.case = T ) ) ) {

  names ( UX_all ) [ grep ( "plate" , names ( UX_all ) , ignore.case = T ) ]  <- "Shipping_Plate_XX"
  names ( UX_all ) [ grep ( "well" , names ( UX_all ) , ignore.case = T ) ]  <- "Shipping_Well_ID"
  UX_all$Shipping_Plate_ID <- "Shipping_Plate_XX"
  UX_all$Shipping_Well_ID <- "XX"
  
  } else {

  UX_all$Shipping_Plate_ID <- "Shipping_Plate_XX"
  UX_all$Shipping_Well_ID <- "XX"
}

# colnames ( UX_all )
           
UX_all1 <- UX_all
dim ( UX_all1 )

# colnames ( UX_all1 )
# colnames ( UX_all )






####################### Output file information #########################################

setwd ( outputDir )
Season <- "Small_Set_For_Manuscript"
FileToExportResults <- paste ( "Analyzed_F1_data" , Season ,  sep = "_" )


## Assuming that the expected frequency of a line in the crossing block and assuming that in F4 generation (line-derivation generation) 
## the plant was heterozygous at some SNP locus, also assuming that the earliest the plant made it to the crossing block occurred 
## in generation F4:7, then, in generation F4:8 (the generation of the seed in false cross-derived pod) the frequency of 
## heterozygous plants at the same locus within that line will be 1/16 (6.25%).
freqHetSeedInF4_8 <- 1/16

## In a similar manner, the probability of observing one of the two non-heterozygous genotypes in a plant of an F4:7 line
## that was heterozygous in F4:
freqNonHetPlantInF4_7OneHomClass <- 7/16




############################ End of script section that needs manual input ##############################################
#########################################################################################################################







##################################################################################################################################
########################### Select and run code below  #############################################

### Remove rows with "Empty" status, i.e., there was no tissue for those samples // Also subset the columns used to the listed below
Consolidated1 <- Consolidated [ Consolidated$STATUS != "Empty" , which ( names (Consolidated) %in% c( "FEMALE" , "MALE" , "POPULATION" , "UX_DASH_NUMBER" , "MATERIAL" , "STATUS" , "SNPsum") ) ]
dim ( Consolidated1 )
# Consolidated1 [ 1 : 5 , ]


### Create empty matrix with NAs with the row dimensions of Consolidated1 and column dimensions depending on the number of SNPs used for genotyping
SNPtable <- matrix ( NA , nrow = dim ( Consolidated1 ) [ 1 ] , ncol = length ( SNPs1 ) )

### Bind objects
Consolidated2 <- cbind ( Consolidated1 , SNPtable )
colnames ( Consolidated2 ) <- c( colnames ( Consolidated1 ) , SNPs1 )
# Consolidated2 [ 1:10 , ]
# str ( Consolidated2 )



## Here I create a list using the shipping plate IDs as the splitting factor. The IDs into a single column were consolidated manually before import
UX_all2 <- split ( UX_all1 , UX_all1$Shipping_Plate_ID , drop = T)
length ( UX_all2 )
# colnames ( UX_all2 [[ 1 ]] )

ListSNPColStart <- dim ( UX_all1 ) [ 2 ] - ( length ( SNPs1 ) + 1 ) + 1 

### to convert any unwanted "" character into NAs
UX_all2 [ UX_all2 == "" ] <- NA
# str ( UX_all2 [[ 1 ]] )

### This loop populates SNP information in the Consolidated2 object based on the ACTUAL SNPs selected for genotyping (as opposed to all possible polymorphic SNPs)
UX_DASH_NUMBER <- "UX_DASH_NUMBER"
SNPcolStart <- ( dim ( Consolidated2 ) [ 2 ] - length ( SNPs1 ) ) + 1

### Here I add a "1" to length ( SNPs1 ) because UX_all1 has the "SNPsum" column at the end. So, I need to account for that. 
# i <- 1
# j <- 1

ptm <- proc.time() # Start the clock!
UX_all2Sub1 <- list()
UX_all2Sub2 <- list()
  for ( i in 1 : dim ( Consolidated2 ) [ 1 ] ) {
#    for ( i in 1 : 1 ) {
    for ( j in 1 : length ( UX_all2 ) ) {
#      for ( j in 1 : 5 ) {
    TempUX <- Consolidated2 [ i , UX_DASH_NUMBER ]
    UX_all2Sub1 [[ j ]] <- UX_all2 [[ j ]] [ UX_all2 [[ j ]] [ , UX_DASH_NUMBER ] %in% TempUX , ]
      if ( dim ( UX_all2Sub1 [[ j ]] ) [ 1 ] > 0 ) {
        UX_all2Sub2 [[ j ]] <- UX_all2Sub1 [[ j ]] [ , colSums ( is.na ( UX_all2Sub1 [[ j ]] ) ) < dim ( UX_all2Sub1 [[ j ]] ) [ 1 ] ]
        for ( m in 1 :  dim ( UX_all2Sub2 [[ j ]] ) [ 1 ]  ) {
          for ( k in SNPcolStart : dim ( Consolidated2 ) [ 2 ] ) {
             if ( is.na ( Consolidated2 [ i , k ] ) ) {
              ### The line below has a "-1" bacause the last column in UX_all2Sub2 is not a SNP, it is a SNP count column "SNPsum"
              for ( l in ListSNPColStart : ( dim ( UX_all2Sub2 [[ j ]] ) [ 2 ] - 1 ) ) {
                if ( !is.na ( unique ( UX_all2Sub2 [[ j ]] [ m , l ] ) ) ) {
                  if ( ( TempUX  ==   UX_all2Sub2 [[ j ]] [ m , UX_DASH_NUMBER ] ) & ( colnames ( Consolidated2 [ k ] )  ==  colnames ( UX_all2Sub2 [[ j ]] [ l ] ) ) ) {
                    Consolidated2 [ i , k ] = UX_all2Sub2 [[ j ]] [ m , l ] 
                  }}}}}}}}
print ( i )    
}
proc.time() - ptm # Stop the clock

### Identify which SNP columns in UX_all2 list are empty ( NA's in the entire column ) and make a subset by removing those empty columns

UX_all3 <- list ()
for ( i in 1 : length ( UX_all2 ) )
  {
  UX_all3 [[ i ]] <- UX_all2 [[ i ]] [ , colSums ( is.na ( UX_all2 [[ i ]] ) ) < dim ( UX_all2 [[ i ]] ) [ 1 ] ]
  }


############################################################################################################################################################# 
############################# Second section ########################################

### Beginning of loop:
ptm <- proc.time() # Start the clock!
FemMaleList <- list ()
NewColStarList <- list ()
# i <- 1 
# for ( i in 1 : 1 )
  for ( i in 1 : length ( UX_all3 ) )
{

  

TempUX <- UX_all3 [[ i ]] 
FemSubset <- data15 [ unique ( TempUX$FEMALE ) , ]
MaleSubset <- data15 [ unique ( TempUX$MALE ) , ]
FEMALE <- "FEMALE"
MALE <- "MALE"



### This loop will identify the index of the first and last SNPs among the SNPs present within a given list.
###  This will be handy to identify which columns to use in loops below. 
SNPsInList1 <- NULL
for ( z in 1 : dim ( TempUX ) [ 2 ] )
  {
  SNPsInList <- sum ( colnames ( TempUX [ z ] ) == SNPs1 )
  SNPsInList1 <- rbind ( SNPsInList1 , SNPsInList )
  }
ColIndex <- which ( SNPsInList1 > 0 )
SNPs2 <- colnames ( TempUX [ ColIndex ] )

### Here, I further subset the parental genotype information object by keeping only SNPs present in current list, for both, females and males
FemSubset1 <- FemSubset [ which ( names ( FemSubset ) %in% SNPs2 ) ]
MaleSubset1 <- MaleSubset [ which ( names ( MaleSubset ) %in% SNPs2 ) ]

### For some reason rownames ( FemSubset [ , ] ) appears as NULL when indexing and there is only one column in the data frame. After adding a second column is works well. 
### It seems as if rownames does not work when having only one column, the same way colnames does not work when having only one row.
FemSubset2 <- cbind.data.frame ( FemSubset$DNA...Assay , FemSubset1 , stringsAsFactors = F )
MaleSubset2 <- cbind.data.frame ( MaleSubset$DNA...Assay , MaleSubset1 , stringsAsFactors = F )


### Need empty object to dump the FEMALE genotype information for each SNP
FemGenos <- as.data.frame ( matrix ( NA , nrow = dim ( TempUX ) [ 1 ] , ncol = length ( SNPs2 ) ) )
timeStart <- proc.time()
### This loop will pull out genotype information for FEMALES only that are included in the each list
 for ( e in 1 : length ( SNPs2 ) ) {
#  for ( e in 1 : 1 ) {
  for ( a in 1 : dim ( TempUX ) [ 1 ] ) {
    if ( !is.na ( TempUX [ a , FEMALE ] ) ) {
      if ( is.na ( FemGenos [  a , e ] ) ) {
      tempFemSub <- FemSubset2 [  FemSubset2 [ , 1 ]  %in% TempUX [ a , FEMALE ] , ]
      for ( c in 2 : ( ( 2 + e ) - 1 ) ) {
        for ( d in min ( ColIndex ) : max ( ColIndex ) ) {
            if ( colnames ( tempFemSub [ c ] ) == colnames ( TempUX [ d ] ) ) {
            FemGenos [ a , e ] = tempFemSub [ 1 , c ] 
}}}}}}}
proc.time() - timeStart

# dim ( FemGenos )

### This loop will pull out genotype information for MALES only that are included in the each list
MaleGenos <- as.data.frame ( matrix ( NA , nrow = dim ( TempUX ) [ 1 ] , ncol = length ( SNPs2 ) ) )
# emptyColnames <- NULL
timeStart <- proc.time()
for ( e in 1 : length ( SNPs2 ) ) {
  for ( a in 1 : dim ( TempUX ) [ 1 ] ) {
    if ( !is.na ( TempUX [ a , MALE ] ) ) {
      if ( is.na ( MaleGenos [  a , e ] ) ) {
        tempMaleSub <- MaleSubset2 [  MaleSubset2 [ , 1 ]  %in% TempUX [ a , MALE ] , ]
        for ( c in 2 : ( ( 2 + e ) - 1 ) ) {
          for ( d in min ( ColIndex ) : max ( ColIndex ) ) {
            if ( colnames ( tempMaleSub [ c ] ) == colnames ( TempUX [ d ] ) ) {
            MaleGenos [ a , e ] = tempMaleSub [ 1 , c ]
            # emptyColnames <- rbind ( emptyColnames , colnames ( tempMaleSub [ c ] ) )
}}}}}}} 
proc.time() - timeStart



NewFemCOl <- paste ( colnames ( tempFemSub [ 2 : length ( tempFemSub ) ] ) , c( ".F" ) , sep = "_" )
NewMaleCOl <- paste ( colnames ( tempMaleSub [ 2 : length ( tempMaleSub ) ] ) , c( ".M" ) , sep = "_" )
colnames ( FemGenos ) <- NewFemCOl
colnames ( MaleGenos ) <- NewMaleCOl

FemMaleFrame <- cbind ( FemGenos , MaleGenos )
FemMaleFrame1 <- cbind ( FemMaleFrame , TempUX [ , names ( TempUX ) %in% SNPs2 ] )
colnames ( FemMaleFrame1 ) <- c( names( FemMaleFrame ) , SNPs2 )


FemMaleFrame2 <- FemMaleFrame1 [ , order ( names ( FemMaleFrame1 ) ) ]
w <- sapply ( FemMaleFrame2 , is.factor )
FemMaleFrame2 [ w ] <- lapply ( FemMaleFrame2 [ w ] , as.character )

### In this step I will remove the remaining SNP columns from the original "UX entry to Plante and SNP" object. Those remaining SNPs correspond to the chosen SNPs
### The "ColIndex" object includes the column indices that correspond to those SNPs. In this way, I will not have SNP column name duplicates in step that follows.
TempUXsnpOut <- TempUX [ , -c ( ColIndex ) ]

# unique ( TempUX [ , "UX_DASH_NUMBER" ] )

### Here I am creating a list with the dimensions of the reduced object so that the start column can be entered in loop that follows in the next section
NewColStarList [[ i ]] <- dim ( TempUXsnpOut ) [ 2 ] + 1 

### This is the final output list containing all SNP genotype information across all plates (lists)
FemMaleList [[ i ]] <- cbind ( TempUXsnpOut , FemMaleFrame2 )

print ( i )
}
proc.time() - ptm # Stop the clock

### End of loop:

########################################### End of section ##############################################################
###############################################################################################################################

###############################################################################################################
################################# Beginning of third section ##############################################



### Adding the "_F" and "_M" suffix to SNP IDs to latter consolidated with their corresponding genotype information
### Added a dot to the .F and .M so that they can be sorted before the H column in step below
AllSNPsWFemGenos <- paste ( SNPs1 , ".F" , sep = "_" )
AllSNPsWMaleGenos <- paste ( SNPs1 , ".M" , sep = "_" )
AllSNPsWhybrids <- paste ( SNPs1 , "H" , sep = "_" )

### Creating empty objects with the lengths of the SNP gender-suffix set created to consolidate them
### The nrow of the object corresponds to the UX summary object created above (no duplicates, consolidated summary)
EmptyFemObj <- as.data.frame ( matrix ( 0 , nrow = dim ( Consolidated1 ) [ 1 ] , ncol = length ( AllSNPsWFemGenos )  ) )
EmptyMaleObj <- as.data.frame ( matrix ( 0 , nrow = dim ( Consolidated1 ) [ 1 ] , ncol = length ( AllSNPsWMaleGenos )  ) )
EmptyHybridsObj <- as.data.frame ( matrix ( 0 , nrow = dim ( Consolidated1 ) [ 1 ] , ncol = length ( AllSNPsWMaleGenos )  ) )
EmptySNPsOnly <- as.data.frame ( matrix ( 0 , nrow = dim ( Consolidated1 ) [ 1 ] , ncol = length ( SNPs1 )  ) )

### Consolidation of objects
AllFemsMalesGenos <- cbind ( EmptyFemObj , EmptyMaleObj )
colnames ( AllFemsMalesGenos ) <- c ( AllSNPsWFemGenos , AllSNPsWMaleGenos )
AllFemsMalesHybridsGenos1 <- cbind ( AllFemsMalesGenos , EmptyHybridsObj )
colnames ( AllFemsMalesHybridsGenos1 ) <- c ( colnames ( AllFemsMalesGenos ) , AllSNPsWhybrids )
AllFemsMalesGenos1 <- cbind ( AllFemsMalesHybridsGenos1 , EmptySNPsOnly )
colnames ( AllFemsMalesGenos1 ) <- c ( colnames ( AllFemsMalesHybridsGenos1 ) , SNPs1 )

### Rearranging the column order to keep them consistent, SNP_ID, Female_SNP_ID, Male_SNP_ID and Hybrid_ID
AllFemsMalesGenos2 <- AllFemsMalesGenos1 [ , order ( names ( AllFemsMalesGenos1 ) ) ]
dim ( AllFemsMalesGenos2 )
# colnames ( AllFemsMalesGenos2 )


########################### Start of loop

### Now I consolidate ALL SNPs (all of them) with the UX summary consolidated file. So, I have a clean frame to populate
ConsolidatedFMSNPs <- cbind ( Consolidated1 , AllFemsMalesGenos2 )
# Consolidated1 [ 1 : 10 , ]
# dim ( Consolidated1 )
# colnames ( ConsolidatedFMSNPs )
length ( colnames ( ConsolidatedFMSNPs ) )
# ConsolidatedFMSNPs [ 1: 30 , 1 : 15 ]
dim ( ConsolidatedFMSNPs )

### This loop populates SNP AND GENOTYPE PARENTAL INFORMATION in the ConsolidatedFMSNPs object based on the ACTUAL SNPs selected for genotyping (as opposed to all possible polymorphic SNPs)
UX_DASH_NUMBER <- "UX_DASH_NUMBER"

### Note that in this case, I am multiplying by 4 because now I have 4 times as many SNP-related columns, including the gender-suffix columns 
SNPcolStart2 <- ( dim ( ConsolidatedFMSNPs ) [ 2 ] - ( length ( SNPs1 ) * 4 ) ) + 1
# str ( ConsolidatedFMSNPs )


ptm <- proc.time() # Start the clock!
UX_all2Sub2 <- list()
# k <- 91
#for ( i in 1 : 1 ) {
 for ( i in 1 : dim ( ConsolidatedFMSNPs ) [ 1 ] ) {
  for ( j in 1 : length ( FemMaleList ) ) {
    TempUX1 <- ConsolidatedFMSNPs [ i , UX_DASH_NUMBER ]
    UX_all2Sub2 [[ j ]] <- FemMaleList [[ j ]] [ FemMaleList [[ j ]] [ , UX_DASH_NUMBER ] %in% TempUX1 , ]
    if ( dim ( UX_all2Sub2 [[ j ]] ) [ 1 ] > 0 ) {
        for ( k in SNPcolStart2 : dim ( ConsolidatedFMSNPs ) [ 2 ] ) {
          if ( colnames ( ConsolidatedFMSNPs [ k ] ) %in% colnames ( UX_all2Sub2 [[ j ]] ) ) {
            if ( !grepl ( "_H" , colnames ( ConsolidatedFMSNPs [ k ] ) ) ) {
              if ( ( ConsolidatedFMSNPs [ i , k ] == 0 ) | is.na ( ConsolidatedFMSNPs [ i , k ] ) ) {
                ### NewColStarList object created above carries the column dimensions of each of the lists and can be used below for the sequence of "l" to define a start point
                ListSNPColStart2 <- NewColStarList [[ j ]]
                for ( l in ListSNPColStart2 : ( dim ( FemMaleList [[ j ]] ) [ 2 ]  ) ) {
                  if ( colnames ( ConsolidatedFMSNPs [ k ] )  ==  colnames ( UX_all2Sub2 [[ j ]] [ l ] ) ) {
                  ConsolidatedFMSNPs [ i , k ] = UX_all2Sub2 [[ j ]] [ 1 , l ]  
                  }}}}}}}}
print ( i )    
} 
proc.time() - ptm # Stop the clock

dim ( ConsolidatedFMSNPs )


########################### End of loop ###########################

########################################### End of section ##############################################################
#########################################################################################################################

#########################################################################################################################
####################### Fourth section: Consolidation of F1 genotype information ################################

########################### Start of loop

### First clean up objects because issues may arise.... 
ConsolidatedFMSNPs3 <- ConsolidatedFMSNPs [ !ConsolidatedFMSNPs [ , "UX_DASH_NUMBER" ] %in%  c ( NA ) , ]
# dim ( ConsolidatedFMSNPs3 )
# dim ( ConsolidatedFMSNPs )

ConsolidatedFMSNPs1 <- ConsolidatedFMSNPs3
dim ( ConsolidatedFMSNPs1 )
# colnames ( ConsolidatedFMSNPs1 ) == colnames ( RedHybData )
# colnames ( RedHybData )
# ConsolidatedFMSNPs1 [ 19 ]
# head ( ConsolidatedFMSNPs1 )
HybridData1 <- HybridData
# HybridData1 [ ( nrow ( HybridData1) - 10 ) : nrow ( HybridData1 ), ]
# HybridData [ ( nrow ( HybridData ) - 10 ) : nrow ( HybridData  ), ]
# tail ( HybridData1 )



SNPsH <- colnames ( HybridData1 [ , 2 : dim ( HybridData1 ) [ 2 ] ] ) 
SNPsH2 <- colnames ( HybridData1 ) 
UX_IDs <- SNPsH2 [ 1 ]
HybCols1 <- paste ( SNPsH , "H" , sep = "_" )
colnames ( HybridData1 ) <- c ( UX_IDs , HybCols1  )
UX_DASH_NUMBER <- "UX_DASH_NUMBER"
SNPcolStart2 <- ( dim ( ConsolidatedFMSNPs1 ) [ 2 ] - ( length ( SNPs1 ) * 4 ) ) + 1
HybDataStartCol <- length ( UX_IDs ) + 1

# k <- 11
ptm <- proc.time() # Start the clock!
 for ( i in 1 : dim ( ConsolidatedFMSNPs1 ) [ 1 ] ) {
#  for ( i in 1 : 1 ) {
 TempHyb <- ConsolidatedFMSNPs1 [ i , UX_DASH_NUMBER ]
 RedHybData <- HybridData1 [ HybridData1 [ , UX_IDs ] %in% TempHyb , ] 
 if ( dim ( RedHybData ) [ 1 ] > 0 ) {
  for ( k in SNPcolStart2 : dim ( ConsolidatedFMSNPs1 ) [ 2 ] ) {
    if ( grepl ( "_H" , colnames ( ConsolidatedFMSNPs1 [ k ] ) )  ) {
      for ( l in HybDataStartCol :  dim ( RedHybData ) [ 2 ] ) {
        if ( !is.na ( RedHybData [ 1 , l ] ) ) {
          if( colnames ( ConsolidatedFMSNPs1 [ k ] ) == colnames ( RedHybData [ l ] )  ) {
            ConsolidatedFMSNPs1 [ i , k ] = RedHybData [ 1 , l ]
          }}}}}}
print ( i )    
}
proc.time() - ptm 

# ConsolidatedFMSNPs1 [ 1 : 100 , 19 ]
# colnames ( ConsolidatedFMSNPs1 [ 11 ] )


nucleotideCalls <- c ( "A" , "C" , "G" , "T" )
collectHetParents <- matrix ( 0 , nrow = nrow ( ConsolidatedFMSNPs1 ) , ncol = 1 )
collectHetParentsAndHybCalls <- matrix ( 0 , nrow = nrow ( ConsolidatedFMSNPs1 ) , ncol = 1 )
# k <- 9
for ( k in SNPcolStart2 : ( dim ( ConsolidatedFMSNPs1 ) [ 2 ] ) - 2 ) {
  temp <- NULL
  step1 <- NULL
  if ( ( grepl ( "_.F" , colnames ( ConsolidatedFMSNPs1 [ k ] ) ) ) |  ( grepl ( "_.M" , colnames ( ConsolidatedFMSNPs1 [ k + 1 ] ) ) ) ) {
    step1 <- data.frame ( do.call ( 'rbind' , strsplit ( as.character ( ConsolidatedFMSNPs1 [ , k ] ) , ':' , fixed = TRUE ) ) , stringsAsFactors = T )
    step2 <- data.frame ( do.call ( 'rbind' , strsplit ( as.character ( ConsolidatedFMSNPs1 [ , k + 1 ] ) , ':' , fixed = TRUE ) ) , stringsAsFactors = T )
    ## Added on 09.11.2020 to count HETEROGENEOUS HYB CALL from true HETEROGENEOUS SNPs
    step3 <- data.frame ( do.call ( 'rbind' , strsplit ( as.character ( ConsolidatedFMSNPs1 [ , k + 2 ] ) , ':' , fixed = TRUE ) ) , stringsAsFactors = T )
    # dim ( step1 )
      if ( dim ( step1 ) [ 2 ] > 1 & dim ( step2 ) [ 2 ] > 1 ) {
           xx <- sapply ( step1 , is.factor )
           step1 [ xx ] <- lapply ( step1 [ xx ] , as.character )
          # str ( step1 )
           xx2 <- sapply ( step2 , is.factor )
           step2 [ xx2 ] <- lapply ( step2 [ xx2 ] , as.character )
          # str ( step2 )
        ## (11.12.18) for some reason when running script on mac, it required the two additional below steps activated, whereas before they were disabled
          #step1$X1 <- factor ( step1$X1 , unique ( c ( levels ( step1$X1 ) ,  levels ( step1$X2 ) ) ) )
          #step1$X2 <- factor ( step1$X2 , unique ( c ( levels ( step1$X1 ) ,  levels ( step1$X2 ) ) ) )
      temp <- { as.data.frame ( ifelse ( ( step1 [ , 1 ] %in% nucleotideCalls  & step1 [ , 2 ] %in% nucleotideCalls  
              & step2 [ , 1 ] %in% nucleotideCalls  & step2 [ , 2 ] %in% nucleotideCalls )
              & ( step1 [ 1 ] != step1 [ 2 ] & step2 [ 1 ] == step2 [ 2 ] |  step1 [ 1 ] == step1 [ 2 ] & step2 [ 1 ] != step2 [ 2 ] )
              , yes = T , no = F ) )
              }
      collectHetParents <- cbind.data.frame ( collectHetParents , temp )
      }
    ### If step below modified on 01.22.21 because NA columns were not splitting into two columns and an error was halting the completion of the loop. 
    ### Original step only had the step3 (hybrid) object argument in it. Now it contains both parents too. 
    if ( dim ( step1 ) [ 2 ] > 1 & dim ( step2 ) [ 2 ] > 1 & dim ( step3 ) [ 2 ] > 1 ) {
      xx <- sapply ( step3 , is.factor )
      step3 [ xx ] <- lapply ( step3 [ xx ] , as.character )
      tempHeterogenHyb <- { as.data.frame ( ifelse ( ( step1 [ , 1 ] %in% nucleotideCalls  & step1 [ , 2 ] %in% nucleotideCalls  
                                           & step2 [ , 1 ] %in% nucleotideCalls  & step2 [ , 2 ] %in% nucleotideCalls
                                           & step3 [ , 1 ] %in% nucleotideCalls  & step3 [ , 2 ] %in% nucleotideCalls )
                                         & ( step1 [ 1 ] != step1 [ 2 ] & step2 [ 1 ] == step2 [ 2 ] |  step1 [ 1 ] == step1 [ 2 ] & step2 [ 1 ] != step2 [ 2 ] )
                                         & (  step3 [ 1 ] != step3 [ 2 ] )
                                         , yes = T , no = F ) )
        }
      collectHetParentsAndHybCalls <- cbind.data.frame ( collectHetParentsAndHybCalls , tempHeterogenHyb )
      }
    }
print ( k )    
}

# dim ( collectHetParents )
# collectHetParents [ 1 : 10 , ]
# ConsolidatedFMSNPs1 [ , 9 : 10 ]
# colnames ( ConsolidatedFMSNPs1 )

collectHetParents1 <- as.data.frame ( rowSums ( collectHetParents ) ) 
collectHetParentsAndHybCalls1 <- as.data.frame ( rowSums ( collectHetParentsAndHybCalls ) ) 


collectHetHybs <- matrix ( 0 , nrow = nrow ( ConsolidatedFMSNPs1 ) , ncol = 1 )
# k <- 11
for ( k in SNPcolStart2 : dim ( ConsolidatedFMSNPs1 ) [ 2 ] ) {
  temp <- NULL
  step1 <- NULL
  if ( grepl ( "_H" , colnames ( ConsolidatedFMSNPs1 [ k ] ) ) ) {
    step1 <- data.frame ( do.call ( 'rbind' , strsplit ( as.character ( ConsolidatedFMSNPs1 [ , k ] ) , ':' , fixed = TRUE ) ) )
    # dim ( step1 )
    # print ( dim ( step1 ) [ 2 ] )
    if ( dim ( step1 ) [ 2 ] > 1 ) {
       xx <- sapply ( step1 , is.factor )
       step1 [ xx ] <- lapply ( step1 [ xx ] , as.character )
      ## (11.12.18) for some reason when running script on mac, it required the two additional below steps activated, whereas before they were disabled
      #  step1$X2 <- factor ( step1$X2 , unique ( c ( levels ( step1$X1 ) ,  levels ( step1$X2 ) ) ) )
        temp <- as.data.frame ( ifelse ( ( step1 [ , 1 ] %in% nucleotideCalls  & step1 [ , 2 ] %in% nucleotideCalls ) & step1 [ 1 ] != step1 [ 2 ] , yes = T , no = F ) )
        collectHetHybs <- cbind.data.frame ( collectHetHybs , temp )
    }}
print ( k )      
}
# colnames ( ConsolidatedFMSNPs1 )
# dim ( collectHetHybs )
# collectHetHybs [ 1 : 10 , ]
# ConsolidatedFMSNPs1 [ , 11 ]
# str ( step1 )

collectHetHybs1 <- as.data.frame ( rowSums ( collectHetHybs ) ) 
# head ( collectHetHybs1 )
# str ( ConsolidatedFMSNPs1 )

collectSelfs <- matrix ( 0 , nrow = nrow ( ConsolidatedFMSNPs1 ) , ncol = 1 )
collectColNamesHere00 <- NULL
# k <- 15
for ( k in SNPcolStart2 : dim ( ConsolidatedFMSNPs1 ) [ 2 ] ) {
  temp <- NULL
  step1 <- NULL
  if ( grepl ( "_H" , colnames ( ConsolidatedFMSNPs1 [ k ] ) ) ) {
    step1 <- data.frame ( do.call ( 'rbind' , strsplit ( as.character ( ConsolidatedFMSNPs1 [ , k ] ) , ':' , fixed = TRUE ) ) )
    # dim ( step1 )
    if ( dim ( step1 ) [ 2 ] > 1 ) {
      ## (11.12.18) for some reason when running script on mac, it required the two additional below steps activated, whereas before they were disabled
      ## 10.15.23 on this date, using the borrowed macbook from IT and R 4.2 with a bunch of gibberish installed, I had to disable the lines below in order to avoid having the output set as NAs. 
      ## On 4.28.25, on new macbook need to enable lines to convert factors to characters
       # step1$X1 <- factor ( step1$X1 , unique ( c ( levels ( step1$X1 ) ,  levels ( step1$X2 ) ) ) )
       # step1$X2 <- factor ( step1$X2 , unique ( c ( levels ( step1$X1 ) ,  levels ( step1$X2 ) ) ) )
      temp <- as.data.frame ( ifelse ( ( step1 [ , 1 ] %in% nucleotideCalls  & step1 [ , 2 ] %in% nucleotideCalls ) & step1 [ 1 ] == step1 [ 2 ] , yes = T , no = F ) )
      collectSelfs <- cbind.data.frame ( collectSelfs , temp , stringsAsFactors = F )
      
      collectColNamesHere00 <- rbind.data.frame ( collectColNamesHere00 , colnames ( ConsolidatedFMSNPs1 [ k ] ) )
      yy <- sapply ( collectColNamesHere00 , is.factor )
      collectColNamesHere00 [ yy ] <- lapply ( collectColNamesHere00 [ yy ] , as.character )
    }}
  print ( k )      
}
# collectSelfs
# colnames ( ConsolidatedFMSNPs1 [ k ] ) 
# colnames ( ConsolidatedFMSNPs1 )
# str ( collectColNamesHere00 )
# str ( collectSelfs )
# class ( collectSelfs )
# dim ( collectSelfs )
# collectSelfs [ 1 : 10 , ]
# ConsolidatedFMSNPs1 [ 1 : 2 , ]
# str ( step1 )
# str ( ConsolidatedFMSNPs1 )
# str ( collectColNamesHere00 )
# dim ( collectColNamesHere00 )

collectSelfs1 <- as.data.frame ( rowSums ( collectSelfs ) ) 
# head ( collectSelfs1 )

# colnames ( collectSelfs ) <- c ( "collectSelfs" , as.vector ( collectColNamesHere00 ) )
# collectSelfs

# write.csv ( as.vector ( collectColNamesHere00 ) , "temp.csv" , row.names = F )

genoCalls <- c ( "A:A" , "C:C" , "G:G" , "T:T" )
collectTrueHomPoly <- matrix ( 0 , nrow = nrow ( ConsolidatedFMSNPs1 ) , ncol = 1 )
# k <- 9
for ( k in SNPcolStart2 : ( dim ( ConsolidatedFMSNPs1 ) [ 2 ] ) - 2 ) {
  temp <- NULL
  if ( grepl ( "_.F" , colnames ( ConsolidatedFMSNPs1 [ k ] ) ) & grepl ( "_.M" , colnames ( ConsolidatedFMSNPs1 [ k + 1 ] ) ) ) {
      temp <- {   as.data.frame ( ifelse ( ( ( ConsolidatedFMSNPs1 [ , k ] %in% genoCalls ) 
                  & ( ConsolidatedFMSNPs1 [ , k + 1 ] %in% genoCalls ) ) 
                  & ( ConsolidatedFMSNPs1 [ k ] != ConsolidatedFMSNPs1 [ k + 1 ] )
                  , yes = T , no = F ) ) 
              }
      collectTrueHomPoly <- cbind.data.frame ( collectTrueHomPoly , temp )
   # }
      }
  print ( k )      
}
# head ( ConsolidatedFMSNPs1 [ , 9 : 10 ] )
# dim ( collectTrueHomPoly )
# head ( collectTrueHomPoly )
# dim ( temp )
# head ( temp )

collectTrueHomPol1 <- as.data.frame ( rowSums ( collectTrueHomPoly ) ) 
# head ( collectTrueHomPol1 )
# dim ( collectHetHybs )
# dim ( ConsolidatedFMSNPs1 )
# dim ( collectTrueHomPoly )
# tempdf <- colnames ( collectTrueHomPoly )
# tempdf2 <- do.call ( 'rbind' , strsplit ( as.character ( tempdf ) , '_.F' , fixed = TRUE ) ) 
# tempdf2 %in% colnames ( HybridData )
# tempdf3 <- tempdf2 [ !tempdf2 %in% colnames ( HybridData ) ]

HetHybCallsfromTruePolySNPs <- matrix ( 0 , nrow = nrow ( collectTrueHomPoly ) , ncol = 1 )
selfCallsfromTruePolySNPs <- matrix ( 0 , nrow = nrow ( collectTrueHomPoly ) , ncol = 1 )
for ( k in 1 : dim ( collectTrueHomPoly ) [ 2 ]  ) {
  temp1 <- NULL
  temp2 <- NULL
    temp1 <- ifelse ( collectTrueHomPoly [ k ] == T & collectHetHybs [ k ] == T , yes = T , no = F )
    temp2 <- ifelse ( collectTrueHomPoly [ k ] == T & collectSelfs [ k ] == T , yes = T , no = F )
    HetHybCallsfromTruePolySNPs <- cbind.data.frame ( HetHybCallsfromTruePolySNPs , temp1 )
    selfCallsfromTruePolySNPs <- cbind.data.frame ( selfCallsfromTruePolySNPs , temp2 )
    print ( k )      
}

# dim ( HetHybCallsfromTruePolySNPs )
# dim ( selfCallsfromTruePolySNPs )
# head ( HetHybCallsfromTruePolySNPs )
# head ( selfCallsfromTruePolySNPs )
HetHybCallsfromTruePolySNPs1 <- as.data.frame ( rowSums ( HetHybCallsfromTruePolySNPs ) ) 
selfCallsfromTruePolySNPs1 <- as.data.frame ( rowSums ( selfCallsfromTruePolySNPs ) ) 

# HetCallsfromHETPolSNPs <- as.data.frame ( collectHetHybs1 - HetHybCallsfromTruePolySNPs1 )
HetCallsfromHETPolSNPs <- as.data.frame ( collectHetParentsAndHybCalls1 )
probTrueHetHybfromHomPolySNPs <- as.data.frame ( 1 - ( freqHetSeedInF4_8^HetHybCallsfromTruePolySNPs1 ) )
probTrueHetHybfromHETPolySNPs <- as.data.frame ( 1 - ( freqNonHetPlantInF4_7OneHomClass^HetCallsfromHETPolSNPs ) )





ConsolidatedFMSNPs2 <-  { cbind.data.frame  ( ConsolidatedFMSNPs1 , collectTrueHomPol1 , collectHetParents1 , collectHetHybs1 
                        , HetHybCallsfromTruePolySNPs1 , HetCallsfromHETPolSNPs , collectSelfs1 , selfCallsfromTruePolySNPs1 
                        , probTrueHetHybfromHomPolySNPs , probTrueHetHybfromHETPolySNPs , stringsAsFactors = F )
                        }

# dim ( ConsolidatedFMSNPs2 )
# dim ( ConsolidatedFMSNPs1 )
# ConsolidatedFMSNPs2 [ 1 : 5 , ]
colnames ( ConsolidatedFMSNPs2 ) <- { c ( colnames ( ConsolidatedFMSNPs1 ) , "sumTrueHOMpolySNPs" , "sumHeterogeneousPolySNPs" 
                                    , "sumTotalHetHybCalls" , "HetHybCallsfromTruePolySNPs" , "HetCallsfromHETEROGENEOUSPolySNPs" 
                                    , "sumTotalSelfCalls" , "selfCallsfromTruePolySNPs" , "probTrueHetHybfromHomPolySNPs" 
                                    , "probTrueHetHybfromHETEROGENEOUSPolySNPs" )
                                    }


ConsolidatedFMSNPs2$Score <-  { ifelse ( ConsolidatedFMSNPs2$HetHybCallsfromTruePolySNPs >= 3 
                                | ConsolidatedFMSNPs2$HetCallsfromHETEROGENEOUSPolySNPs > 6 
                                | ( ConsolidatedFMSNPs2$HetHybCallsfromTruePolySNPs + ConsolidatedFMSNPs2$HetCallsfromHETEROGENEOUSPolySNPs ) > 5
                                , "H" , "" )
                              }
# ConsolidatedFMSNPs2 [ 1 : 5 , ]

#### Export results file and save an R object with 
# getwd()

# write.csv ( ConsolidatedFMSNPs2 , file = paste ( FileToExportResults , Type_of_Crosses , ".csv" , sep = "" ) , row.names = F , na = "NA" )
# saveRDS ( ConsolidatedFMSNPs2 , file = paste ( FileToExportResults , Type_of_Crosses , ".RDS" , sep = "" ) )


########################################### End of fourth section ##############################################################
#########################################################################################################################


### APPENDIX

### Notes on how to interpret the output file generated above:
### Example of output file and legend to understand the information and labels

# FEMALE	      MALE	UX_DASH_NUMBER	MATERIAL            	STATUS SNPsum	  BARC.018175.02534	BARC.018175.02534_F	BARC.018175.02534_M	BARC.020505.04644	BARC.020505.04644_F	BARC.020505.04644_M	BARC.031343.07057	BARC.031343.07057_F	BARC.031343.07057_M
# Parent_1	Parent_2	UX3358-2	SCN Yield Crosses 2016	Active	5	      BARC.018175.02534	        T:T	                 C:C				                                                                  #N/A	            A:A	                A:A
# Parent_1	Parent_2	UX3358-6	SCN Yield Crosses 2016	Active	5	      BARC.018175.02534	        T:T	                 C:C				                                                                  #N/A	            A:A	                A:A
# Parent_1	Parent_2	UX3358-1	SCN Yield Crosses 2016	Active	5	      BARC.018175.02534	        T:T	                 C:C				                                                                  #N/A	            A:A	                A:A
# Parent_1	Parent_2	UX3358-3	SCN Yield Crosses 2016	Active	5	      BARC.018175.02534	        T:T	                 C:C				                                                                  #N/A	            A:A	                A:A
# Parent_1	Parent_2	UX3358-4	SCN Yield Crosses 2016	Active	5	      BARC.018175.02534	        T:T	                 C:C				                                                                  #N/A	            A:A	                A:A
# Parent_1	Parent_2	UX3358-5	SCN Yield Crosses 2016	Active	5     	BARC.018175.02534	        T:T	                 C:C				                                                                  #N/A	            A:A	                A:A
# Parent_1	Parent_3	UX3359-3	SCN Yield Crosses 2016	Active	1	            #N/A	              T:T	                 T:T	      BARC-020505-04644_H	      G:G	                  G:A	              #N/A	            A:A	                 ?
# Parent_1	Parent_3	UX3359-4	SCN Yield Crosses 2016	Active	1	            #N/A	              T:T	                 T:T	      BARC-020505-04644_H	      G:G	                  G:A	              #N/A	            A:A	                 ?
# Parent_1	Parent_3	UX3359-2	SCN Yield Crosses 2016	Active	1     	      #N/A	              T:T	                 T:T	      BARC-020505-04644_H	      G:G	                  G:A	              #N/A	            A:A	                 ?
# Parent_1	Parent_3	UX3359-5	SCN Yield Crosses 2016	Active	1	            #N/A	              T:T	                 T:T	      BARC-020505-04644_H	      G:G	                  G:A	              #N/A	            A:A	                 ?
# Parent_1	Parent_3	UX3359-6	SCN Yield Crosses 2016	Active	1	            #N/A	              T:T	                 T:T	      BARC-020505-04644_H	      G:G	                  G:A	              #N/A	            A:A	                 ?


### The SNP information is presented in groups of three columns, the first column has the SNP ID (above is column 7 from left to right), the second column has the female parental genotype for the SNP and the third column has the male parental genotype informtaion.
### First SNP column ( columns 7 above ): If there is a SNP ID in a SNP-row coordinate, then, it means the SNP is polymorphic between the two parents as shown by the genotypes.
### Second SNP column for FEMALE genotype: Contains the genotype of the corresponding female
### Third SNP column for MALE genotype: Contains the genotype of the corresponding male
### A fourth column would contain the Hybrid genotype for the F1 offspring

## #N/A: Whenever an "#N/A" value is displayed in the first column for the SNP ID, it means that the SNP is not polymorphic for the parents in question BUT the samples were genotyped with that SNP because they were
## part of a plate that consolidated different combinations of SNPs and the polymorphic SNPs could not target all samples in plate. So, in a sense, it could be regarded as a Quality Check control.

## Also, note that SNP IDs in the First column may contain sufixes such as "BARC-020505-04644_H", where the H at the end means that one of the parents was heterogeneous for that SNP as shown for the genoypes of 
## SNP BARC.020505.04644_M above. This may not be the case for most files.

