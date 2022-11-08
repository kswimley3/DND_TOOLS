# Loot table generator
library(readODS)
library(tidyverse)

# Read in files ======================================
indiv.df <- read_ods("LootTables.ods", sheet = 1)
hoard.df <- read_ods("LootTables.ods", sheet = 2)
gems.df <- read_ods("LootTables.ods", sheet = 3)
art.df <- read_ods("LootTables.ods", sheet = 4)
artificer.df <- read_ods("LootTables.ods", sheet = 5)

# Parameters ========================================
loot.type <- 1 # Enter 1 if individual, 2 if hoard
die_roll <- as.numeric(sample(x = 1:20, size = 1, replace = FALSE)) # Player roll
chal <- 0 # enter challenge rating

# Functions =========================================
roll <- function(die = 20){
  # roll a die with (die) sides, default is 20
  roll <- as.numeric(sample(x = 1:die, size = 1, replace = FALSE))
  return(roll)
}

get_loot <- function(challenge, die_roll){
  # returns individual treasure from an encounter
  loot <- indiv.df$Reward[die_roll]
  mult <- indiv.df$Multiplier[die_roll]
  loot.string <- paste("Loot: ", loot, " times ", mult)
  print(loot.string)
  return(loot.string)
}

get_hoard <- function(chal){
  # returns hoard of loot based on a d100 roll
  if(chal < 5){
    # get table row from first roll
    die_roll <- roll(100)
    type <- hoard.df$Gems_Art[die_roll]
    GP <- hoard.df$GP[die_roll]
    DieNum <- hoard.df$DieNum[die_roll]
    DieType <- hoard.df$DieType[die_roll]
    MagItemDieNum <- hoard.df$MagicItemDieNum[die_roll]
    MagItemDieType <- hoard.df$MagicItemDieType[die_roll]
    MagItemsTable <- hoard.df$MagicItemsTable[die_roll]
    CraftGP <- hoard.df$ArtificerLootGP[die_roll]
    CraftDieType <- hoard.df$ArtificerDieType[die_roll]
    CraftDieNum <- hoard.df$ArtificerDieNum[1]
    cp <- ((c(1, 1, 1, 1, 1) * roll(6)) %>% sum())*100
    sp <- (c(1, 1, 1) * roll(6)) %>% sum() * 100
    gp <- (c(1, 1) * roll(6)) %>% sum() * 100
  }
  
  # roll the amount of loot items
  loot_num <- 0
  for(i in 1:DieNum){
    loot_num <- loot_num + roll(DieType)
  }
  
  # add money strings to loot vector
  loot.vector <- c(paste("CP: ", cp), paste("SP: ",sp), paste("GP: ", gp)) 
  
  # if type is art, add all art items
  if(type == "Art"){
    # Art generation
    # TODO: add more conditional GP Art values to this
    for(i in 1:loot_num){
      loot.temp.index <- roll(10)
      loot.vector <- append(loot.vector, art.df$Name[loot.temp.index])
    }
  } 
  
  if(type == "Gems"){
    # Gem generation
    if(GP == 10){
      for(i in 1:loot_num){
        loot.temp.index <- roll(7)
        loot.vector <- append(loot.vector, gems.df$Gemstones[loot.temp.index])
      }
    } else if(GP == 100){
      for(i in 1:loot_num){
        loot.temp.index <- roll(7) + 7
        loot.vector <- append(loot.vector, gems.df$Gemstones[loot.temp.index])
      }
    }
    
  } 
  
  if(type == "None"){
    loot.vector <- append(loot.vector, "No gems or art")
  }
  
  # generate crafting materials
  craft.vector <- c()
  CraftLootNum <- 0
  for(i in 1:CraftDieNum){
    CraftLootNum <- CraftLootNum + roll(CraftDieType)
  }
  
  
  if(CraftGP == 10){
    for(i in 1:CraftLootNum){
      craft.temp.index <- roll(12)
      craft.vector <- append(craft.vector, artificer.df$Name[craft.temp.index])
    }
  }
  
  if(CraftGP == 25){
    for(i in 1:CraftLootNum){
      craft.temp.index <- roll(12) + 12
      craft.vector <- append(craft.vector, artificer.df$Name[craft.temp.index])
    }
  }
  
  # print die rolls and table number for magic items, done in person
  MagItemChar <- paste("Roll", MagItemDieNum, " d", MagItemDieType, "on Magic Item Table", MagItemsTable)
  
  loot.vector <- append(loot.vector, craft.vector)
  loot.vector <- append(loot.vector, MagItemChar)
  return(loot.vector)
}
  
  
# Main ==========
loot.string <- get_loot(chal, die_roll)
loot.list <- get_hoard(chal)

# Output ===========
if(exists("loot.string")){
  print(loot.string)
}

if(exists("loot.list")){
  for(i in loot.list){
    print(i)
  }
}
