# Combat rating calculator

# Party Levels
party <- c(1, 1, 1, 1, 1) # Enter party levels here
names(party) <- c("Gibby", "Eric", "Elise", "Gauge", "Kaleb")
monster_xp <- c(50, 50, 75) # Monster encounter, enter xp points for each monster here

# MAIN ================

# XP threshold Table
char_lvl <- c(1:20)
easy <- c(25, 50, 75, 125, 250, 300, 350, 450, 550,
          600, 800, 1000, 1100, 1250, 1400, 1600, 2000,
          2100, 2400, 2800)
medium <- easy*2
hard <- easy*3
deadly <- easy*4
threshold.df <- data.frame(char_lvl, easy, medium, hard, deadly)
dif <- c(2:5) 

party_xp <- c()
for(i in dif){
  party_xp <- append(party_xp, sum(party*threshold.df[party,i]))
}
names(party_xp) <- c("Easy", "Medium", "Hard", "Deadly")

# encounter multipliers
mult.table <- data.frame(monster_num = c(1:20), 
                         mult = c(1, 1.5, 2, 2, 2, 2, 2.5, 2.5, 2.5, 2.5, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4))
multiplier <- mult.table[length(monster_xp),2]
monster_xp_tot <- sum(monster_xp)*multiplier # add any modifiers to monster xp here

# print end results =================
print("Party Thresholds:")
party_xp

print("Monster Adjusted XP value:")
monster_xp_tot


