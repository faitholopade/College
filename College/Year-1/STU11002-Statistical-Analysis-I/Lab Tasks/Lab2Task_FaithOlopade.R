#Code for Multiple Monte Carlo Games

#declaring and initializing count variables to 0
n_stay <- 0
n_switch <- 0
n_random <- 0

#for loop to play game 100 times
for (i in 1:100) {
  #i represents play of each game and this instruction will be
  #carried out for play 1 up to and including play 100
  door <-
    c(1, 2, 3) #vector door created and initialized with number 1, 2 and 3
  #to represent the numbers of doors in the game
  cardoor <-
    sample(door, 1) #creates a variable cardoor and randomly assigns it
  #one of the numbers in the vector door, this represents the door that the car
  # is behind
  choice <-
    sample(door, 1) #creates a variable choice and assigns it one random
  #number from door vector, this represents the players selection of door 1,2 or3
  goatdoors <-
    setdiff(door, cardoor) #creates vector goatdoors which holds
  #numbers that correspond to doors with goats behind them, the setdiff operation
  #does this by selecting the numbers that were not selected and placed in cardoor
  #from door vector
  reveal_options <-
    setdiff(goatdoors, choice) #creates a vector that will
  #contain the remaining reveal options each run of the game, setdiff again
  #selects from goatdoors vector numbers that were not assigned to choice
  if (choice == cardoor) {
    #checking if players choice was the winning door
    reveal <-
      sample(reveal_options, 1)  #if it was then reveal_options will
    #contain two goats (not literally:)) and we assign reveal one of them
  } else {
    #in any other scenario (i.e, the player has not won) reveal is set to
    #one of the values(doors) in the reveal_options vector
    reveal <- reveal_options
  }
  remaining_doors <-
    setdiff(door, reveal) #creates vector that contains two
  #unrevealed doors, setdiff does this by selecting numbers from door that were
  #not assigned to reveal
  newchoice <-
    setdiff(remaining_doors, choice) # newchoice will contain
  #number from remaining_doors that has not been assigned to choice
  #i.e. recording whether player has selected to switch or stay
  randomchoice <-
    sample(remaining_doors, 1) #randomly choosing to switch or
  #stay by choosing door from remaining doors
  
  
  if (choice == cardoor) {
    #checking to see if original(stay) choice was the
    #door with car behind it
    n_stay <- n_stay + 1 #if it is equal add one to stay counter
  }
  
  
  if (newchoice == cardoor) {
    #checking to see if new(switch) choice was the
    #door with car behind it
    n_switch <-
      n_switch + 1 #if it is equal add one to switch counter
  }
  if (randomchoice == cardoor) {
    #checking to see if random(switch/stay) choice
    #was the door with car behind it
    n_random <-
      n_random + 1 #if it is equal add one to random counter
  }
}
#printing count of successful stay, switch or random. dividing by 100 shows
# the figures for each play of the game
print(n_stay / 100)
print(n_switch / 100)
print(n_random / 100)

