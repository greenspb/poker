#########################
#Function: 	deal
# Description:	generate Player+Community cards = 2x(nPlayers)+5
# Inputs:		nPlayers : number of hands to deal 
												#integer in {1, 2, ... , 8}
#				position : dealer position 	#integer in {1, 2, ..., nPlayers}
# Locals:		nCards : number of cards dealt to players
												#integer in {4, 6, 8, ... 16}
# Outputs:		y : cards dealt in hole			#vector[nCards] in {1, 2, ..., 52}
deal <- function(nPlayers, position) {
	nCards <- 2*nPlayers+5
	y <- numeric(nCards)
	y <- sample(1:52, nCards, replace=FALSE, prob=rep(1/52,52))
	y
	#rm(nCards)
}
