#####################
#'Function: 	testRoundOfPoker
#'
#'Author: Ben Greenspan		
#'Created: December 19, 2010
#'Description:	run a test round of poker
#'Inputs:		@param alias : names of players
#'												#vector of strings
#'												#length of vector must be less than or equal to 9
#'				@param position : dealer position 		#integer in {1, 2, ..., nPlayers}
#'				@param holeCards : the hand of player 1
#'Locals:		nPlayers : number of hands to deal 
#'												#integer in {1, 2, ..., nPlayers}
#'				y : cards dealt in hole			#vector[nCards] in {1, 2, ..., 52}
#'				players :	the hole cards in absolute position
#'												#matrix[nPlayers, 4]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'				board :	the board cards			#vector[5] in {1, 2, ..., 52}
#'				cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Outputs:		@return textual summary that includes scores of each hand, as well as a graphical display of the round
#'@export
testRoundOfPoker <- function() {
	alias <- c("Player 1", "Player 2","Player 3", "Player 4", "Player 5", "Player 6", "Player 7", "Player 8", "Player 9")
	nPlayers <- length(alias)
	position <- nPlayers

	y <- deal(nPlayers, position)
	players <- assignToPlayers(nPlayers, position, y)
	board <- assignToBoard(y)
	cards <- hand(players, board)
	score <- showdown(cards)
	winner <- tiebreaker(nPlayers,cards,score)

	round <- 1
	CGIplayers(round, alias, position, cards, score)
	round <- 2
	CGIplayers(round, alias, position, cards, score)
	round <- 3
	CGIplayers(round, alias, position, cards, score)
	round <- 4
	CGIplayers(round, alias, position, cards, score)

	cat("Please look at the graphics window to see the current hand.\n\nThe score of each player is one of the following:\n\t9 = Straight Flush\n\t8 = Four of a Kind\n\t7 = Full House\n\t6 = Flush\n\t5 = Straight\n\t4 = Three of a Kind\n\t3 = Two Pair\n\t2 = Pair\n\t1 = Highcard\nThe scores for this hand are:\n\tPlayer 1: ",score[1],"\n\tPlayer 2: ",score[2],"\n\tPlayer 3: ",score[3],"\n\tPlayer 4: ",score[4],"\n\tPlayer 5: ",score[5],"\n\tPlayer 6: ",score[6],"\n\tPlayer 7: ",score[7],"\n\tPlayer 8: ",score[8],"\n\tPlayer 9: ",score[9],".\nThe winners of this hand are Players",winner,".\n\nThank you for playing.")
}


#'Function: 	deal
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	generate Player+Community cards = 2x(nPlayers)+5
#'Inputs:		@param nPlayers : number of hands to deal 
#'												#integer in {1, 2, ... , 8}
#'				@param position : dealer position 	#integer in {1, 2, ..., nPlayers}
#'Locals:		nCards : number of cards dealt to players
#'												#integer in {4, 6, 8, ... 16}
#'Outputs:		@return y : cards dealt in hole			#vector[nCards] in {1, 2, ..., 52}
#'@export
deal <- function(nPlayers, position) {
	nCards <- 2*nPlayers+5
	y <- numeric(nCards)
				#create a vector for the cards
	y <- sample(1:52, nCards, replace=FALSE, prob=rep(1/52,52))
				#deal numbers in {1, 2, ... , 52}
	y
}


#'Function: 	dotTestDealer
#'
#'Author: Ben Greenspan		
#'Created: December 19, 2010
#'Description:	assume player 1 already has cards.  for remaining players, 
#'				generate Player+Community cards = 2x(nPlayers-1)+5
#'Inputs:		@param nPlayers : number of hands to deal 
#'												#integer in {1, 2, ... , 8}
#'				@param position : dealer position 	#integer in {1, 2, ..., nPlayers}
#'				@param holeCards : the hand of player 1
#'Locals:		nCards : number of cards dealt to players
#'												#integer in {4, 6, 8, ... 16}
#'Outputs:		@return y : cards dealt in hole			#vector[nCards] in {1, 2, ..., 52}
#'@export
dotTestDealer <- function(nPlayers, position,holeCards) {
	nCards <- 2*nPlayers+5
	y <- numeric(nCards)
				#create a vector for the cards
	y[c(1,nPlayers+1)] <- holeCards
				#the cards for player 1
	y[-c(1,nPlayers+1)] <- sample(subset(1:52, 1:52 != holeCards), nCards-2, replace=FALSE, prob=rep(1/50,50))
				#deal numbers in {1, 2, ... , 52}
	y
}



#'Function: 	assignToPlayers
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	standard normal deal: begin the deal starting at the left of the dealer.
#'Inputs:		@param nPlayers : number of hands to deal 
#'												#integer in {1, 2, ... , 8}
#'				position : dealer position 	#integer in {1, 2, ..., nPlayers}
#'				@param y : cards dealt					#vector[2*nPlayers+5] in {1, 2, ..., 52}
#'Locals:		i : for name in vector
#'												#numeric in vector
#'				k : for name in vector
#'												#numeric in vector
#'Outputs:		@return players :	the hole cards in absolute position
#'												#matrix[nPlayers, 4]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'@export
assignToPlayers <- function(nPlayers, position, y) {
	players <- matrix(0, nPlayers, 2) 
				#create a vector for the hole cards
	for (i in 1:2) {
		for (k in 1:nPlayers) {	
			players[transformToAbsolute(nPlayers,position,k), i] <- y[(i-1)*nPlayers+k]
				#(i-1)*nPlayers+k : number of cards dealt at this time
												#integer in {1, 2, ..., 2*nPlayers}
			}
		}
	players
}



#'Function: 	assignToBoard
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	deal 3 community cards 
#'Inputs:		@param y : cards dealt					#vector[2*nPlayers+3] in {1, 2, ..., 52}
#'Outputs:		@return board :	the board cards			#vector[5] in {1, 2, ..., 52}
#'@export
assignToBoard <- function(y) {
	board <- numeric(5)
				#create a vector for the board cards
	board <- y[(length(y)-4):length(y)]
				#assign the board cards
	board
}


#'Function: 	hand
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	determine the 7 card hands
#'Inputs:	@param players :	the hole cards			#matrix[nPlayers, 4]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'			@param board :	the board cards			#vector[5] in {1, 2, ..., 52}
#'Locals:	nPlayers : number of hands			#integer in {1, 2, ... , 8}
#'				i : for name in vector
#'												#numeric in vector
#'				j : index for hole cards
#'												#numeric in vector
#'Outputs:	cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'@export
hand <- function(players, board) {
	nPlayers <- nrow(players)
				#create a variable for the number of players
	cards <- matrix(0, nrow=nPlayers, ncol = 14)
				#create a variable for the cards
	for (i in 1:nPlayers) {
		j <- 1:2
			cards[i, 2*j-1] <- transformToRank(players[i,])
				#2*j-1 : column for rank	#integer in {1, 3}
			cards[i, 2*j] <- transformToSuit(players[i,])
				#2*j : column for rank	#integer in {2, 4}
		j <- 1:5
			cards[i, 4+2*j-1] <- transformToRank(board)
				#4+2*j-1 : column for rank	#integer in {5, 7, 9, 11, 13}
			cards[i, 4+2*j] <- transformToSuit(board)
				#4+2*j :column for suit		#integer in {6, 8, 10, 12, 14}
		}
	cards
}


#'Function: 	transformToRank
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	transforms a number into a rank
#'Inputs:		@param y : number corresponding to card
#'												#integer in {1, 2, ... , 52}
#'Outputs:		@return rank: rank of card y 
#'												#integer in {2, ... , 14}
#'												# 2 = deuce
#'												# .
#'												# .
#'												# .
#'												# 11 = jack
#'												# 12 = queen
#'												# 13 = king
#'												# 14 = ace 
#@export
transformToRank <- function(y) {
	rank <- (y-1) %% 13+2
				#(y-1) %% 13+2: rank of this card
	rank
}


#'Function: 	transformToSuit
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	transforms a number into a suit
#'Inputs:		@param y : number corresponding to card
#'												#integer in {1, 2, ... , 52}
#'Outputs:		@return suit: suit of card y 
#'												#integer in {1, 2, 3, 4}
#'												# 1 = ♠
#'												# 2 = ♣
#'												# 3 = ♥
#'												# 4 = ♦
#'@export
transformToSuit <- function(y) {
	suit <-  (y-1) %/% 13+1
				#(y-1) %/% 13+1: suit of this card
	suit
}


#'Function: 	transformToNumber
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	transforms a rank and suit into a number
#'Inputs:		@param rank: rank of card y 
#'												#integer in {2, ... , 14}
#'												# 2 = deuce
#'												# .
#'												# .
#'												# .
#'												# 11 = jack
#'												# 12 = queen
#'												# 13 = king
#'												# 14 = ace 
#'				@param suit: suit of card y 
#'												#integer in {1, 2, 3, 4}
#'												# 1 = ♠
#'												# 2 = ♣
#'												# 3 = ♥
#'												# 4 = ♦
#'Outputs:		@return y : number corresponding to card
#'												#integer in {1, 2, ... , 52}
#'@export
transformToNumber <- function(rank, suit) {
	y <- 13*(suit-1)+(rank-2)+1
				#the number of this card
	y
}


#'Function: 	transformToRelative
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	transforms an absolute (seat at the table) position into a relative (seats behind the dealer) position
#'Inputs:		@param nPlayers : number of hands to deal 
#'												#integer in {1, 2, ... , 8}
#'				@param position : dealer position 		#integer in {1, 2, ..., nPlayers}
#'				@param j : absolute position of a player
#'												#integer in {1, 2, ... , nPlayers}
#'Outputs:		@return k : relative position of a player
#'												#integer in {1, 2, ... , nPlayers}
#'@export
transformToRelative <- function(nPlayers, position, j) {
	if (j-position > 0) k <- j-position			
	else k <- j-position+nPlayers
	k
}
	

#'Function: 	transformToAbsolute
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	transforms a relative position (seats behind the dealer) into an absolute position (seat at the table) 
#'Inputs:		@param nPlayers : number of hands to deal 
#'												#integer in {1, 2, ... , 8}
#'				@param position : dealer position 		#integer in {1, 2, ..., nPlayers}
#'				@param k : relative position of a player
#'												#integer in {1, 2, ... , nPlayers} 
#'Outputs:		@return j : absolute position of a player
#'												#integer in {1, 2, ... , nPlayers}
#'@export
transformToAbsolute <- function(nPlayers, position, k) {
	j <- (k+position-1)%%nPlayers+1
	j
}


#'Function: 	scorer
#'
#'Author: Ben Greenspan		
#'Created: December 15, 2010
#'Updated: December 17, 2010
#'Description:	returns the ranking of one hand
#'Inputs:		@param cardsRow : one 7 card hand
#'												#vector[14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'Locals:		ranks : the indices for rank	#vector
#'				suits : the indices for suit	#vector
#' 				sortedSuits : the sorted suits
#'												#vector
#' 				sortedRanks : the sorted suits
#'												#vector
#'				suitValues : the unique values of equal suits
#'												#vector
#'												#col1: lowest unique suit
#'												#.
#'												#.
#'												#.
#'												#coln: highest unique suit
#'				suitLengths : the lengths of equal suits
#'												#vector
#'												#col1: length of lowest rank
#'												#.
#'												#.
#'												#.
#'												#coln: length of highest rank
#'				rankValues : the unique values of equal ranks
#'												#vector
#'												#col1: lowest unique rank
#'												#.
#'												#.
#'												#.
#'												#coln: highest unique rank
#'				rankLengths : the lengths of equal ranks
#'												#vector
#'												#col1: length of lowest rank
#'												#.
#'												#.
#'												#.												
#'				coln: length of highest rank
#'				j : for name in vector
#'												#numeric in vector
#'				k : the length of the length of unique ranks
#'												#integer
#'				kgt4 : the positive difference between k and 4
#'												#integer
#'				straight : create an indicator variable for the presence of a straight
#'												#boolean
#'				yTemp : the 7 card hand transformed to number
#'												#vector
#'												#col1: the number of card 1 in {1, 2, ... , 52}
#'												#.
#'												#.
#'												#.
#'												#col7: the number of card 7
#'				straightFlushCounter : a counter for the number of sequential cards of the same rank
#'												#integer
#'Outputs:		@return ranking : the ranks of the hand
#'												#integer in {1, 2, ... , 9}
#'@export
scorer <- function(cardsRow) {
	ranks <- seq(from = 1, to = 13, by = 2)
				#the columns of rank
	suits <- seq(from = 2, to = 14, by = 2)
				#the columns of suit
	sortedSuits <- sort(cardsRow[suits])
				#sort the suits from low to high
	sortedRanks <- sort(cardsRow[ranks])
				#sort the ranks
	suitValues <- rle(sortedSuits)$values
				#the unique suits
	suitLengths <- rle(sortedSuits)$lengths
				#the length of the unique suits
	rankValues <- rle(sortedRanks)$values
				#the unique ranks
	rankLengths <- rle(sortedRanks)$lengths
				#the length of the unique ranks
	k <- length(rankValues)
				#create a variable for the length of the length of unique ranks
	kgt4 <- k - 4
				#create a variable for the number of sequences to check in a straight
	straight <- FALSE
				#create an indicator variable for the presence of a straight
	straightFlush <- FALSE
				#create an indicator variable for the presence of a straight flush
	#determine the ranking of the hand
	#high card
	if (sum(rankLengths > 1) == 0) ranking <- 1
	#one pair
	if (sum(rankLengths == 2) == 1) ranking <- 2
	#two pair 
	if (sum(rankLengths == 2) >= 2) ranking <- 3
				#there can be two or three pairs
	#three of a kind
	if (sum(rankLengths == 3) >= 1) ranking <- 4
				#there can be one or two sets
	#straight 
	if (k >= 5) {
				#if there are 5 unique ranks
		if (sum(rankValues[c(k,1,2,3,4)] == c(14,2,3,4,5)) == 5) straight <- TRUE
							#check for wrap around
							#aka a low straight or wheel
		for (i in 1:kgt4) {
							#check kgt4 number of sequences
			if (rankValues[i+4] == (rankValues[i]+4)) straight <- TRUE
							#if the (i+4)th rank is the ith rank plus four 
							#the high card is the (i+4)th rank
		}
		if (straight == TRUE) ranking <- 5
	}
	#flush
	if (sum(suitLengths >= 5) == 1 ) ranking <- 6
	#full house
	if (sum(rankLengths == 3) == 2 | (sum(rankLengths == 3) == 1 & sum(rankLengths == 2) >= 1)) {
				#if there are 
				#two sets
				#or
				#a set and one/two pairs
		ranking <- 7	
	}
	#four of a kind
	if (sum(rankLengths == 4) == 1) ranking <- 8
	#straight flush
	if (straight == TRUE & sum(suitLengths >= 5) == 1) {
				#if there is
				#a straight
				#and 
				#a flush
		yTemp <- transformToNumber(cardsRow[ranks],cardsRow[suits])
				#transform cards to number
		yTemp <- sort(yTemp)
				#sort the numbers	
		for (i in 0:3){
				if (sum(yTemp %in% c(13+12*i,1+12*i,2+12*i,3+12*i,4+12*i)) == 5) straightFlush <- TRUE
				#check for a flush and a wheel
		}
		for (i in 1:3) {
				#check four sequences
			if (transformToRank(yTemp[i]) <= 10) {
				#if the low card in a straight is less than or equal to 10
				if (yTemp[i+4] == (yTemp[i]+4)) straightFlush <- TRUE
				#if the (i+4)th rank is the ith rank plus four 
				#the high card is the (i+4)th rank
				#there is a straight flush
			}
		}
		if (straightFlush == TRUE) ranking <- 9
	}
	ranking
}


#'Function: 	showdown
#'
#'Author: Ben Greenspan		
#'Created: December 15, 2010
#'Description:	returns the ranking of the hands
#'Inputs: 		@param cards :	the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'Locals:				
#'Outputs: 	#score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'@export
showdown <- function(cards) {
	score <- apply(cards,1,scorer)
	score
	}
	

#'Function: 	dotHighcardCompare
#'
#'Author: Ben Greenspan		
#'Created: December 17, 2010
#'Description:	determines the players with the high card
#'Inputs:		@param rankMatrix : the ranks from the 7 card hand
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'Locals:		step : the step for comparison of highest card
#'												#integer
#'				Tie : the positions of the players with Tied highest cards
#'												#vector
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotHighcardCompare <- function(rankMatrix) {
	#rankMatrix <- t(apply(rankMatrix,1,sort,decreasing=TRUE))
							#sort the ranks 
							#CANT APPLY IF RANKMATRIX IS A ROW
							#UNNECESSARY IF RANKMATRIX IS SORTED CORRECTLY
							
	for (step in 2:ncol(rankMatrix)) {
		Tie <- which(rankMatrix[,step-1]==max(rankMatrix[,step-1])) 
							#determine which rows are tied
		rankMatrix[-Tie,] <- 0		
							#zero out the others
		winner <- which(rankMatrix[,step]==max(rankMatrix[,step]))
							#which of the hands have the high card hand
	}
	winner
}


#'Function: 	dotHighcard
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	determines the players with a high card hand
#'Inputs:		@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				step : the step for comparison of highest card
#'												#integer
#'				Tie : the positions of the players with Tied highest cards
#'												#vector
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotHighcard <- function(cards) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[, ranks] 	#assume score = 1 for all players
							#create a copy of cards with ranks only
							#sorts the ranks
	temp <- t(apply(temp,1,sort,decreasing=TRUE))
							#systematic scan
	winner <- dotHighcardCompare(temp)
	winner
}	


#'Function: 	dotPairRanker
#'
#'Author: Ben Greenspan		
#'Created: December 17, 2010
#'Description:	This function requires a hand with a score of 2. The function determines the rank of the pair.  
#' THIS FUNCTION WORKS BEST WHEN RANKS ARE SORTED IN DECREASING ORDER.
#'Inputs:		@param oneHand : a sorted hand with ranks only.
#'												#vector[7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				@param rankValues : the unique values of equal ranks
#'												#vector
#'												#col1: lowest unique rank
#'												#.
#'												#.
#'												#.
#'												#coln: highest unique rank
#'				@param rankLengths : the lengths of equal ranks
#'												#vector
#'												#col1: length of lowest rank
#'												#.
#'												#.
#'Outputs:		@return pairRank : the rank of the pair
#'												#vector
#'@export
dotPairRanker <- function(oneHand) {
	rankValues <- rle(oneHand)$values
				#the unique ranks
	rankLengths <- rle(oneHand)$lengths
				#the length of the unique ranks
	pairRank <- rankValues[rankLengths == 2]
				#the rank of the pair
	pairRank
}


#'Function: 	dotPair
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	determines the players with the highest pair and kicker cards.
#'Inputs:		@param nPlayers : number of hands		#integer in {1, 2, ... , 8}
#'				@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				pairRanks : the rank of each pair
#'												#vector[nPlayers]
#'				kickers : the kicker cards with respect to a pair, i.e. 3 highest nonpaired cards
#'												#matrix[nPlayers,3]
#'				step : the step for comparison of highest card
#'												#integer
#'				Tie : the positions of the players with Tied kicker cards
#'												#vector
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotPair <- function(nPlayers,cards,score) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[,ranks]	#create a copy of cards with ranks only
	temp[which(score!=2),] <- 0 	
							#when there is not a pair,
							#set the copy to zero.
	temp <- t(apply(temp,1,sort,decreasing=TRUE))
							#sorts the ranks
	pairRanks <- rep(0, nrow(temp))
							#create a vector for pair rank
	pairRanks[which(score==2)] <- apply(temp[which(score==2),],1,dotPairRanker)
							#determine the pairRank
	winner <- which(pairRanks == max(pairRanks))
							#the absolute position of players with the highest pair
							#
							#systematic scan for kicker cards
	temp[-winner,] <- 0
							#if not the highest pair, set to zero
	kickers <- matrix(0, nrow=nrow(temp), ncol=3)
							#create a copy of cards with kickers only.
	for (i in 1:nPlayers){
			if (temp[i,1]==0) kickers[i,1:3] = 0
							#kickers are zero if not highest pair
			else kickers[i,] <- subset(temp[i,], temp[i,] != pairRanks[i])[1:3]
							#otherwise, kickers are 3 highest nonpair cards
	}
	winner <- dotHighcardCompare(kickers)
							#determine the player with the top kicker
	winner
}


#'Function: 	dotTwoPairRanker
#'
#'Author: Ben Greenspan		
#'Created: December 17, 2010
#'Description:	This function requires a hand with a score of 2. The function determines the rank of the pair.  
#' THIS FUNCTION WORKS BEST WHEN RANKS ARE SORTED IN DECREASING ORDER.
#'Inputs:		@param oneHand : a sorted hand with ranks only.
#'												#vector[7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				@param rankValues : the unique values of equal ranks
#'												#vector
#'												#col1: lowest unique rank
#'												#.
#'												#.
#'												#.
#'												#coln: highest unique rank
#'				@param rankLengths : the lengths of equal ranks
#'												#vector
#'												#col1: length of lowest rank
#'												#.
#'												#.
#'Outputs:		@return pairRank : the rank of the pair
#'												#vector
#'@export
dotTwoPairRanker <- function(oneHand) {
	rankValues <- rle(oneHand)$values
				#the unique ranks
	rankLengths <- rle(oneHand)$lengths
				#the length of the unique ranks
	pairRank <- rankValues[rankLengths == 2]
				#the ranks of the pairs
	pairRank[1:2]
				#the rank of the top two pairs
}


#'Function: 	dotTwoPairs
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	determines the players with the highest pair and kicker cards.
#'Inputs:		@param nPlayers : number of hands		#integer in {1, 2, ... , 8}
#'				@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				numPairs : the number of pairs for each player
#'												#vector[nPlayers]
#'												# 2 = two pairs
#'												# 3 = three pairs
#'				pairRanks : the rank of each set of three of a kind
#'												#vector[nPlayers]
#'				kicker : the two highest unpaired cards
#'												#matrix[nPlayers,2]
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotTwoPairs <- function(nPlayers,cards,score) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[,ranks]	#create a copy of cards with ranks only
	temp[which(score!=3),] <- 0 	
							#when there is not a set,
							#set the copy to zero.
	temp <- t(apply(temp,1,sort,decreasing=TRUE))
							#sorts the ranks
	pairRanks <- matrix(0, nPlayers,2)
							#create a vector for pair ranks
	pairRanks[which(score==3),] <- t(apply(temp[which(score==3),],1,dotTwoPairRanker))
							#determine the ranks of the pairs
	winner <- dotHighcardCompare(pairRanks)
							#determine which players have the highest two pair
	temp[-winner,] <- 0
							#if not the highest two pairs, set to zero
	kickers <- matrix(0, nrow=nrow(temp), ncol=1)
							#create a copy of cards with kickers only.
	for (i in 1:nPlayers){
			if (temp[i,1]==0) kickers[i,1] = 0
							#kickers are zero if not highest two pair
			else kickers[i,] <- subset(temp[i,],temp[i,] != pairRanks[i,1] & temp[i,] != pairRanks[i,2])[1]
							#otherwise, kicker is highest card not in the top and bottom pair
	}
	winner <- which(kickers == max(kickers))
							#determine which hand with top and bottom pair has the highest kicker
	winner
}


#'Function: 	dotTripRanker
#'
#'Author: Ben Greenspan		
#'Created: December 17, 2010
#'Description:	This function requires a hand with a score of 2. The function determines the rank of the pair.  
#'Inputs:		@param oneHand : a sorted hand with ranks only.
#'												#vector[7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				@param rankValues : the unique values of equal ranks
#'												#vector
#'												#col1: lowest unique rank
#'												#.
#'												#.
#'												#.
#'												#coln: highest unique rank
#'				@param rankLengths : the lengths of equal ranks
#'												#vector
#'												#col1: length of lowest rank
#'												#.
#'												#.
#'Outputs:		@return pairRank : the rank of the pair
#'												#vector
#'@export
dotTripRanker <- function(oneHand) {
	rankValues <- rle(oneHand)$values
				#the unique ranks
	rankLengths <- rle(oneHand)$lengths
				#the length of the unique ranks
	tripRank <- rankValues[rankLengths == 3]
				#the ranks of the sets
	tripRank[1]
				#the rank of the top set
}


#'Function: 	dotTrips
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	determines the players with the highest pair and kicker cards.
#'Inputs:		@param nPlayers : number of hands		#integer in {1, 2, ... , 8}
#'				@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				tripRank : the rank of each set of three of a kind
#'												#vector[nPlayers]
#'				kickers : the two highest unpaired cards
#'												#matrix[nPlayers,2]
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotTrips <- function(nPlayers,cards,score) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[,ranks]	#create a copy of cards with ranks only
	temp[which(score!=4),] <- 0 	
							#when there is not a set,
							#set the copy to zero.
	temp <- t(apply(temp,1,sort,decreasing=TRUE))
							#sorts the ranks
	tripRanks <- matrix(0, nPlayers,1)
							#create a vector for trip rank
	tripRanks[which(score==4),] <- apply(temp[which(score==4),],1,dotTripRanker)
							#determine the rank of the set
	winner <- which(tripRanks == max(tripRanks))
							#the absolute position of players with the biggest set
	temp[-winner,] <- 0
							#if not the highest pair, set to zero
	kickers <- matrix(0, nrow=nrow(temp), ncol=2)
							#create a copy of cards with kickers only.
	for (i in 1:nPlayers){
			if (temp[i,1]==0) kickers[i,1:2] = 0
							#kickers are zero if not highest set
			else kickers[i,] <- subset(temp[i,], temp[i,] != tripRanks[i])[1:2]
							#otherwise, kickers are 2 highest unmatched cards
	}
	winner <- dotHighcardCompare(kickers)
							#determine the player with the top kicker
	winner
}


#'Function: 	dotStraightRanker
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	returns the highest card in the straight
#'Inputs:		@param oneHand : a hand sorted from low to high with score of 5
#'												#vector[7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'Locals:		k : length of unique ranks 		#integer in {2, 3, ... , 7}
#'				counter : number of cards in sequence
#'Outputs:		@return straightRank : the top card in the straight
#'												#integer
#'@export
dotStraightRanker <- function(oneHand) {
	oneHand <- sort(oneHand)
							#sort the values from low to high
	rankValues <- rle(oneHand)$values
							#the unique ranks		
	k <- length(rankValues)
							#the number of unique cards
	kgt4 <- k - 4
							#create a variable for the number of sequences to check
	if (sum(rankValues[c(k,1,2,3,4)] == c(14,2,3,4,5)) == 5) straightRank <- 5
							#check for wrap around
							#aka a low straight or wheel
	for (i in 1:kgt4) {
							#check kgt4 number of sequences
		if (rankValues[i+4] == (rankValues[i]+4)) straightRank <- rankValues[i+4]
							#if the (i+4)th rank is the ith rank plus four 
							#the high card is the (i+4)th rank
	}
	straightRank
}


#'Function: 	dotStraight
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Description:	determines the player with the highest straight
#'Inputs:		@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				maxStraight : the high card in each straight
#'												#vector[nPlayers]
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotStraight <- function(cards,score) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[, ranks] 	#assume score = 1 for all players
							#create a copy of cards with ranks only
	temp <- t(apply(temp,1,sort,decreasing=FALSE))
							#sorts the ranks
	temp[-which(score==5),] <- 0
							#if not a straight, set temp to zero. SLIGHTLY UNNECESSARY, BUT GOOD TO KEEP IN

	straightRanks <- matrix(0, nrow=nrow(temp), ncol=1)
							#create a variable for the high card in the straight
	straightRanks[which(score==5),] <- apply(temp[which(score==5),],1,dotStraightRanker)
							#determine the high card in each straight
	winner <- which(straightRanks == max(straightRanks))
							#determine the player with the top straight
	winner
}	


#'Function: 	dotFlushRanker
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	returns the rank of top 5 cards in the flush
#'Inputs:		@param cardsRow : one 7 card hand
#'												#vector[14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'Locals:		suits : the indices for suit	#vector
#' 				sortedSuits : the sorted suits
#'												#vector
#'				suitValues : the unique values of equal suits
#'												#vector
#'												#col1: lowest unique suit
#'												#.
#'												#.
#'												#.
#'												#coln: highest unique suit
#'				suitLengths : the lengths of equal suits
#'												#vector
#'												#col1: length of lowest rank
#'												#.
#'												#.
#'												#.
#'												#coln: length of highest rank
#'				flushSuit : the suit of the flush
#'												#integer
#'				flushIndex : the indices of cards in flush
#'												#vector of integers 
#'												#col1: index in {1,2,...,7}
#'												#.
#'												#.
#'												#.
#'												#coln: index in {1,2,...,7}
#'Outputs:		@return flushRank : the rank of 5 high cards in flush
#'												#vector[5]
#'												#col1: suit of card 1 in {2, ... , 14}
#'												#.
#'												#.
#'												#.
#'												#col5: suit of card n in {2, ... , 14}
#'Outputs:		@return winner : absolute position of the winner
#'												#vector									
#'@export								
dotFlushRanker <- function(cardsRow){
	suits <- seq(from = 2, to = 14, by = 2)
				#the columns of suit
	sortedSuits <- sort(cardsRow[suits])
				#sort the suits from low to high
	suitValues <- rle(sortedSuits)$values
				#the unique suits
	suitLengths <- rle(sortedSuits)$lengths
				#the length of the unique suits
	flushSuit <- suitValues[suitLengths >= 5]
				#the flush suit
	flushIndex <- which(cardsRow[suits] == flushSuit)
				#the index of cards in flush
	flushRank <- cardsRow[2*flushIndex - 1]
				#rank of each card in flush
	flushRank <- sort(flushRank,decreasing=TRUE)[1:5]
				#5 high cards in flush
	flushRank
}


#'Function: 	dotFlush
#'
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Updated: December 17, 2010
#'Description:	determines the player with the highest flush
#'Inputs:		@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:	flushRank : the rank of 5 high cards in flush
#'												#matrix[nPlayers,5]
#'												#col1: suit of card 1 in {2, ... , 14}
#'												#.
#'												#.
#'												#.
#'												#col5: suit of card n in {2, ... , 14}	
#'@export
dotFlush <- function(cards,score) {
	flushRanks <- matrix(0, nrow=nPlayers, ncol=5)
				#create a vector for each rank in flush
	flushRanks[which(score==6),] <- t(apply(cards[which(score==6),],1,dotFlushRanker))
				#if there is a flush,
				#fill in top 5 ranks of the same suit
	winner <- dotHighcardCompare(flushRanks)
				#determine the player with the top flush
	winner
}


#'Function: 	dotFullHouseRanker
#'
#'Author: Ben Greenspan		
#'Created: December 15, 2010
#'Description:	returns the rank of the top set and the top pair
#'Inputs:		@param oneHand : the ranks of one 7 card hand
#'												#vector[7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'Locals:		values : the unique values of equal ranks
#'												#vector
#'												#col1: top unique rank
#'												#.
#'												#.
#'												#.
#'												#coln: bottom unique rank
#'				lengths : the lengths of equal ranks
#'												#vector
#'												#col1: length of top rank
#'												#.
#'												#.
#'												#.
#'												#coln: length of bottom rank
#'Outputs:		@return fullHouseRank : the ranks of the high set and the high pair
#'												#vector
#'												#col1: the rank of the top set
#'												#col2: the rank of the top pair
#'@export
dotFullHouseRanker <- function(oneHand) {
							#rle is the equivalent of the Unix uniq -c command.
	values <- rle(oneHand)$values
							#compute the values of equal ranks
	lengths <- rle(oneHand)$length
							#compute the lengths of equal ranks 
							#A full Boat could have 2 sets, 1 set and 2 pairs, or 
							#1 set and 1 pair.
	fullHouseRank <- rep(0,2)
							#create a vector for pair ranks
	if (sum(lengths == 3) == 2) {
							#if there are two sets
							#choose the top set as the set
							#choose the bottom set as the pair
		fullHouseRank[1] <- max(values[lengths == 3])
		fullHouseRank[2] <- min(values[lengths == 3])
		}
	else if (sum(lengths == 2) == 2) {
							#if there are two pairs
							#choose the set as the set
							#choose the top pair as the pair
		fullHouseRank[1] <- values[lengths == 3]
		fullHouseRank[2] <- max(values[lengths == 2])
		}
	else if (sum(lengths == 2) == 1) {
							#if there is one pair
							#choose the set as the set
							#choose the pair as the pair
		fullHouseRank[1] <- values[lengths == 3]
		fullHouseRank[2] <- values[lengths == 2]
		}
	fullHouseRank
}


#'Function: 	dotFullHouse
#'
#'Author: Ben Greenspan		
#'Created: December 15, 2010
#'Description:	determines the player with the highest boat
#'Inputs:		@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 5]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				fullHouseRanks : a matrix for the rank of the high set and the high pair
#'												#matrix[nPlayers,2]
#'												#col1: rank of high set
#'												#col2: rank of high pair
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotFullHouse <- function(cards,score) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[,ranks]	#create a copy of cards with ranks only
	temp[-which(score==7),] <- 0
							#when there is not a boat,
							#set the copy to zero.
							#SLIGHTLY UNNECESSARY
	temp <- t(apply(temp,1,sort,decreasing=TRUE))
							#sorts the ranks
	fullHouseRanks <- matrix(0,nrow=nPlayers,ncol=2)
							#create a variable for the rank of the high set and the high pair
	fullHouseRanks[-which(score==7),] <- 0
							#if there is not a full house, then set to zero
	fullHouseRanks[which(score==7),] <- t(apply(temp[which(score==7),],1,dotFullHouseRanker))
							#determine the rank of the high set and the high pair
	winner <- which(fullHouseRanks[,1] == max(fullHouseRanks[,1]))
							#determine the players with the high set
	fullHouseRanks[-winner,] <- 0
							#if player does not have the high set, zero out their full house
	winner <- which(fullHouseRanks[,2] == max(fullHouseRanks[,2]))
							#determine which player with the high set has the high pair
	winner					
}


#'Function: 	dotFourOfAKindRanker
#'
#'Author: Ben Greenspan		
#'Created: December 15, 2010
#'Updated: December 17, 2010
#'Description:	returns the rank of the four of a kind and the kicker
#'Inputs:		@param oneHand : the ranks of one 7 card hand
#'												#vector[7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'Locals:		values : the unique values of equal ranks
#'												#vector
#'												#col1: highest unique rank
#'												#.
#'												#.
#'												#.
#'												#coln: lowest unique rank
#'				lengths : the lengths of equal ranks
#'												#vector
#'												#col1: length of highest rank
#'												#.
#'												#.
#'												#.
#'												#coln: length of lowest rank
#'Outputs:		@return fourOfAKindRank : the ranks of the quads and the high kicker
#'												#vector
#'												#col1: the rank of the quads
#'												#col2: the rank of the kicker
#'@export
dotFourOfAKindRanker <- function(oneHand) {
							#rle is the equivalent of the Unix uniq -c command.
	values <- rle(oneHand)$values
							#compute the values of equal ranks
	lengths <- rle(oneHand)$length
							#compute the lengths of equal ranks 
							#A full Boat could have 2 sets, 1 set and 2 pairs, or 
							#1 set and 1 pair.
	fourOfAKindRank <- rep(0,2)
							#create a vector for pair ranks
							#choose the four of a kind as the quad
							#choose the high "other" as the kicker
	fourOfAKindRank[1] <- values[lengths == 4]
	fourOfAKindRank[2] <- max(values[lengths != 4])
	fourOfAKindRank
}


#'Function: 	dotFourOfAKind
#'
#'Author: Ben Greenspan		
#'Created: December 15, 2010
#'Updated: December 17, 2010
#'Description:	determines the player with the four of a kind
#'Inputs:		@param nPlayers : number of hands		#integer in {1, 2, ... , 8}
#'				@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 5]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				fourOfAKindRanks : a matrix for the rank of the quads and the kicker
#'												#matrix[nPlayers,2]
#'												#col1: rank of high set
#'												#col2: rank of high pair
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotFourOfAKind <- function(nPlayers,cards,score) {
	ranks <- seq(from = 1, to = 13, by = 2)	
							#the columns for rank
	temp <- cards[,ranks]	#create a copy of cards with ranks only
	temp <- t(apply(temp,1,sort,decreasing=TRUE))
							#sorts the ranks
	temp[-which(score==8),] <- 0
							#when there is not a boat,
							#set the copy to zero.
	fourOfAKindRanks <- matrix(0,nrow=nPlayers,ncol=2)
							#create a variable for the rank of the quads and the kicker
	fourOfAKindRanks[which(score==8),] <- t(apply(temp[which(score==8),],1,dotFourOfAKindRanker))
							#determine the rank of the four of a kind and the kicker
	winner <- which(fourOfAKindRanks[,1] == max(fourOfAKindRanks[,1]))
							#determine the players with the high four of a kind
	fourOfAKindRanks[-winner,] <- 0
							#if player does not have the high four of a kind, zero out their quads
	winner <- which(fourOfAKindRanks[,2] == max(fourOfAKindRanks[,2]))
							#determine which player with the high four of a kind has the high kicker
	winner					
}


#'Function: 	dotStraightFlushRanker
#'Author: Ben Greenspan		
#'Created: December 17, 2010
#'Description:	returns the top card in the straight flush
#'Inputs:		@param yTempRow : a sorted 7 card hand of numbers
#'												#vector[7]
#'												#col1: number of card 1 in {1, 2, ... , 52}
#'												#col2: number of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: number of card 7
#'Locals:		
#'Outputs:		@return straightFlushRank : the top card in the straight flush
#'												#integer
#'@export								
dotStraightFlushRanker <- function(yTempRow) {
	for (i in 0:3){
				if (sum(yTempRow %in% c(13+12*i,1+12*i,2+12*i,3+12*i,4+12*i)) == 5) straightFlushRank <- 5
				#if there is a flush and a wheel
				#the top card is a 5
	}
	for (i in 1:3) {
				#check four sequences
			if (transformToRank(yTempRow[i]) <= 10) {
				#if the low card in a straight is less than or equal to 10
				if (yTempRow[i+4] == (yTempRow[i]+4)) straightFlushRank <- yTempRow[i+4]
				#if the (i+4)th rank is the ith rank plus four 
				#the high card is the (i+4)th rank
				#there is a straight flush
			}
	}
	straightFlushRank <- transformToRank(straightFlushRank)
				#transform the top card in straight flush to rank
	straightFlushRank
}


#'Function: 	dotStraightFlush
#'Author: Ben Greenspan		
#'Created: December 14, 2010
#'Description:	determines the player with the highest straight flush
#'Inputs:		@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:		ranks : the indices for rank	#vector
#'				temp : a copy of cards with ranks only
#'												#matrix[nPlayers, 7]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: rank of card 2
#'												#.
#'												#.
#'												#.
#'												#col7: rank of card 7
#'				maxStraight : the high card in each straight
#'												#vector[nPlayers]
#'Outputs:		@return winner : absolute position of the winner
#'												#vector
#'@export
dotStraightFlush <- function(nPlayers, cards, score) {
	ranks <- seq(from = 1, to = 13, by = 2)
				#the columns of rank
	suits <- seq(from = 2, to = 14, by = 2)
				#the columns of suit
	yTemp <- matrix(0,nrow=nPlayers,ncol=7)
				#create a copy of the 7 card hand transformed to number
	for (i in 1:nPlayers) {
		yTemp[i,] <- transformToNumber(cards[i,ranks],cards[i,suits])
				#transform cards to number
	}
	yTemp[-which(score==9),] <- 0
				#if not a straight flush, set to zero
	yTemp <- t(apply(yTemp,1,sort))	
				#sort the numbers
	straightFlushRanks <- matrix(0, nrow=nPlayers, ncol=1)
				#create a variable for the top card in the straight flush
	straightFlushRanks[which(score==9),] <- t(apply(yTemp[which(score==9),],1,dotStraightFlushRanker))
				#determine the top card in each straight flush
	winner <- which(straightFlushRanks == max(straightFlushRanks))
				#determine the player with the top straight flush
	winner
}


#'Function: 	tiebreaker
#'
#'Description:	in case of >= 2 similar scores, determines a winner
#'Inputs: 		@param nPlayers : number of hands			#integer in {1, 2, ... , 8}
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'				@param cards :	the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'Locals:	
#'Outputs: 	#winner : the absolute position of the winner[s]
#'												#vector[]
#'@export
tiebreaker <- function(nPlayers,cards,score) {
	winner <- which(score==max(score))
	if (length(winner) > 1) {
			#if length(max(scores)) >= 2
			#there is a tie
			if (max(score)==1) winner <- dotHighcard(cards)
			if (max(score)==2) winner <- pair(nPlayers,cards,score)
			if (max(score)==3) winner <- twoPairs(nPlayers,cards,score)
			if (max(score)==4) winner <- trips(nPlayers,cards,score)
			if (max(score)==5) winner <- straight(cards,score)
			if (max(score)==6) winner <- flush(cards,score)
			if (max(score)==7) winner <- fullHouse(cards,score)
			if (max(score)==8) winner <- fourOfAKind(nPlayers,cards,score)
			if (max(score)==9) winner <- straightFlush(nPlayers,cards,score)
	}
	winner
}


#'Function: 	CGIplayers
#'
#'Author: Ben Greenspan		
#'Created: May/June 2010
#'Description:	computer graphics
#'Inputs: 		@param time : the current round			#integer in {1, 2, 3, 4}
#'												# 1 = pre-flop
#'												# 2 = flop
#'												# 3 = turn
#'												# 4 = river
#'				@param alias : names of players			#vector[nPlayers]
#'				@param position : dealer position 		#integer in {1, 2, ..., nPlayers}
#'				@param cards : the 7 card hand
#'												#matrix[nPlayers, 14]
#'												#col1: rank of card 1 in {2, ... , 14}
#'												#col2: suit of card 1 in {1, 2, 3, 4}
#'												#col3: rank of card 2
#'												#col4: suit of card 2
#'												#.
#'												#.
#'												#.
#'												#col13: rank of card 7
#'												#col14: suit of card 7
#'				@param score : the score of the hand in absolute terms
#'												#vector[nPlayers]
#'												# 9 = Straight Flush
#'												# 8 = Four of a Kind
#'												# 7 = Full House
#'												# 6 = Flush
#'												# 5 = Straight
#'												# 4 = Three of a Kind
#'												# 3 = Two Pair
#'												# 2 = One Pair
#'												# 1 = High Card
#'Locals:	#nPlayers : number of hands			#integer in {1, 2, ... , 8}
#'			#width : width of the plot			#integer in {20, 30, ... , 10*nPlayers}
#'			#height : height of the plot		#integer in {10}
#'			#X1 : x-coordinate for Players		#real number in [0, width]
#'			#Y1 : y-coordinate for Players		#real number in [0, height]
#'			#X2 : x-coordinate for Cards		#real number in [0, width]
#'			#Y2 : y-coordinate for Cards		#real number in [0, height]
#'			#X3 : x-coordinate for Community	#real number in [0, width]
#'			#Y3 : y-coordinate for Community	#real number in [0, height]
#'			#X4 : x-coordinate for outline		#real number in [0, width]
#'			#Y4 : y-coordinate for outline		#real number in [0, height]
#'Outputs: 
#'@export
CGIplayers <- function(time, alias, position, cards, score) {
	nPlayers <- nrow(cards)
	width <- 30
	height <- 20
	X1 <- numeric(nPlayers)
	Y1 <- numeric(nPlayers)
	X2 <- numeric(nPlayers)
	Y2 <- numeric(nPlayers)
	X3 <- numeric(5)
	Y3 <- height-5.5
	#PRE-FLOP
	if (time == 1) {
		plot(0,0, xlim=c(0,width), ylim=c(0,height), axes=FALSE, xlab="", ylab="", cex= 0, main="Ben's Poker Simulator")
		text(width/2,Y3+.7, "Community")
		X4 <- c(width/2-7.5,width/2+1.5,width/2+1.5,width/2-7.5,width/2-7.5,width/2+1.5,width/2+4.5,width/2+4.5,
			width/2+1.5,width/2+1.5,width/2+4.5,width/2+7.5,width/2+7.5,width/2+4.5)
		Y4 <- c(Y3+.35,Y3+.35,Y3-.35,Y3-.35,Y3+.35,Y3+.35,Y3+.35,Y3-.35,Y3-.35,Y3+.35,Y3+.35,Y3+.35,Y3-.35,Y3-.35)
		points(X4,Y4,"l", col="green")
		if (nPlayers==2) {
			X1 <- seq(from = 10,to = width-10, length = 2)
			Y1 <- rep(3.5,2)
			}
		if (nPlayers==3) {
			X1 <- seq(from = 5,to = width-5, length = 3)
			Y1 <- rep(3.5,3)
			}
		if (nPlayers==4) {
			X1 <- c(5,seq(from = 10,to = width-10, length = 2),width-5)
			Y1 <- c(6,rep(3.5,2),6)
			}
		if (nPlayers==5) {
			X1 <- c(5,seq(from = 5,to = width-5, length = 3),width-5)
			Y1 <- c(6,rep(3.5,3),6)
			}
		if (nPlayers==6) {
			X1 <- c(rep(5,2),seq(from = 10,to = width-10, length = 2),rep(width-5,2))
			Y1 <- c(8.5,6,rep(3.5,2),6,8.5)
			}
		if (nPlayers==7) {
			X1 <- c(rep(5,2),seq(from = 5,to = width-5, length = 3),rep(width-5,2))
			Y1 <- c(8.5,6,rep(3.5,3),6,8.5)
			}
		if (nPlayers==8) {
			X1 <- c(rep(5,3),seq(from = 10,to = width-10, length = 2),rep(width-5,3))
			Y1 <- c(11,8.5,6,rep(3.5,2),6,8.5,11)
			}
		if (nPlayers==9) {
			X1 <- c(rep(5,3),seq(from = 5,to = width-5, length = 3),rep(width-5,3))
			Y1 <- c(11,8.5,6,rep(3.5,3),6,8.5,11)
			}
		for (i in 1:nPlayers) 
		{
				text(X1[i], Y1[i], alias[i])
				for (j in 1:2) {			
					#RANK
					X2 <- X1[i]-2+3*(j-1)
					Y2[i] <- Y1[i] - .5
					if(cards[i,2*j-1]<=10) {
						text(X2,Y2[i], cards[i,2*j-1])
						}
					if(cards[i,2*j-1]==11) {
						text(X2,Y2[i], "J")
						}
					if(cards[i,2*j-1]==12) {
						text(X2,Y2[i], "Q")
						}
					if(cards[i,2*j-1]==13) {
						text(X2,Y2[i], "K")
						}
					if(cards[i,2*j-1]==14) {
						text(X2,Y2[i], "A")
						}			
					#SUIT
					if(cards[i,2*j]==1) {
						text(X2+1,Y2[i],"♠")
						}
					if(cards[i,2*j]==2) {
						text(X2+1,Y2[i],"♣")
						}
					if(cards[i,2*j]==3) {
						text(X2+1,Y2[i],"♥")
						}
					if(cards[i,2*j]==4) {
						text(X2+1,Y2[i],"♦")
						}	
				}
		}
	}		
	#FLOP
	if (time == 2 ) {
		X3 <- seq(from = width/2-6.5, by = 3, length = 3)
		for (i in 1:3) {
			if(cards[1,3+2*i]<=10) {
				text(X3[i],Y3, cards[1,3+2*i])
				}
			if(cards[1,3+2*i]==11) {
				text(X3[i],Y3, "J")
				}
			if(cards[1,3+2*i]==12) {
				text(X3[i],Y3, "Q")
				}
			if(cards[1,3+2*i]==13) {
				text(X3[i],Y3, "K")
				}
			if(cards[1,3+2*i]==14) {
				text(X3[i],Y3, "A")
				}			
			#SUIT
			if(cards[1,4+2*i]==1) {
				text(X3[i]+1,Y3,"♠")
				}
			if(cards[1,4+2*i]==2) {
				text(X3[i]+1,Y3,"♣")
				}
			if(cards[1,4+2*i]==3) {
				text(X3[i]+1,Y3,"♥")
				}
			if(cards[1,4+2*i]==4) {
				text(X3[i]+1,Y3,"♦")
				}
		}
	}
	#TURN
	if (time == 3 ) {
		X3 <- seq(from = width/2-6.5, by = 3, length = 5)
		i <- 4	
		if(cards[1,3+2*i]<=10) {
			text(X3[i],Y3, cards[1,3+2*i])
			}
		if(cards[1,3+2*i]==11) {
			text(X3[i],Y3, "J")
			}
		if(cards[1,3+2*i]==12) {
			text(X3[i],Y3, "Q")
			}
		if(cards[1,3+2*i]==13) {
			text(X3[i],Y3, "K")
			}
		if(cards[1,3+2*i]==14) {
			text(X3[i],Y3, "A")
			}			
		#SUIT
		if(cards[1,4+2*i]==1) {
			text(X3[i]+1,Y3,"♠")
			}
		if(cards[1,4+2*i]==2) {
			text(X3[i]+1,Y3,"♣")
			}
		if(cards[1,4+2*i]==3) {
			text(X3[i]+1,Y3,"♥")
			}
		if(cards[1,4+2*i]==4) {
			text(X3[i]+1,Y3,"♦")
			}
	}
	#RIVER	
	if (time == 4 ) {
	X3 <- seq(from = width/2-6.5, by = 3, length = 5)
		i <- 5	
		if(cards[1,3+2*i]<=10) {
			text(X3[i],Y3, cards[1,3+2*i])
			}
		if(cards[1,3+2*i]==11) {
			text(X3[i],Y3, "J")
			}
		if(cards[1,3+2*i]==12) {
			text(X3[i],Y3, "Q")
			}
		if(cards[1,3+2*i]==13) {
			text(X3[i],Y3, "K")
			}
		if(cards[1,3+2*i]==14) {
			text(X3[i],Y3, "A")
			}			
		#SUIT
		if(cards[1,4+2*i]==1) {
			text(X3[i]+1,Y3,"♠")
			}
		if(cards[1,4+2*i]==2) {
			text(X3[i]+1,Y3,"♣")
			}
		if(cards[1,4+2*i]==3) {
			text(X3[i]+1,Y3,"♥")
			}
		if(cards[1,4+2*i]==4) {
			text(X3[i]+1,Y3,"♦")
			}
	}
}
