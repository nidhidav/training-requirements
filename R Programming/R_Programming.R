# build board
board <- data.frame(
  c("NA", "NA", "NA"),
  c("NA", "NA", "NA"),
  c("NA", "NA", "NA")
)
rownames(board) <- c("row 1", "row 2", "row 3")
colnames(board) <- c("col 1", "col 2", "col 3")
print(board)

#function to place computer's move
comp_move <- function (value, myboard) {
  assigned <- FALSE
  while (assigned == FALSE) {
    row = sample(1:3,1)
    col = sample(1:3, 1)
    if (myboard[row,col] == "NA") {
      myboard[row,col] = value
      assigned <- TRUE
    }
  }
  cat("\n","Computer move placed","\n","Current board:","\n")
  print(myboard)
  cat(" \n")
  return(myboard)
}
#function to ask row
ask_row <- function(){
  valid <- FALSE
  while(valid == FALSE) {
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    cat("What row? ")
    row_pos <- readLines(con = con, n = 1)
    row_pos <- as.numeric(row_pos)
    if(row_pos %in% c(1,2,3)) {
      valid <- TRUE
    } else {
      cat("Row value is not valid, please select from 1, 2, or 3", " \n")
    }
  }
  return(row_pos)
}
#function to ask column
ask_col <- function(){
  valid <- FALSE
  while(valid == FALSE) {
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    cat("What col? ")
    col_pos <- readLines(con = con, n = 1)
    col_pos <- as.numeric(col_pos)
    if(col_pos %in% c(1,2,3)) {
      valid <- TRUE
    } else {
      cat("Column value is not valid, please select from 1, 2, or 3", " \n")
    }
  }
  return(col_pos)
}

#function to place player's move
player_move <- function(value, myboard){
  cat(value, "'s turn!", "\n")
  #check if viable position
  valid_pos <- FALSE
  while (valid_pos == FALSE) {
    row_pos <- ask_row()
    col_pos <- ask_col()
    if(myboard[row_pos,col_pos] == "NA") {
      valid_pos <- TRUE
      myboard[row_pos,col_pos] <- value
    } else {
      cat("That place is already taken, please re-enter the row and col", "\n")
      valid_pos <- FALSE
    }
  }
  cat("Move placed", "\n")
  return(myboard)
}

#function to check if game ends
winner <- "no one"
game_end <- function(myboard) {
  #need to check three in a row that isn't NA 
  #returns columns + rows w three in a row, and values of those columns/rows
  same_col <- which(apply(myboard, 2, function(x) length(unique(x))) == 1)
  same_row <- which(apply(myboard, 1, function(x) length(unique(x))) == 1)
  #end game if rows are not NA (X or O)
  if (any(myboard[,same_col]!="NA")) {
    winner <- myboard[1,same_col]
    for (x in winner) {
      if (x != 'NA') {
        winner <- x
      }
    }
  }
  if (any(myboard[same_row,]!="NA")) {
    winner <- myboard[same_row,1]
    for (x in winner) {
      if (x != 'NA') {
        winner <- x
      }
    }
  }
  #check diagonal
  if (all(sapply(list(myboard[1,1],myboard[2,2],myboard[3,3]), function(x) x == myboard[3,3])) | all(sapply(list(myboard[1,3],myboard[2,2],myboard[3,1]), function(x) x == myboard[3,1]))){
    if (myboard[2,2] != "NA"){
      winner <- myboard[2,2]
    }
  }
  #check tie - no NA in the board
  if (any(myboard=="NA") == FALSE  & winner == "no one") {
    winner<- "tie"
  }
  return(winner)
}

#assign player and computer
name_player <- function(){
  name_valid <- FALSE
  cat("X or O? ")
  while (!name_valid){
    if (interactive()) {
      con <- stdin()
    } else {
      con <- "stdin"
    }
    myplayer <- readLines(con = con, n = 1)
    cat("\n")
    if (myplayer == "X" | myplayer == "O") {
      name_valid <- FALSE
      return (myplayer)
    } else {
      cat("Invalid selection, please enter either X or O: ", "\n")
      name_valid <- FALSE
    }
  }
}

player = name_player()

if (player == 'X') {
  computer <- 'O'
} else {
  computer <- 'X'
  board = comp_move(computer,board)
}
#player and computer take turns playing till 3 in a row or a tie
repeat { 
  board = player_move(player,board)
  #check if game ends
  if (game_end(board) != "no one") {
    break
  }
  board = comp_move(computer,board)
  #check if game ends
  if (game_end(board) != "no one") {
    break
  }
}
#determine winner/tie
cat("\n", "Ending board: ", "\n")
print(board)
if (game_end(board) == "tie") {
  cat("The game is a ", game_end(board), "\n")
} else {
  cat("The winner is", game_end(board), "\n")
}
