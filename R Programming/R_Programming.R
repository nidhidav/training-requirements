# function to get user input
if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

# function to place computer's move
comp_move <- function(value, myboard) {
  assigned <- FALSE
  while (!assigned) {
    row <- sample(1:3, 1)
    col <- sample(1:3, 1)
    if (myboard[row, col] == "NA") {
      myboard[row, col] <- value
      assigned <- TRUE
    }
  }
  cat("\n", "Computer move placed", "\n", "Current board:", "\n")
  print(myboard)
  cat(" \n")
  return(myboard)
}

# function to ask for row and check if valid
ask_row <- function() {
  valid <- FALSE
  while (!valid) {
    cat("What row? ")
    row_pos <- readLines(con <- con, n <- 1)
    row_pos <- as.numeric(row_pos)
    if (!(row_pos %in% c(1:3))) {
      cat("Row value is not valid, please select from 1, 2, or 3", " \n")
    } else {
      valid <- TRUE
    }
  }
  return(row_pos)
}

# function to ask for col and check if valid
ask_col <- function() {
  valid <- FALSE
  while (!valid) {
    cat("What col? ")
    col_pos <- readLines(con <- con, n <- 1)
    col_pos <- as.numeric(col_pos)
    if (!(col_pos %in% c(1:3))) {
      cat("Col value is not valid, please select from 1, 2, or 3", " \n")
    } else {
      valid <- TRUE
    }
  }
  return(col_pos)
}

# function to place player's move
player_move <- function(value, myboard) {
  cat(value, "'s turn!", "\n")
  # check if viable position
  valid_pos <- FALSE
  while (!valid_pos) {
    row_pos <- ask_row()
    col_pos <- ask_col()
    if (myboard[row_pos, col_pos] == "NA") {
      valid_pos <- TRUE
      myboard[row_pos, col_pos] <- value
    } else {
      cat("That place is already taken, please re-enter the row and col", "\n")
    }
  }
  cat("Move placed", "\n")
  return(myboard)
}

# function to check if game ends
game_end <- function(myboard) {
  winner <- "no one"
  # possible_wins is a list of the values in each column, row, diagonal, and anti-diagonal
  possible_wins <- list(
    myboard$"col 1", myboard$"col 2", myboard$"col 3", unlist(myboard[1, ], use.names = FALSE), unlist(myboard[2, ], use.names = FALSE), unlist(myboard[3, ], use.names = FALSE),
    c(myboard[1, 1], myboard[2, 2], myboard[3, 3]), c(myboard[1, 3], myboard[2, 2], myboard[3, 1])
  )
  # loops through all sequences in possible_wins, winner is assigned if there are three in a row
  for (sequence_win in possible_wins) {
    if (length(unique(sequence_win)) == 1 & sequence_win[1] != "NA") {
      winner <- sequence_win[1]
      break
    }
  }
  # check tie - no NA in the board
  if (!any(myboard == "NA") & winner == "no one") {
    winner <- "tie"
  }
  return(winner)
}

# function to assign player and computer
name_player <- function() {
  cat("X or O? ")
  while (TRUE) {
    myplayer <- readLines(con <- con, n <- 1)
    cat("\n")
    if (myplayer %in% c("x", "X", "o", "O")) {
      return(myplayer)
    } else {
      cat("Invalid selection, please enter either X or O: ", "\n")
    }
  }
}

play_game <- TRUE
while (play_game) {
  # build board
  board <- data.frame(
    c("NA", "NA", "NA"),
    c("NA", "NA", "NA"),
    c("NA", "NA", "NA")
  )
  rownames(board) <- c("row 1", "row 2", "row 3")
  colnames(board) <- c("col 1", "col 2", "col 3")
  cat("\n", "Starting board: ", "\n")
  print(board)

  # assign player and computer symbols
  player <- name_player()
  if (player == "X" | player == "x") {
    computer <- "O"
  } else {
    computer <- "X"
    board <- comp_move(computer, board)
  }

  # player and computer take turns playing till 3 in a row or a tie
  for (val in c(1:9)) {
    board <- player_move(player, board)
    # check if game ends
    if (game_end(board) != "no one") {
      break
    }
    board <- comp_move(computer, board)
    # check if game ends
    if (game_end(board) != "no one") {
      break
    }
  }

  # determine winner/tie
  cat("\n", "Ending board: ", "\n")
  print(board)
  if (game_end(board) == "tie") {
    cat("The game is a ", game_end(board), "\n")
  } else {
    cat("The winner is", game_end(board), "\n")
  }

  # ask if player wants to play again
  cat("Would you like to play again? Type 'Yes' to play again, or any character to exit game ", "\n")
  response <- readLines(con <- con, n <- 1)
  if (!(response %in% c("Yes", "yes"))) {
    cat("Thanks for playing! ", "\n")
    play_game <- FALSE
  }
}
