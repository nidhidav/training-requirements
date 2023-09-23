import pandas as pd
import random

# function to place computer's move
def computer_move(computer, board):
    assigned = False
    while not assigned:
        row = random.randint(0,2)
        col = random.randint(0,2)
        if board.iloc[row,col] == '.':
            board.iloc[row,col] = computer
            assigned = True
    print("\n" + "Computer move placed" + "\n")
    print("Current board: ")
    print(board)
    return board

# helper functions for player_move
# make sure row is a valid integer between 0,1,2
def choosevalidrow():
    valid = False
    while not valid:
        row = input("\n" + "What row? ")
        if not row.isdigit():
            print('row is not a number, choose between 0,1,2')
            continue
        if int(row) not in [0,1,2]:
            print('row is not valid, choose between 0,1,2')
            continue
        valid = True 
    return row
# make sure col is a valid integer between 0,1,2
def choosevalidcol():
    valid = False
    while not valid:
        col = input("What col? ")
        if not col.isdigit():
            print('col is not a number, choose between 0,1,2')
            continue
        if int(col) not in [0,1,2]:
            print('col is not valid, choose between 0,1,2')
            continue
        valid = True 
    return col

# function to make player move
def player_move(symbol, board):
    print(symbol + "'s turn! ")
    valid_values = False
    while not valid_values:
        row = choosevalidrow()
        col = choosevalidcol()
        if board.iloc[int(row),int(col)] != '.':
            print("spot is already taken, choose row and col again")
            continue
        valid_values = True
    board.iloc[int(row),int(col)] = symbol
    print("Move placed.")
    return board

# function to check if game ends
def gameend(board):
    winner = "no one"
    # possible_wins is a list of the values in each column, row, diagonal, and anti-diagonal
    possible_wins = [board.loc["row 0"],board.loc["row 1"],board.loc["row 2"],board["col 0"],board["col 1"],board["col 2"],[board.iloc[0,0],board.iloc[1,1],board.iloc[2,2]],[board.iloc[0,2],board.iloc[1,1],board.iloc[2,0]]]
    for sequence_win in possible_wins:
        if len(set(sequence_win)) == 1 and sequence_win[1] != '.':
            winner = sequence_win[1]
            break
    if '.' not in board.values and winner == "no one":
        winner = "tie"
    return winner

# function to assign player and computer
def name_player():
    while(True):
        symbol = input("X or O? ")
        if (symbol in ["x","X","o","O"]):
            return symbol
        else:
            print("Invalid selection, please enter either X or O: " + "\n")
            
play_game = True
while(play_game):
    # initalize starting board
    start_board = [['.','.','.'],['.','.','.'],['.','.','.']]
    board = pd.DataFrame(data=start_board,columns=["col 0", "col 1", "col 2"],index=["row 0", "row 1", "row 2"])
    print("\nStarting board:" )
    print(board)
    print(" ")

    # assign player and computer symbols
    player = name_player()
    if (player in ['X','x']):
        computer = 'O'
    else:
        computer = 'X'
        board = computer_move(computer, board)

    # player and computer take turns playing till 3 in a row or a tie
    for i in range(9):
        board = player_move(player, board)
        #check if game ends 
        if gameend(board) != "no one":
            break
        board = computer_move(computer, board)
        #check if game ends
        if gameend(board) != "no one":
            break

    # determine winner/tie
    print("Final board: ")
    print(board)
    if gameend(board) == "tie":
        print("The game is a " + gameend(board))
    else:
        print("The winner is " + gameend(board))
    
    #ask if player wants to play again
    response = input("Would you like to play again? Type 'Yes' to play again, or any character to exit game ")
    if (not(response in ['Yes','yes'])):
        print("Thanks for playing!")
        play_game = False