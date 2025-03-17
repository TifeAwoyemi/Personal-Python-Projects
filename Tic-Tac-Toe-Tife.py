# -*- coding: utf-8 -*-
"""
Created on Sat Mar 15 04:57:39 2025

@author: bawoyemi
"""
        
#%%
def display_board(board):
    # The function accepts one parameter containing the board's current status
    # and prints it out to the console.
   
    for i in range(3):
       print(('+' + '-' * 7) * 3 + '+\n' + ('|' + '       ') * 3, end = '|\n|')
       for j in range(3):
           print('  ', board[i][j], '  ', end = '|')
       print()    
       print(('|' + '       ') * 3, end = '|\n')     
    print(('+' + '-' * 7) * 3, end = '+')    
    
# display_board(board)


#%%
def make_list_of_free_fields(board):
    # The function browses the board and builds a list of all the free squares; 
    # the list consists of tuples, while each tuple is a pair of row and 
    # column numbers.
    
    free_squares = []
    for i in range(3):
        for j in range(3):
            if type(board[i][j]) == int:
                free_squares.append((i, j))
    
    return free_squares
                    
# make_list_of_free_fields(board)



#%%
def enter_move(board):
    # The function accepts the board's current status, asks the user about 
    # their move, checks the input, and updates the board according to the 
    # user's decision.
    
    user_move = 'O'
    user_choice = int(input('Enter your move: '))
    free_sq = make_list_of_free_fields(board)
                
    if user_choice < 1 or user_choice > 9 or \
        all_sq[user_choice - 1] not in free_sq:
        
        user_choice = int(input('Make an available move: '))
        
    u_row = all_sq[user_choice - 1][0]
    u_col = all_sq[user_choice - 1][1]
    board[u_row][u_col] = user_move  
    board = board

    board_out = display_board(board)
    
    return board_out         


#%%
def victory_for(board, sign):
    # The function analyzes the board's status in order to check if 
    # the player using 'O's or 'X's has won the game
    
    free_sq = make_list_of_free_fields(board)
                
    comp_win = [sign[0], sign[0], sign[0]]
    user_win = [sign[1], sign[1], sign[1]]
    
    result = ''
    winR1, winR2, winR3, winDiag, winC1, winC2, winC3 = \
        [], [], [], [], [], [], []
    
    for i in range(3): 
        winR1.append(board[0][i])
        winR2.append(board[1][i])
        winR3.append(board[2][i]) 
        winDiag.append(board[i][i])
        winC1.append(board[i][0]) 
        winC2.append(board[i][1])
        winC3.append(board[i][2])
    
    win = [winR1, winR2, winR3, winDiag, winC1, winC2, winC3]       
    
    if comp_win in win and user_win not in win:
        result = 'The computer won!'
           
    elif user_win in win and comp_win not in win:
        result = 'You won!'
           
    elif comp_win and user_win in win:
        result = 'A tie...You both won! '
            
    elif free_sq != []:
        result = ''
    
    else:
        result = 'Game over...No one wins!'
            
    return result

    

#%%

from random import randrange

def draw_move(board):
    # The function draws the computer's move and updates the board.
    
    comp_draw = 0
    first_comp_move = 5
    comp_move = 'X'
    free_sq = make_list_of_free_fields(board)
                
    if all_sq[first_comp_move - 1] in free_sq:
        c_row = all_sq[first_comp_move - 1][0]
        c_col = all_sq[first_comp_move - 1][1]
        board[c_row][c_col] = comp_move 
        board = board
    else:
        comp_draw = randrange(len(free_sq))
        c_row = free_sq[comp_draw][0]
        c_col = free_sq[comp_draw][1]
        board[c_row][c_col] = comp_move 
        board = board
        
    board_out = display_board(board)
    
    return board_out
    

#%%

board = [[3 * i + j + 1 for j in range(3)] for i in range (3)]

all_sq = [(0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), \
          (2, 1), (2, 2)]
    
sign = ['X', 'O']


def play_game(board, sign):
    
    result = victory_for(board, sign)
    draw_move(board)
    print()
    result = victory_for(board, sign)
    check = ''
    if result != check:
        game = result
    else:
        game = enter_move(board)
        print()
        result = victory_for(board, sign)
        if result != check: 
            game = result
        else:
            game = play_game(board, sign)
    print(result)

play_game(board, sign)
    
    












