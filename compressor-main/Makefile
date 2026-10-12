##
## EPITECH PROJECT, 2022
## Makefile
## File description:
## Makefile
##

SRC =			src/*.hs

BIN = $(shell stack path --local-install-root)/bin/imageCompressor-exe
NAME = 			imageCompressor

all:			$(NAME)

$(NAME) :
	stack build && cp $(BIN) . && mv imageCompressor-exe $(NAME)

clean:
			stack clean

fclean:			clean
			rm -f $(NAME)

re:			fclean all
