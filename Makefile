##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile
##

NAME	=	imageCompressor

all:
	make $(NAME)

$(NAME):
	cd Compressor ; stack build --copy-bins --local-bin-path ../
	cd ../
	mv Compressor-exe imageCompressor

clean :
	cd Compressor ; stack clean
	rm -rf imageCompressor

fclean: clean

re: fclean all

.PHONY:	all clean fclean re $(NAME)
