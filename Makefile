##
## EPITECH PROJECT, 2020
## No Project
## File description:
## Makefile
##

PROJET	=	hal-project

NAME	=	hal

RM		=	rm -f

all:	$(NAME)

$(NAME):
	stack build
	cp `stack path --local-install-root`/bin/$(PROJET)-exe ./$(NAME)

clean:
	$(RM) $(NAME)

fclean:	clean

re:		fclean all

.PHONY: all clean fclean re watch tests
