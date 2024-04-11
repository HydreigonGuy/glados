##
## EPITECH PROJECT, 2022
## glados
## File description:
## Makefile
##

EXEC_PATH	=	$(shell stack path --local-install-root)

NAME		=	glados

.PHONY: clean fclean re all

$(NAME):
	stack build .
	cp ${EXEC_PATH}/bin/${NAME}-exe ./$(NAME)

clean:
	stack clean

fclean:
	stack clean --full
	${RM} ${NAME}

re: fclean all

all: $(NAME)
