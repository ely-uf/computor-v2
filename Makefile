NAME = computor-v2
SRC = main.hs GenericParsers.hs ArithmeticExpression.hs TNum.hs TNumParser.hs \
	  AExpressionParser.hs VariableParser.hs ComputorState.hs
OBJ = $(SRC:.hs=.o) $(SRC:.hs=.hi)

all: $(NAME)

$(NAME):
	ghc $(SRC) -o $(NAME)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all
