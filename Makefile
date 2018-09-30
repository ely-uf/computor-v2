NAME = computor-v2
SRC = main.hs

LOCAL_SRC = ArithmeticExpressionSolver.hs
PARSER_SRC = Parsers/GenericParsers.hs Parsers/TNumParser.hs \
			 Parsers/AExpressionParser.hs Parsers/VariableParser.hs
TYPES_SRC = Types/ArithmeticExpression.hs \
			Types/ComputorState.hs Types/TNum.hs

AUX_SRC = $(LOCAL_SRC) $(PARSER_SRC) $(TYPES_SRC)

OBJ = $(SRC:.hs=.o) $(SRC:.hs=.hi) $(AUX_SRC:.hs=.o) $(AUX_SRC:.hs=.hi)

all: $(NAME)

$(NAME):
	ghc $(SRC) -o $(NAME)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all
