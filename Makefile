NAME = computor-v2
SRC = main.hs

LOCAL_SRC = ArithmeticExpressionSolver.hs FunctionOperations.hs \
			ComputorStateOperations.hs VariableAssignment.hs	\
			Types.hs PredefinedFunctions.hs MatrixOperations.hs
PARSER_SRC = Parsers/GenericParsers.hs Parsers/TNumParser.hs \
			 Parsers/AExpressionParser.hs Parsers/VariableAssignmentParser.hs \
			 Parsers/FunctionParser.hs

OPERATIONS_SRC = Operations/ComputorState.hs Operations/Function.hs Operations/Matrix.hs


AUX_SRC = $(LOCAL_SRC) $(PARSER_SRC) $(OPERATIONS_SRC)

OBJ = $(SRC:.hs=.o) $(SRC:.hs=.hi) $(AUX_SRC:.hs=.o) $(AUX_SRC:.hs=.hi)

all: $(NAME)

$(NAME):
	ghc $(SRC) -o $(NAME)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re: fclean all
