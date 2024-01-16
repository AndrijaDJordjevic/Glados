##
## EPITECH PROJECT, 2023
## makefile
## File description:
## makefile
##

PATH_BINARY	=	$(shell stack path --local-install-root)

TEST_PATH	=	./test/coverage/

NAME	=		glados

NAME_VM	=	vm

BENCHMARK_TEST	=	$(NAME)-exe

STACK_BUILD	=	stack build

STACK_BUILD_PROFILE	=	stack build --profile

STACK_EXEC_PROFILE	=	stack exec --profile --

IMAGE_NAME = image-$(NAME)

FLAG_OTHER_USER = --allow-different-user

all:
	$(STACK_BUILD) $(FLAG_OTHER_USER)
	cp $(PATH_BINARY)/bin/$(NAME)-exe ./$(NAME)

build_vm: clean_vm
	$(STACK_BUILD) $(FLAG_OTHER_USER)
	cp $(PATH_BINARY)/bin/$(NAME_VM)-exe ./$(NAME_VM)
	mv vm/$(NAME_VM)-exe .
	chmod +x $(NAME_VM)-exe

benchmark:
	$(STACK_BUILD_PROFILE) $(FLAG_OTHER_USER)
	cp $(PATH_BINARY)/bin/$(NAME)-exe .
	$(STACK_EXEC_PROFILE) $(BENCHMARK_TEST) +RTS -p

tests: clean
	$(STACK_BUILD) $(FLAG_OTHER_USER)
	stack test --coverage
	stack hpc report --all --destdir $(TEST_PATH)

clean_vm:
	$(RM) $(NAME_VM)-exe

clean:
	stack clean $(FLAG_OTHER_USER)

fclean: clean
	$(RM) $(NAME)
	$(RM) $(NAME)-exe
	$(RM) myvm
	$(RM) *.prof
	$(RM) *.hi

cleanall: fclean
	$(RM) -r $(TEST_PATH)

re:	fclean all

build:
	docker build -t $(IMAGE_NAME) .

run:
	@docker run -it $(IMAGE_NAME) /bin/bash

.PHONY: all benchmark tests clean fclean re build run
