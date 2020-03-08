##
## EPITECH PROJECT, 2019
## cpp_rush3_2019
## File description:
## automated desc ftw
##

NAME = wolfram
SRC = $(shell find ./app ./src -name '*.hs')

$(NAME): all

all: $(SRC)
	stack install wolfram:exe:wolfram-exe --local-bin-path '.'
	mv wolfram-exe wolfram -f

tests_run:
	stack install wolfram:test:wolfram-test --local-bin-path '.'

clean: fclean

fclean: ./.stack-work
	rm -rf .stack-work tags TAGS

re:
	$(MAKE) fclean
	$(MAKE)

.PHONY: clean re all fclean
