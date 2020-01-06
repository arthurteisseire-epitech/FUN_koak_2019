##
## EPITECH PROJECT, 2019
## koak
## File description:
## Makefile
##

all:
	stack build
	stack install --local-bin-path ./

tests_run:
	stack test

clean:
	stack clean
	rm koak
