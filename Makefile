build:
	stack build

run: build
	stack exec vpa-talk-exe
