.DEFAULT_GOAL := help
.PHONY: help day1 day2 day3 day4

help:
	$(info Advent of code 2024)
	$(info elm-live is required (npm i --global elm-live))
	$(info make dayX to run a day)

day1: ## Run elm-live for day1 
	cd day1 && elm-live src/Main.elm --start-page=Main.html -- --output=elm.js

day2: ## Run elm-live for day2
	cd day2 && elm-live src/Main.elm --start-page=Main.html -- --output=elm.js

day3: ## Run elm-live for day3
	cd day3 && elm-live src/Main.elm --start-page=Main.html -- --output=elm.js

day4: ## Run elm-live for day4
	cd day4 && elm-live src/Main.elm --start-page=Main.html -- --output=elm.js

