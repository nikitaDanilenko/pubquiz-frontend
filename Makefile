path=~/quizzes/

all: mkFolder mkInput mkOutput cpStyles

mkFolder:
	mkdir -p $(path)

mkInput:
	elm make src/QuizInput.elm --output=input/input.js
	sudo cp -r input/ $(path)

mkOutput:
	elm make src/QuizOutput.elm --output=quizzes/output.js
	sudo cp -r quizzes/ $(path)

cpStyles:
	sudo cp -r styles/ $(path)