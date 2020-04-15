path=~/quizzes/

all: mkFolder mkInput mkOutput cpStyles

mkFolder:
	mkdir -p $(path)

mkInput:
	elm make src/QuizInput.elm --output=input/input.js
	cp -r input/ $(path)

mkOutput:
	elm make src/QuizOutput.elm --output=quizzes/output.js
	cp -r quizzes/ $(path)

cpStyles:
	cp -r styles/ $(path)