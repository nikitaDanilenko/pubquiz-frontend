# Pubquiz Front End

This is a front end application for [the corresponding back end](https://github.com/nikitaDanilenko/pubquiz-server).

## Structure

The application is separated in two main parts:
one public part for viewing the statistics of any given quiz,
and one back office part for managing quizzes, their teams, and points.

### Public Part

The public part has a hierarchy, which can be viewed in any order:
1. The list of all quizzes
2. The details of a quiz
3. The details of a team at a given quiz

Depending on the entry point, one can navigate top down (from all quizzes, to a particular team of a particular quiz)
or bottom up (from a team, to the quiz, to all quizzes).
The first approach is the common pattern when viewing quizzes in the past.
The second approach is the common pattern during a quiz,
because the quiz sheets contain links to those team pages,
from where participants can navigate to the more general pages.

### Back Office Part

The back office provides an interface for managing quizzes.
The lifecycle of a quiz is typically as follows:
1. Creation, a couple of days to hours before the quiz
2. Point management, during the quiz
3. Locking, immediately after the quiz

Once a quiz is locked, only the administrator account can unlock it.
In practice, this is not a hard constraint, because usually everything is final at the end of the quiz.
The current setup is more of an exploration (learning experience).

### Back Office Access

There are only two fixed accounts for the back office:
1. One adminstrator account (can unlock quizzes)
2. One organizer account

This is a pragmatic choice, because there is usually no need for more accounts.
Even if someone else needs access, the password can be shared easily,
and it is just as easy to change the password if necessary (deployment variable).

### Quiz Sheets

For each quiz there is a quiz sheet that anyone can access.
When accessed during the quiz, the team names are already filled in,
but this just a gimmick.
The quiz sheets are just pretty HTML pages, which can be printed out nicely.
The approach is very light-weight, because there is no actual file generation anywhere.

## Development

The application requires a running back end to work.
There is an included `dev.docker-compose.yaml` file for easy development.
The development setup does not contain any quizzes,
but it is easy enough create them via the application itself.

### Open API

The application is built upon the Open API specification of the back end.
The `elm-open-api` package is used to generate the API client,
and that client is used where necessary.
Parts of the generated code are unused intentionally,
either because there is no need (encoder/decoder parity),
or the API is not used.

## Deployment

There is a [deployment directory](./deployment) in this repository.
It contains a subset of Helm templates that are necessary to deploy
_the entire application_, i.e. including the back end.
As such, the templates are incomplete, and focus only on the front end.
An example of a complete deployment can be viewed in my
[GitOps repository](https://github.com/nikitaDanilenko/git-ops/tree/main/pubquiz).
