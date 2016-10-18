# libpopular

libpopular finds the most commonly used packages and functions in Haskell projects. It may help Haskell learners.

## TODO

1. select target Haskell repositories (GitHub Trending crawling or GitHub API)
1. git clone into a temporary directory
1. find symbols in local repos
  1. use `haskell-names` package to resolve unqualified names
  1. stack build
  1. need a symbol list of imported libraries to make an `Environment` (`*.hi` or ghci's `:browse`?)
  1. find package names by module names
  1. handle version and renaming (`as`)
1. clean up local repos
1. print statistics
1. (for the other programming languages)

