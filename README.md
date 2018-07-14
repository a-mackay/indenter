# Source Code Indenter

## Building

1. Install Haskell Cabal.
1. Clone this repo.
1. ```> cd path/to/git-cloned/folder```
1. ```> cabal new-build```

## Usage

To change a 2-space indented file to a 4-space indented file:

```> indenter SomeFile.java OutputFile.java 2 4```

If any lines have unexpected amounts of indentation, ```indenter``` will not create the output file. Instead, it will print the lines numbers of the lines with unexpected amounts of indentation.