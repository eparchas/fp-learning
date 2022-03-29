# Ghost Game
Ghost is a two-person word game where players alternate appending letters to a word. The first person who spells out a word, or creates a prefix for which there is no possible continuation, loses. Here is a sample game:

- Player 1: `g`
- Player 2: `h`
- Player 1: `o`
- Player 2: `s`
- Player 1: `t` [loses]

Given a dictionary of words, determine the letters the first player should start with, such that with optimal play they cannot lose.

For example, if the dictionary is `["cat", "calf", "dog", "bear"]`, the only winning start letter would be `b`.

# Parsing
We can assume that the dictionary of words is provided as a command line argument in a file where every line contains a word.

```
cat
calf
dog
bear
```

# Notes
Whether player A will be able to win depends largely on the dictionary provided. It is quite possible that no words exist to ensure that player A can win. Also, it is possible that a player can win in multiple ways, as it is possible to require more than one starting letters to ensure that he can win. For this reason, the return type of the `calculateFirstLetters` function is a List of Strings. Each string contains only the letters that player A will need to play in order to win, however this can be easily changed to return the play of both players so far.

# Running Instructions
## Nix
Make sure you have Nix or NixOS installed

Add support for Nix Flakes or update your existing nix config accordingly.

```
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

Run the following to enter the appropriate shell
```
nix develop
```

In the resulting shell you can build and run the code
```
stack run dict.txt
```
