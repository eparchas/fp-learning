# The problem

You need to go through a room with a number of detectors, whose efficacy of detecting a moving object is a function of the distance of that object with the detector. Your goal is to find, with the highest precision possible, the path across the room with the lowest probability of detection.

Write a program that, given the description of the room, including your starting position and the ending position, produces a single floating point number as output. This floating point number represents the lowest probability of detection achievable given the number and location of the detectors, rounded to the 3rd decimal digit.

## Detectors:
For any object at distance D from the detector, the probability of detection per meter is given by:
```
exp(-(π*D/L)^2)
```
where `L` is the width of the room (see below) and `x^2` is the square of `x`.

# Input

The main input to the program is a `*.map` file, which has the following format:

The first line is a single floating point number L, representing the length of the room, which is assumed to always be a square.
The second line is a single integer `N`, giving the number of detectors in the room.
All following lines in the file are coordinates, i.e. pairs of floating point numbers, separated by a space character. The number of coordinates in the file is given by `N`.
The coordinate system is x, y based, with the origin located in the bottom left corner of the room. Coordinates never include negative numbers.

Starting position is assumed to be always the exactly midway accross the bottom edge of the room.

Ending position is assumed to be exactly midway accross the top edge of the room.

### Example input file:

```
25.0
5
2.423929917008996 20.187139309438546
19.39788132776695 14.174570106439353
1.3175678970133191 10.019351994529405
1.0536920857525445 2.8936703202385115
16.739302303324447 15.87541372165791
```

Feel free to create more input files for testing purposes.

### Example usage
The program should be used as follows:

```
$ calculate room.map
0.245
```

The program should return exit status 0 if the hypothetical room.map file was parsed successfully. Returns exit status 1 otherwise.

# Running instructions
Make sure you have Nix or NixOS installed
```
curl -L https://nixos.org/nix/install | sh
```

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
> mvn clean install
[...]
> ./calculate sample.map
0.736
```