### The rules of our flavor of the Pacman game

At the start of the game, the board looks like this:

```
       (a1) (a2) (a3) (a4) (a5 Pacman)
       (b1)                (b5)
       (c1)                (c5)
       (d1)                (d5)
       (e1) (e2) (e3) (e4) (e5)
       (f1)                (f5)
       (g1)                (g5)
       (h1)                (h5)
 (Ghost i1) (i2) (i3) (i4) (i5)
```

As you can see, the board is shaped like the digit "8".

<table>
<tr>
<td valign="top">Pacman moves back and forth along an "S"-shaped path, reversing direction at each end, at a speed of one board position per second.</td>
<td valign="top">Ghost moves continuously along a big-"O"-shaped path, in a counterclockwise fashion, at a speed of one board position per second.</td>
</tr>
<tr>
<td valign="top">At the start of the game, Pacman is at board position "a5" and moves to the left.</td>
<td valign="top">At the start of the game, Ghost is at board position "i1" and moves to the right.</td>
</tr>
<tr>
<td valign="top">If Pacman is either in the same position as Ghost or in his immediate vicinity when Ghost turns red (toxic), Pacman dies and the game is over.<br>
(Yes, Pacman and Ghost can be in the same board position at the same time.)</td>
<td valign="top">Ghost is blue (harmless) most of the time, but turns red (toxic) for a duration of exactly one second every x number of seconds (x is an integer obtained from user input at the start of the game).</td>
</tr>
<tr>
<td valign="top">Pacman collects points by eating pellets.</td>
<td valign="top">Ghost generates a pellet and leaves it behind every time he turns red (toxic). If a pellet already exists at that position, Ghost does not generate a new one.</td>
</tr>
</table>

Pacman moves back and forth along this path:

```
P P P P P
P       •
P       •
P       •
P P P P P
•       P
•       P
•       P
P P P P P
```

Ghost moves continuously along this path:

```
G G G G G
G       G
G       G
G       G
G • • • G
G       G
G       G
G       G
G G G G G
```

### The problem statement

Design a Haskell function called "pacman" that takes an integer x (the length of time in seconds between when Ghost turns red/toxic) and produces an infinite list out of which the number of points collected / pellets eaten by Pacman throughout the entire game, can be extracted via "takeWhile" or "until".

