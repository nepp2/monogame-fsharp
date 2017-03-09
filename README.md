# snakes.com

A clone of [slither.io](http://slither.io/)

Written for an game AI module at Goldsmiths. I cloned it so I could try writing a bot for it.

I ran out of time before I could try anything silly with machine learning. Instead I wrote some steering behaviours (eat, attack, flee), and extracted a bunch of parameters to govern how they are prioritised and how they work. The parameters are evolved over time. Breeding pairs are chosen from the recently deceased snakes, based on how big they managed to get before they died.

If you leave it running for a long time the snakes currently converge on extremely cautious behaviour, because I haven't balanced the game very well. There's too much food lying on the ground, and the attack behaviour is too crude to work well. Still, it's a cool screen-saver.

# Controls

By default the game just chooses a snake to follow until it dies.

* **Space** - toggle fast mode (game will run at max simulation speed to speed up evolution)
* **Enter** - take control of a snake
* **Mouse** - the snake follows the mouse pointer
* **Left Click** - move faster, at the cost of some energy

# Running the thing

There are still some hard-coded directory paths which might only work if you run the game from visual studio.

---

**Why on earth is this written in mutation-heavy F#, and rendered entirely using an unofficial, debug DrawRect method?**

Uhh. Type annotations are for losers, and sprites are bourgeois.
