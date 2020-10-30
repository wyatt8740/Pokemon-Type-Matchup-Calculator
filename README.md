# Pokémon type matchup calculator for generations II through V

This is my first time ever really finishing something I started writing in
Common Lisp. As such, it's not really 'great' code quality. There are several
places I really should define and use functions - places where some code is
duplicated. If I come back to this, I'll probably do some tweaks like that and
input validation. But it works!

Note: if your input is not what my code expects, the program will return
"normally effective" (1.00x) power due to it assuming that it's some "non-game"
type, like shadow type in Colosseum/XD or the ???? type. After all, this was
pretty much for private use - but I felt proud enough of figuring out Common
Lisp that I decided to share.

The type match-up data in the code was translated from the ROM data of
Pokémon: Crystal Version.

All in all, this was just an excuse for me to use a Lisp - it just seemed like
the kind of problem that Lisps would be suited to solving (association lists
made it pretty simple), and I feel like I learned a lot in the process.

Tested in SBCL and in GNU Clisp, both running on Debian Linux.
