This code exists to conduct a study on password memorability. It contains
an app designed to collect keystroke data during a sequence of spaced
repetition sessions.

In order to run it, you'll need to supply a few things that don't appear
in the repo;

- salt.txt : some server-specific sequence of bytes used in hashing to
   map userids to training strings
- local-config.rkt : a few server-specific paths
- users.rktd : file with hashed passwords. E.G.:
((foo ("bar"))
 (baz ("quux")))
