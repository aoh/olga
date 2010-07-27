Olgame is currently being rewritten from a set of Unicode console 
games to a slightly more graphical game collection.

Olgame is compiled to a binary which contains all the games. It 
draws graphics using a program called grale, which is also fetched 
and compiled when you type make.

Testing in Debian-based Linux distributions, like Ubuntu:

	$ sudo apt-get install gcc libsdl-dev
	$ make test

If everything seems to work

	$ make install

copies the games to a directory called bin in your home directory.
This directory, if present, is typically automatically added to 
your PATH on login, so you probably can start olgame after that 
by just saying

	$ olgame

To uninstall, issue

	$ make uninstall


Work in progress.
