# Because of the note above, during active development of your project, 
# we advise building your project with dune in a polling mode using the option --watch. 
# This rebuilds your project whenever a file is changed in your project.
watch:
	dune build --watch --terminal-persistence=clear-on-rebuild