all: cl-twit.info manual

cl-twit.info: cl-twit.texinfo
	makeinfo cl-twit.texinfo

manual: cl-twit.texinfo
	makeinfo --html -o manual cl-twit.texinfo