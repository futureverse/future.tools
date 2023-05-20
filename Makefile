include .make/Makefile

spelling:
	$(R_SCRIPT) -e "spelling::spell_check_package()"
	$(R_SCRIPT) -e "spelling::spell_check_files(c('NEWS.md'), ignore=readLines('inst/WORDLIST', warn=FALSE))"
