all:
	(cd src && $(MAKE) all)
	(cd c_src && $(MAKE))

clean:
	(cd src && $(MAKE) clean)
	(cd c_src && $(MAKE) clean)
