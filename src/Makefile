PROJECT=Compilo.native

project:
	make clean_project
	ocamlbuild $(PROJECT)

clean_project:
	rm -f *.o
	rm -f *.byte
	rm -rf _build
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(PROJECT)
	rm -f $(PROJECT).opt

clean:
	make clean_project

exec_project:
	./$(PROJECT)
