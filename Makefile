MAKE_OPTS:= --no-builtin-rules

submake: Makefile.coq
	$(MAKE) $(MAKE_OPTS) -f Makefile.coq $(MAKECMDGOALS)

Makefile.coq: _CoqProject
	$(COQBIN)coq_makefile -f $< -o $@

%:: submake ;

# known sources

Makefile: ;

_CoqProject: ;
