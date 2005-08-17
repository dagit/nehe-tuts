GHC = ghc
lessons = lesson*.hs

default: $(lessons)

usage:
	@echo "Usage: make lessonXY"
	@echo "  Or"
	@echo "Usage: make lessonXY.hs"
	@echo "Current lessons available are:" $(lessons)

.PHONY: $(lessons)

#this is nice so you can type "make *"
$(lessons):
	$(GHC) -O2 --make $@ -o $(subst .hs,,$@)

#with this rule you can type "make lessonXY"
% :: %.hs
	$(GHC) -O2 --make $< -o $@

clean:
	rm -rf *.o *.hi

distclean:
	rm -rf *.o *.hi lesson[0-9][0-9]