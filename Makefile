GHC = ghc
FLAGS = -Wall -Werror -O2
lessons = lesson*.hs

default: $(lessons)

usage:
	@echo "Usage: make lessonXY"
	@echo "  Or"
	@echo "Usage: make lessonXY.hs"
	@echo "Current lessons available are:" $(lessons)

#this is nice so you can type "make *"
$(lessons):
	$(GHC) $(FLAGS) --make $@ -o $(subst .hs,,$@)

#with this rule you can type "make lessonXY"
% :: %.hs
	$(GHC) $(FLAGS) --make $< -o $@

clean:
	rm -rf *.o *.hi

distclean:
	rm -rf *.o *.hi lesson[0-9][0-9]