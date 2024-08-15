# This is just an example Makefile to show how to build the individual components easily
# This is *NOT* robust, see the hardcoded filenames in `clean`

all: libbabybear.a libsphinxgnark.a Main

libbabybear.a:
	cd babybear; cargo build --release
	cp babybear/target/release/libbabybear.a ./

libsphinxgnark.a:
	git submodule update --init --recursive
	cd sphinx/recursion/gnark-ffi/go; CGO_ENABLED=1 go build -buildmode=c-archive -o ../../../../libsphinxgnark.a .

Main:
	ghc --make -main-is Main -o Main Main.hs libbabybear.a libsphinxgnark.a

clean:
	rm -f libbabybear.a libsphinxgnark.a libsphinxgnark.h Main Main.hi Main.o

# This ensures we always try to rebuild those targets
.PHONY: libbabybear.a libsphinxgnark.a Main clean all
