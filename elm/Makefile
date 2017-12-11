all: Native/Blake2s1.js

Native/blake2s1.js:
	curl -L -o Native/blake2s1.js https://github.com/imuli/treehash/raw/master/blake2s1.js

Native/Blake2s1.js: Native/Blake2s1.rest.js Native/blake2s1.js
	cat Native/blake2s1.js Native/Blake2s1.rest.js > Native/Blake2s1.js

clean:
	rm -f Native/blake2s1.js Native/Blake2s1.js
