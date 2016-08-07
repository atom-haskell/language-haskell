all:
	NODE_ENV="dev" apm install
	node ./node_modules/coffee-script/bin/coffee src/haskell.coffee

prelude:
	node ./node_modules/coffee-script/bin/coffee src/makeprelude.coffee
