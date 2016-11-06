# How to build

* Install npm
* Install bower and pulp via npm
* Run `npm install react react-dom` in the frontend directory
* Run `bower install` in the frontend directory
* Run `pulp browserify --to dist/app.js` in the frontend directory
* `cd ..` (central-counter directory)
* `stack build`
* `stack exec central-counter`
* Visit http://localhost:8081/index.html in multiple browser windows and click `+` and `-`.
* Have fun!

Note that this example is setup to work with the `master` branch of `purescript-servant`.
If you're using `stack` or copying this as a template, make sure you're using the right version!
