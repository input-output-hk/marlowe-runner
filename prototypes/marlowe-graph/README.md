# marlowe-graph

This project is compiled down to the `MarloweGraph.js` file which is referenced directly in the runner project as foreign module. It is not really a prototype any more ;-)

## Development

Project is self contained to simplify development and testing. It has it's own webpack.js so you can use regular devel flow. On the application level it has some example config and renders graph for it.

Whenever you finish development please run `$ tsc` from the top level of the project to actually generate the final module.
