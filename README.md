# Neural Network Minesweeper

An implementation of the the tutorial outlined here: (http://www.ai-junkie.com/ann/evolved/nnt1.html). It is a 
Feedforward Neural Network with supervised learning (a simulation environment) used for the purpose of collecting mines


## Dependencies

 - SBT
 - JDK 8+
 - Scala 2.11


## Tests

 - All: `sbt test`
 - Individual: `sbt 'test-only org.danielholmes.smartsweepers.nn.NeuralNetSpec'`
 - Individual continuous: `sbt ~'test-only org.danielholmes.smartsweepers.nn.NeuralNetSpec'`


## Running

`sbt run`


## Observations

I've included a few different fitness strategies. They require some fine tuning to the environment - especially the
avoidance fitnesses. For example a fitness to collect mines and avoid rocks - if you punish rock hitting too much then
a spin on the spot strategy is developed (same as the simple avoid only fitnesses)


## TODO

 - Move params.ini and fitness selection into a UI
 - Graph negative fitnesses better
 - Try a mutable Simulation vs immutable/fully functional to see performance differences