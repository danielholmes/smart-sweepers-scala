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


## Ideas
 
 - Possibly more expensive, but is running in isolation better? Atm their fitness is impacted by other agents getting to 
   mines before them