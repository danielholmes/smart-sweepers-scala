# Neural Network Minesweeper

An implementation of the the tutorial outlined here: (http://www.ai-junkie.com/ann/evolved/nnt1.html). It is a 
Feedforward Neural Network with supervised learning (a simulation environment) used for the purpose of collecting mines


## Dependencies

 - SBT
 - JDK 8+
 - Scala 2.11


## Tests

 - All: `sbt test`
 - Individual: `sbt 'test-only org.danielholmes.nnmine.nn.NeuralNetSpec'`
 - Individual continuous: `sbt ~'test-only org.danielholmes.nnmine.nn.NeuralNetSpec'`


## Running

`sbt "run-main org.danielholmes.smartsweepers.Main"`

A UI matching the supplied/original solution is also available:

`sbt "run-main org.danielholmes.smartsweepers.OldMain"`

- Press **F** to toggle between viewing the current sweepers at real time and a sped up evolution only
- Press **R** to reset


## Ideas
 
 - Possibly more expensive, but is running in isolation better? Atm their fitness is impacted by other agents getting to 
   mines before them