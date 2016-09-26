# Neural Network Minesweeper

An implementation of the the tutorial outlined here: (http://www.ai-junkie.com/ann/evolved/nnt1.html)


## Dependencies

 - SBT
 - JDK 8+
 - Scala 2.11


## Tests

 - All: `sbt test`
 - Individual: `sbt 'test-only org.danielholmes.nnmine.nn.NeuralNetSpec'`
 - Individual continuous: `sbt ~'test-only org.danielholmes.nnmine.nn.NeuralNetSpec'`


## Running

`sbt run`


## Ideas
 
 - Possibly more expensive, but is running in isolation better? Atm their fitness is impacted by other agents getting to 
   mines before them