# Smart Sweepers Scala

Attempt to port (http://www.ai-junkie.com/ann/evolved/nnt1.html) C++ solution to Scala

All class/struct, property and variable names are kept consistent with the C++ source to try and ensure an exact port
of functionality and to make it easier to follow the tutorial and code.

Rendering is pretty different though as it won't use the windows APIs: this mainly affects CController and Main.


## Dependencies

 - Scala 2.11+
 - Gradle


## Running

`./sbt run`