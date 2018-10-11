# hanjie3

Program to solve Hanjie logic problems, to be run from the Clojure REPL

## Installation

Download from http://example.com/FIXME.

## Usage

### Start up a REPL ###
```

Starting nREPL server...
/usr/local/share/JetBrains/Toolbox/apps/IDEA-U/ch-0/182.4505.22/jre64/bin/java -Dfile.encoding=UTF-8 -XX:-OmitStackTraceInFastThrow -Dclojure.compile.path=/home/ben/dev/clojure/hanjie3/target/default/classes -Dhanjie3.version=0.1.0-SNAPSHOT -Dclojure.debug=false -javaagent:/usr/local/share/JetBrains/Toolbox/apps/IDEA-U/ch-0/182.4505.22/lib/idea_rt.jar=34919:/usr/local/share/JetBrains/Toolbox/apps/IDEA-U/ch-0/182.4505.22/bin -classpath /home/ben/dev/clojure/hanjie3/test:/home/ben/dev/clojure/hanjie3/src:/home/ben/dev/clojure/hanjie3/dev-resources:/home/ben/dev/clojure/hanjie3/resources:/home/ben/dev/clojure/hanjie3/target/default/classes:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-util/1.8/batik-util-1.8.jar:/home/ben/.m2/repository/quil/processing-js/1.6.4.0/processing-js-1.6.4.0.jar:/home/ben/.m2/repository/org/clojure/tools.logging/0.4.0/tools.logging-0.4.0.jar:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-svggen/1.8/batik-svggen-1.8.jar:/home/ben/.m2/repository/quil/quil/2.6.0/quil-2.6.0.jar:/home/ben/.m2/repository/com/lowagie/itext/2.1.7/itext-2.1.7.jar:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-css/1.8/batik-css-1.8.jar:/home/ben/.m2/repository/org/bouncycastle/bcmail-jdk14/1.38/bcmail-jdk14-1.38.jar:/home/ben/.m2/repository/quil/processing-dxf/3.2.4/processing-dxf-3.2.4.jar:/home/ben/.m2/repository/org/clojure/tools.nrepl/0.2.12/tools.nrepl-0.2.12.jar:/home/ben/.m2/repository/xml-apis/xml-apis/1.3.04/xml-apis-1.3.04.jar:/home/ben/.m2/repository/clojure-complete/clojure-complete/0.2.4/clojure-complete-0.2.4.jar:/home/ben/.m2/repository/xalan/xalan/2.7.0/xalan-2.7.0.jar:/home/ben/.m2/repository/org/clojure/tools.trace/0.7.9/tools.trace-0.7.9.jar:/home/ben/.m2/repository/bouncycastle/bcprov-jdk14/138/bcprov-jdk14-138.jar:/home/ben/.m2/repository/quil/processing-svg/3.2.4/processing-svg-3.2.4.jar:/home/ben/.m2/repository/org/bouncycastle/bctsp-jdk14/1.38/bctsp-jdk14-1.38.jar:/home/ben/.m2/repository/org/clojure/clojure/1.9.0/clojure-1.9.0.jar:/home/ben/.m2/repository/bouncycastle/bcmail-jdk14/138/bcmail-jdk14-138.jar:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-dom/1.8/batik-dom-1.8.jar:/home/ben/.m2/repository/org/clojure/math.combinatorics/0.1.4/math.combinatorics-0.1.4.jar:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-awt-util/1.8/batik-awt-util-1.8.jar:/home/ben/.m2/repository/org/clojure/test.check/0.9.0/test.check-0.9.0.jar:/home/ben/.m2/repository/org/clojure/core.specs.alpha/0.1.24/core.specs.alpha-0.1.24.jar:/home/ben/.m2/repository/quil/processing-pdf/3.2.4/processing-pdf-3.2.4.jar:/home/ben/.m2/repository/quil/gluegen-rt-fat/2.3.2/gluegen-rt-fat-2.3.2.jar:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-ext/1.8/batik-ext-1.8.jar:/home/ben/.m2/repository/xml-apis/xml-apis-ext/1.3.04/xml-apis-ext-1.3.04.jar:/home/ben/.m2/repository/quil/processing-core/3.2.4/processing-core-3.2.4.jar:/home/ben/.m2/repository/org/apache/xmlgraphics/batik-xml/1.8/batik-xml-1.8.jar:/home/ben/.m2/repository/quil/jogl-all-fat/2.3.2/jogl-all-fat-2.3.2.jar:/home/ben/.m2/repository/org/clojure/spec.alpha/0.1.143/spec.alpha-0.1.143.jar:/home/ben/.m2/repository/org/bouncycastle/bcprov-jdk14/1.38/bcprov-jdk14-1.38.jar clojure.main -i /tmp/form-init239937415827300863.clj
Connecting to local nREPL server...
Clojure 1.9.0
nREPL server started on port 35747 on host 127.0.0.1 - nrepl://127.0.0.1:35747
(require '[hanjie3.redux :as hr])
=> nil
(require '[hanjie3.examples :as ex])
=> nil
(hr/pprint-hanjie (hr/iterate-then-guess (hr/init-redux ex/umbrella)))
"Elapsed time: 31.083714 msecs"
"Elapsed time: 5.48303 msecs"
"Elapsed time: 1.652137 msecs"
"Elapsed time: 1.995666 msecs"
"Elapsed time: 1.11241 msecs"
"Elapsed time: 3.329104 msecs"
"Elapsed time: 0.88381 msecs"
"Elapsed time: 0.906984 msecs"
       O       
     OOOOO     
    OOOOOOO    
   OOOOOOOOO   
  OOOOOOOOOOO  
 OOOOOOOOOOOOO 
OOOOOOOOOOOOOOO
 O O O O O O O 
       O       
       O       
       O       
   O   O       
   O   O       
   OO OO       
    OOO        

=> nil

```

You can invoke the permutation function from the repl
like this
```
(seq (hr/multi-permute-eduction hr/constant-progressive-filter-fn 10 6))
=>
([0 1 1 1 1 6]
... 462 permutations later
[6 1 1 1 1 0])
```

You can build a pixel line from a set of blocks and space like this
```
(vec (hr/build-pixelline 15 [1 1 1 1] [0 2 3 4 2]))
=>
[true false false true false false false true false false false false true false false]
```

You can accumulate pixel certainties like this
```
(-> (reduce hr/accumulate-pixels
            (hr/new-pixel-accumulator 15)
            [(boolean-array [false true false false true false false false true false false false false true false])
             (boolean-array [true true false false true false false false true false false false false true false])])
    (update :on vec)
    (update :counter deref))
=>
#hanjie3.redux.PixelAccumulator{:on [1 2 0 0 2 0 0 0 2 0 0 0 0 2 0], :counter 2}
```
It is useful to know the numbers between zero (all off)
and counter (all on) for the situations where we need to guess a pixel

you can solve the trickiest hanjie i've found thus
```
(time (hr/pprint-hanjie (hr/iterate-then-guess (hr/init-redux ex/peter))))
...

     OOOOOOOO           
      OOO O     O        
      OO      OOO        
     OO             OO   
    OO           OO   O  
    OO   O      OOO  O O 
   OOO               OOOO
    OO O      X    OOOOO 
     OOO      OOO OO OO O
     O  O        OO  O   
     O O        OO   O  O
       OO   O        O  O
        OOO O           O
      OO    O           O
      OO   O     O O   OO
      OOO OOO    O     OO
      OOOOOO   OOOO   OOO
      OOOOOOOOOOO    OOOO
       OOOO          O OO
        OOO         O O O
         OOO       OO OO 
       OOOO O        OOOO
    OOOOO OO         OOOO
 OOOOOOO  OOOO      OOOOO
OOOOOOOO   OO      OOOOOO

"Elapsed time: 9389.830679 msecs"
=> nil
```
Note the 'X' that implies a contradiction; 
this hanjie does not appear to be perfect

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
