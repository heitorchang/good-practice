# Clojure

Install Java and Maven, and build the release files manually

* Download a Clojure release zip

Run `mvn -Plocal -Dmaven.test.skip=true package`

Copy the outputted `clojure.jar` to somewhere convenient

Run the REPL with `java -jar clojure.jar`

## Emacs

Install `clojure-mode` and `cider`

In a shell, run `lein.bat new PROJECT_NAME`

Open `project.clj`, then `M-x cider-j` (cider-jack-in)
