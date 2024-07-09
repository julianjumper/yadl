# yadl

## Build Instructions

### Prerequisites

- [Scala 3.X](https://www.scala-lang.org/download/)
- Recent Java SDK (openjdk 22 for example)

### Building in Terminal/Shell

Run the following commands in the project root.

Just building:
```sh
sbt compile
```

Running:
```sh
sbt run
```

Running with Program arguments:
```sh
sbt "run args..."
```

Running the interpretation tests:
```sh
sbt assembly
export YADL_JAR=/path/to/file/created/by/sbt/assembly
pytest
```

The quotes are neccessary here because otherwise they would be interpreted as a new command from sbt.

### Building in intellij IDEA

#### Installing Plugins

install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala/) from the
jetbrains marketplace.

#### Setting up build tasks

When you are in a project go to the top-right where you select your current task and chose 'Edit Configurations...' in the drop-down menu.

In the Configuration menu select the `+` to add a new task and chose the 'sbt Task'.

Now you can give the task a meaningful name and pick a task to run (for example `run` or `"run args..."` with arguments) among other settings.

Once done hit 'Apply' or 'OK' to finish the task setup.

Now you should be able to build/run/package/... the project depending on what you chose as a task.
