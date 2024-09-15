# yadl
## Table of Contents
1. [Build Instructions](#build)
  1. [Prerequisites](#pre)
  2. [Building in Terminal/Shell](#build_sh)

### Build Instructions <a name="build"></a>

#### Prerequisites <a name="pre"></a>

- [Scala 3.X](https://www.scala-lang.org/download/)
- Recent Java SDK (openjdk 22 for example)

#### Building in Terminal/Shell <a name="build_sh"></a>

##### Scala

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

The quotes are neccessary here because otherwise they would be interpreted as a new command from sbt.

##### Zig

Run the following commands in the project root.

Just building:
```sh
zig build
```

Running:
```sh
zig build run
```

Running with Program arguments:
```sh
zig build run -- args...
```

##### Building in intellij IDEA

###### Installing Plugins

install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala/) from the
jetbrains marketplace.

###### Setting up build tasks

When you are in a project go to the top-right where you select your current task and chose 'Edit Configurations...' in the drop-down menu.

In the Configuration menu select the `+` to add a new task and chose the 'sbt Task'.

Now you can give the task a meaningful name and pick a task to run (for example `run` or `"run args..."` with arguments) among other settings.

Once done hit 'Apply' or 'OK' to finish the task setup.

Now you should be able to build/run/package/... the project depending on what you chose as a task.

### Unit testing and Python Script testing

#### Unit testing

Similar to building in the terminal you execute the following for the scala unit tests:
```sh
sbt test
```

#### Python Script testing

These tests involve a bit more work to be run.
For the duration of these steps I assume you are at the root of the project.

##### Prerequisites

Install [pytest](https://pypi.org/project/pytest/)

##### Step 1

Similar to building in the terminal you execute the `assembly`-task added by the `project/plugin.sbt` build config:
```sh
sbt assembly
```
This will emit a jar-file which we use in the following steps.

##### Step 2

The python scripts relies on the `YADL_JAR` envirnoment variable to be pointed to the yadl interpreter.

To set the env. var. use:

For Linux and Mac:
```sh
export YADL_JAR=target/scala-3.4.1/yadl.jar
```

For Windows:
```powershell
set YADL_JAR=target/scala-3.4.1/yadl.jar
```
##### Step 3

Finally run pytest:
```sh
pytest
```


