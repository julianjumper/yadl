# *Y*et *A*nother *D*ata *L*anguage
Simplify Data Workflows by Combining Programming and Data Operations! <br>
Designed to work with data stream operations directly on common data formats such as JSON and CSV. <br>
*YADL* is a Turing-complete language and was created in a group at university. Thanks to all of them. This repository is a re-upload.

## Example

```js
weather_data = load("./weather-data.json", "json") // open json file

bern = weather_data["bern"] // extract "bern" object

// example of a function (fat-arrow style)
has_freezing_days = (city) => {
    return check_any(city, (item) => item["temp"] < 0)
}

print3("Has Bern freezing days?: " + has_freezing_days(bern))
print3("Has Bern not freezing days?: " + check_all(bern, (item) => item["temp"] < 0)) // use function in print-statement
print3("Is Bern the best city?: idk, im a computer")

// find continuous data with a while-loop and if/else
print3("Has Bern continuous data?:")
index = 1
continuous_data = true
while (index < len(bern) && continuous_data) {
    if (bern[index-1]["day"] +1 != bern[index]["day"]) {
        continuous_data = false
    }
    index = index + 1
}
print3(continuous_data)
```
Assuming there is the file `weather-data.json`, this file will use functions, loops and if-statements to analyze the data in the JSON. This is of course only a small demonstration. <br>
Functions, that are not specifically declared in this example are inbuilt functions. For all in-built functions (with description), [click here](spec/stdlib/iterator methods.md). <br>
Please keep in mind, that this project was created in a short period of time. This has not reach its full potential. The most important idea we had in mind, is to load the data chunk-wise so that not all data needs to be stored in memory.

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


