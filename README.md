# *Y*et *A*nother *D*ata *L*anguage
> Simplify Data Workflows by Combining Programming and Data Operations! <br>
> Beyond basic queries, SQL struggles with complex data manipuiation. APIs often return data in JSON format, requiring additional parsing. YADL bridges the gap, offering built-in functionality for both - write less code, analyze more effectively. It's a programming language that allows to parse different file types (json, csv, etc.) with a singe load function and to work with data operations on it.

> *YADL* is a Turing-complete language and was created in a group at university. Thanks to all of them. This repository is a re-upload.

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
while (index < len(bern) and continuous_data) {
    if (bern[index-1]["day"] +1 != bern[index+0]["day"]) {
        continuous_data = false
    }
    
    index = index + 1
}
print3(continuous_data)
```

## Table of Contents
1. [Introduction](#intro)
    1. [Example](#example)
    2. [Anonymous Functions](#anonymous)
    3. [Commong Bugs](#bugs)
2. [Quick Start/Installation](#start)
3. [Build Instructions](#build)
    1. [Prerequisites](#pre)
    2. [Building in Terminal/Shell](#build_sh)
    3. [Building in intellij IDEA](#build_idea)
4. [Testing of Code](#testing)
    1. [Unit testing](#unit)
    1. [Testing with pytest](#python)


## Introduction <a name="intro" />

### Example <a name="example"></a>

Let's examine the example above!
Assuming there is the file `weather-data.json`, this file will use functions, loops and if-statements to analyze the data in the JSON. This is of course only a small demonstration. <br>
Functions, that are not specifically declared in this example are inbuilt functions. For all in-built functions (with description), [click here](https://github.com/julianjumper/yadl/blob/main/spec/stdlib/iterator%20methods.md). <br>
Please keep in mind, that this project was created in a short period of time. This has not reach its full potential. The most important idea we had in mind, is to load the data chunk-wise so that not all data needs to be stored in memory.

### Anonymous functions <a name="anonymous"></a>

We also have anonymous functions! <br>
This code from the example uses one:
```js
has_freezing_days = (city) => {
    return check_any(city, (item) => item["temp"] < 0)
}
print3("Is it freezing?", has_freezing_days(bern)
```
Notice this line: `check_any(city, (item) => item["temp"] < 0)` <br>
Right there, we use the anonymous function `(item) => item["temp"] < 0)` which returns true, if attribute "temp" of the passed object "item" is below 0. <br>
Is this example, it is passed to one of our in-built function from the standard library `check_any` (["see here"](https://github.com/julianjumper/yadl/blob/main/spec/stdlib/iterator%20methods.md) for more information). It is a higher-order function, which takes the anonymous function as an argument. 

But we can also immediately call anonymous functions instead:
```js
print3("2 + 1:", ((a,b) => a+b)(2,1))
```

### Common bugs: <a name="bugs"></a>
- In the example, you might have noticed the `index+0` in the if-statement. This is because the parser expects a value of an operation. I will fix it when I have time! <br>
  btw, `index+0` is an easy fix - in our group we made fun of this bug by using an anonymous identity function and passing the wanted value: `((i) => i)(index)`🤪
- After an if-statement, you have to insert a blank line - also an issue with the parser.

## Quick Start/Installation <a name="start"></a>

### Download JAR

- download the JAR from this GitHub repository

### Running a `.yadl` file
Run this command:
`java -jar <path-to-jar> <path-to-yadl>`
path-to-jar is the path to the downloaded jar-file and path-to-yadl the path to your yadl program. You can download and use the test file `fancy-tests/wather-data.yadl`.

## Build Instructions <a name="build"></a>

### Prerequisites <a name="pre"></a>

- [Scala 3.X](https://www.scala-lang.org/download/)
- Recent Java SDK (openjdk 22 for example)

### Building in Terminal/Shell <a name="build_sh"></a>

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

### Building in intellij IDEA <a name="build_idea"></a>

#### Installing Plugins

install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala/) from the
jetbrains marketplace.

#### Setting up build tasks

When you are in a project go to the top-right where you select your current task and chose 'Edit Configurations...' in the drop-down menu.

In the Configuration menu select the `+` to add a new task and chose the 'sbt Task'.

Now you can give the task a meaningful name and pick a task to run (for example `run` or `"run args..."` with arguments) among other settings.

Once done hit 'Apply' or 'OK' to finish the task setup.

Now you should be able to build/run/package/... the project depending on what you chose as a task.

### Unit testing and Python Script testing <a name="testing"></a>

#### Unit testing <a name="unit"></a>

Similar to building in the terminal you execute the following for the scala unit tests:
```sh
sbt test
```

#### Python Script testing <a name="python"></a>

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


