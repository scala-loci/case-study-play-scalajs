# Play Framework with Scala.js Showcase
###Scala: 2.11.8, Scala.js: 0.6.15, Play: 2.5.14, Slick: 3.1.1

#### Update: July 2016 
This project was started on Oct 2014 when Scala.js was still an experimental project. I didn't expect Scala.js to come this far because there were a lot of Scala projects that were discontinued. Surprisingly, it's not just still alive but keeps growing. 
I have to catch up what I missed for almost 2 years, and start looking for something cool to learn as usual.(Stay hungry, still foolish LoL)  
I'm looking forward to using this: https://github.com/japgolly/scalajs-react/tree/topic/neo   
The new version I plan in my head is to use React and Microservice. 

___

This is a small application showing how cool Scala.js is. You can share code across platforms. For the shared code,
the Scala compiler compiles your Scala code to JVM byte code, and the ScalaJS compiler compiles the same Scala code to JavaScript.
Beside CSS and a few lines of HTML, almost all code in this project are type-safety including HTML (Thanks to [scalatags](https://github.com/lihaoyi/scalatags)). I made this project for learning purpose during my summer intern.
So, the code you will see in this project may look not good, but I hope you can learn something from it like I did.
~~I will try to keep it update-to-date to make sure that it will run with the recent version of Scala.js.~~

The sbt build file contains 3 modules
- `exampleServer` Play application (server side)
- `exampleClient` Scala.js application (client side)
- `exampleShared` Scala code that you want to share between the server and the client.    

This project would not exist if I didn't find this kick-ass example 
[play-with-scalajs-example](https://github.com/vmunier/play-with-scalajs-example).

The project contains 4 simple examples:

- Todo application with backend persistence.(Modified from [Todo application](http://lihaoyi.github.io/workbench-example-app/todo.html))
  - InMemory, Slick, Anorm, Gremlin (Selectable in application.conf)
- Hangman (Inspired by [Yii's demo](http://www.yiiframework.com/demos/hangman/))
- HTML5 Fileupload (Modified from [How to Use HTML5 File Drag and Drop](http://www.sitepoint.com/html5-file-drag-and-drop/))
- Server Push Chat. It supports both Websocket and Server-Sent Event

### Prerequisite steps
Before running the application, you may need to do these steps:
1. Add the folowing to ~/.sbt/0.13/plugins/plugins.sbt

    addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")

### Run the application
```
$ sbt
> run
$ open http://localhost:9000
```

### Run the application on heroku

- Install the [heroku toolbelt](https://devcenter.heroku.com/articles/getting-started-with-scala#set-up)
- Run following commands:
``` 
$ heroku login
$ heroku create
$ git push heroku master
$ heroku open
```

## Features

The application uses the [sbt-play-scalajs](https://github.com/vmunier/sbt-play-scalajs) sbt plugin and the [play-scalajs-scripts](https://github.com/vmunier/play-scalajs-scripts) library.

- Run your application like a regular Play app
  - `compile` simply triggers the Scala.js compilation
  - `run` triggers the Scala.js fastOptJS command on page refresh
  - `~compile`, `~run`, continuous compilation is also available
  - `start`, `stage` and `dist` generate the optimised javascript
  - [`playscalajs.html.scripts`](https://github.com/vmunier/play-with-scalajs-example/blob/c5fa9ce35954278bea903823a7f0528b1d68b5db/server/app/views/main.scala.html#L14) selects the optimised javascript file when the application runs in prod mode (`start`, `stage`, `dist`).
- Source maps
  - Open your browser dev tool to set breakpoints or to see the guilty line of code when an exception is thrown
  - Source Maps is _disabled in production_ by default to prevent your users from seeing the source files. But it can easily be enabled in production too by setting `emitSourceMaps in fullOptJS := true` in the Scala.js projects.
