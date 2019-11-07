## Install pre-requisites

You'll need to install the following pre-requisites in order to build SAFE applications


install [.NET Core SDK](https://www.microsoft.com/net/download) 2.2.402
```
dotnet tool install --global Paket
dotnet tool install fake-cli -g
```
install [Node LTS](https://nodejs.org/en/download/) installed for the front end components.
install [Yarn](https://yarnpkg.com/lang/en/docs/install/) package manager (you an also use `npm` but the usage of `yarn` is encouraged).

## Editor
I like to use VS Code, using the `Ionide` extension
Visual Studio is also an option.

## build.fsx

This contains all the different build commands

To concurrently run the server and the client components in watch mode use the following command:

```
fake build -t Run
```

## Docker

```
fake build -t DockerRun
```

You can use the included `Dockerfile` and `build.fsx` script to deploy your application as Docker container. You can find more regarding this topic in the [official template documentation](https://safe-stack.github.io/docs/template-docker/).

`docker run -d -it -p 8085:8085 kellerd/dpi-reservations`
`docker rmi $(docker images --filter �dangling=true� -q --no-trunc) `


## Azure

Requires three environment variables to be set
DB - InMemory or filepath to simple.db
APPINSIGHTS_INSTRUMENTATIONKEY
public_path - Path to public files relative to server dll

## SAFE Stack Documentation

You will find more documentation about the used F# components at the following places:

* [Giraffe](https://github.com/giraffe-fsharp/Giraffe/blob/master/DOCUMENTATION.md)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)
* [Fable.Remoting](https://zaid-ajaj.github.io/Fable.Remoting/)
* [Fulma](https://fulma.github.io/Fulma/)

If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

## Troubleshooting

* **fake not found** - If you fail to execute `fake` from command line after installing it as a global tool, you might need to add it to your `PATH` manually: (e.g. `export PATH="$HOME/.dotnet/tools:$PATH"` on unix) - [related GitHub issue](https://github.com/dotnet/cli/issues/9321)

# SAFE Template

This template can be used to generate a full-stack web application using the [SAFE Stack](https://safe-stack.github.io/). It was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). If you want to learn more about the template why not start with the [quick start](https://safe-stack.github.io/docs/quickstart/) guide?
