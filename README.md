# msbuild

[![Build Status](https://ci.appveyor.com/api/projects/status/github/TerrorJack/msbuild?branch=master&svg=true)](https://ci.appveyor.com/project/TerrorJack/msbuild?branch=master)

Utilities for working with [MSBuild](https://github.com/Microsoft/msbuild) in Haskell. Currently supports:

* Querying MSBuild installation info
* Running commands in MSBuild command prompts
* Compiling a single C++ source file to object file
* Linking object files to a DLL file

See the [test script](test/msbuild-test.hs) for an example of compiling a C++ source file and loading a function on runtime.

You can install MSBuild with [Build Tools for Visual Studio 2017](https://www.visualstudio.com/downloads/#build-tools-for-visual-studio-2017).

## Motivation

> Why did it turn out like this...?
>
> I fell for Haskell for the very first time.
>
> And made a friend with Windows.
>
> These two wonderful things joined together.
>
> The joy from the two,
>
> Thus, brought me even more happiness.
>
> Like something out a beautiful dream, that's what it was supposed to be...
>
> But then, why... Did it turn out like this...?
