# Brigadier

A single-header header-only library for command parsing.

## Features

- ✔️ Optional parameters (via `std::optional<T>`)
- ✔️ Command help
- ❌ `std::variant<Ts...>` parameters

## Requirements

* C++17 or newer
* Boost (Boost.CallableTraits)
* fmt (or `<format>`)

## Usage

### Declaring a command tree

Brigadier offers various facilities for creating a command tree. A command tree is constituted of a node containing any number of children nodes. A node can be executable, in which case it may not have subnodes.

Given the sample command `hello world`, `hello` would be a simple node, and `world` would be an executable node:
```cpp
// Registers
//  > hello world
constexpr static const auto tree = Brigadier::Node("hello")
    .Then(
        Brigadier::Command("world", []() noexcept { })
    );
```

If you have multiple commands that share no common node, declare your tree as a tuple:
```cpp
// Registers
//  > hello world
//  > goodbye world
constexpr static const auto tree = std::tuple {
    Brigadier::Node("hello")
        .Then(
            Brigadier::Command("world", []() noexcept { })
        ),
    Brigadier::Node("goodbye")
        .Then(
            Brigadier::Command("world", []() noexcept { })
        )
};
```

### Help

Brigadier offers out-of-the-box support for command help, via `Brigadier::TreeParser<S>::PrintHelp`. Pass in a (potentially) incomplete command input, your tree, and an instance of what Brigadier calls a <span style="color:blue">print listener</span>. A <span style="color:blue">print listener</span> is a type that matches the following scaffolding code:

```cpp
struct PrintListener
{
    void NotifyBeginCommand() { }
    void NotifyEndCommand() { }

    void NotifyLiteral(std::string_view literal) { }
    void NotifyParameter(std::string_view name, bool required) {  }
    void NotifyCommandDescription(std::string_view description) { }

    template <typename T>
    void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) { }
    // -- or -- 
    // void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) { }
    // The former will pass parameter type through T.
};
```

This is especially useful for a typical complex implementation of the `help` command, which Brigadier does not provide for you. It gives access to the complete lexical description of a command, and can leads to cool stuff like this:

![oYafEu9EDC](https://user-images.githubusercontent.com/563936/134428322-ff47065e-4a48-4999-9d0e-6ae3b75fee61.gif)

The astute reader may notice the methods `NotifyParameter`, `NotifyCommandDescription`, and `NotifyParameterDescription`, but `Brigadier::Command` offers no help in this regard. As it turns out, Brigadier allows you to declare commands as "detailed" command nodes, allowing you to provide a description for the command and each of its parameter. If your command is not registered as a detailed command, Brigadier will call `NotifyParameter("parameters", true)`. `NotifyParameterDescription` and `NotifyCommandDescription` will not be called.

### A more detailed command node.

`Brigadier::Command` has an extra overload, which takes in the description of the command, and information about its parameters.

```cpp
        Brigadier::Command(
            "foo",
            /* description */ "This command does X thing",
            [](uint32_t nodeIndex, std::optional<Brigadier::QuotedString> path) noexcept { },
            Brigadier::ParameterMeta<uint32_t> { "nodeIndex", "Description of the 'nodeIndex' parameter" },
            Brigadier::ParameterMeta<std::optional<Brigadier::QuotedString>> { "path", "Description of the 'path' parameter" }
        )
```

With only that information, Brigadier will let you print detailed help prompts to your user.

ℹ Assuming reflection makes it into the language, Brigadier may or may not get updated to support it. No guarantees.

### Parameters

Brigadier offers out-of-the-box support for `(u)int(8|16|32|64)_t`, `float`, `double`, and provides three string-like types: `Brigadier::Word`, `Brigadier::QuotedString` and `Brigadier::GreedyString`. `Word` is effectively `[a-zA-Z]+`. `QuotedString`, on the other hand, allows any character as long as it is enclosed in either single or double quotes, allowing escape sequences. Finally, `GreedyString` is a string to the end of the input.

Brigadier also allows you to implement parsing of your own custom parameter types. Provide an explicit specialization of `Brigadier::_ParameterExtractor`:

```cpp
namespace Brigadier {
    template <>
    struct _ParameterExtractor<MyType, void> {
        static auto _Extract(std::string_view& reader) noexcept -> std::optional<MyType> {
            // IMplementation ...
        }
    };
}
```

Notice that this function must mutate `reader` so that it effectively points to the character after the parameter you're parsing. See the source for examples.

Finally, Brigadier supports all variations of `std::optional<T>` for any of the types above, including your own, with no extra work.

### Parsing

The following sample code shows how to parse a command. `parseResult` will be true if any command was successful in processing the input.

```cpp
int main() {
    S source { };
    bool parseResult = Brigadier::TreeParser<S>::Parse("foo foo 42 \"path\"", tree, source);
}
```

## API

### `Brigadier::Node(std::string_view)`

This creates an object representing a node of the command tree, which typically will hold subnodes.

### `Brigadier::Command<Fn>(std::string_view literal, Fn&& fn)`
### `Brigadier::Command<Fn>(std::string_view literal, std::string_view description, Fn&& fn, parameterInfo...)`

This creates an object representing an execution point of the command tree.
- `literal` The command literal.
- `description` A short description of the command.
- `fn`
  A [Callable](https://en.cppreference.com/w/cpp/named_req/Callable) to be called when the command is selected during parsing.  
  Signature must be akin to `void(Args...) noexcept`.  
  Arguments supported out of the box are `(u)int(64|32|16)_t`, `float`, `double`, `std::string`.  
  Brigadier also exposes `Word`, `GreedyString` and `QuotedString`, which are explicitely convertible to `std::string`.  
  You can also inject `Source const&` or `Source &` at any point in the parameters. Usually, for simplicity, keep it in front.
- `parameterInfo`  
  One or more of `Brigadier::ParameterMeta<T>`. Must match the order and types of parameters of `fn` that are extracted from the command.  
  ⚠️ Types are there to ensure any change to the callback is mirrored on the parameter metadata, and vice-versa.

### Why 'Brigadier' ?

This was ~~stolen from~~ inspired by Mojang's [eponymous library](https://github.com/Mojang/brigadier). 

### I want benchmarks

Here are some completely irrelevant benchmarks, done with [Nanobench](https://github.com/martinus/nanobench), with numbers you should not care about, on a system that is busy doing other things.

|               ns/op |                op/s |    err% |     total | benchmark
|--------------------:|--------------------:|--------:|----------:|:----------
|              225.58 |        4,433,058.50 |    2.2% |      2.68 | `foo foo 42 "bar\"itone"`
|              101.90 |        9,813,696.78 |    2.7% |      1.23 | `foo bar biz 42`
|              156.66 |        6,383,308.66 |    4.1% |      1.88 | `foo bar buz "foo"`
|               56.98 |       17,549,555.56 |    3.8% |      0.69 | `PrintHelp("foo foo", tree, printer)`

The help print test is mostly a no-op (`printer` effectively does nothing) but shows the cost of traversing a simple tree.
