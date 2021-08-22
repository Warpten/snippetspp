# Brigadier

A simple command parser.

## Requirements

* C++17 or newer
* Boost (Boost.CallableTraits, Boost.Range)
* fmt

## Usage

```cpp
Brigadier::CommandDispatcher<Source, uint32_t> dispatcher;
dispatcher.Then("foo", [](Source const& source, uint32_t value) noexcept {
    return 0; 
})
.Then("bar", [](uint32_t a, float b) noexcept {
    return 1;
})
.Then("biz", [](uint32_t a, Brigadier::Word const& b) noexcept {
    return 2;
});
```

The above code registers 3 commands:

```
> foo $x
> foo bar $x $y
> foo bar biz $x $y
```

At its core, a command dipatcher (`Brigadier::CommandDispatcher`) is a syntax tree. Each node has an associated literal,
and each node is capable of interpreting a given number of arguments.

## Features

- ✔️ Optional parameters (via `std::optional<T>`)
- ❌ Redirects
- ❌ Command help

## API

### `CommandDispatcher<S, R>`
- `S` An object used to pass feedback of a command's execution. This would be your interface display for a game.
- `R` The type of a command's result code. This is `uint32_t` if unspecified.

- `CommandDispatcher<S, R>::Then(std::string_view literal, Callable&& fn)`
  `CommandDispatcher<S, R>::Then(std::string_view literal)`
  - `literal`: the literal to look for in a command.
  - `fn`: The [Callable](https://en.cppreference.com/w/cpp/named_req/Callable) to be called when the command is selected during parsing.  
     Signature must be akin to `R(Args...) noexcept`.  
     Arguments supported out of the box are `(u)int(64|32|16)_t`, `float`, `double`, `std::string`.  
     Brigadier also exposes `Word`, `GreedyString` and `QuotedString`, which are explicitely convertible to `std::string`.  
     You can also inject `Source const&` at any point in the parameters. Usually, for simplicity, keep it in front.
  Returns the node created, which is an instance of `LiteralTreeNode<S, R>`.

- `R CommandDispatcher<S, R>::Parse(S const& source, std::string_view string)`

  `R CommandDispatcher<S, R>::Parse(S const& source, StringReader& reader)`
  - `source`: The source object.
  - `string` / `reader`: The input to parse.
  Returns the result code of the command.
  Throws `std::logic_error` if the command could not be parsed.

### `LiteralTreeNode<S, R>`
- `S` An object used to pass feedback of a command's execution. This would be your interface display for a game.
- `R` The type of a command's result code. This is `uint32_t` if unspecified.
- `LiteralTreeNode<S, R>& LiteralTreeNode<S, R>::Executes(F fn)`
  Sets the [Callable](https://en.cppreference.com/w/cpp/named_req/Callable) to be called when the command is selected during parsing.
  This should only be called if you called the overload of `Then` that does not accept a callable and you want this to be a command and not a simple node.
- `LiteralTreeNode<S, R>& LiteralTreeNode<S, R>::Then(std::string const& literal, Callable&& fn)`

  `LiteralTreeNode<S, R>& LiteralTreeNode<S, R>::Then(std::string const& literal)`
  - `literal`: the literal to look for in a command.
  - `fn`: The [Callable](https://en.cppreference.com/w/cpp/named_req/Callable) to be called when the command is selected during parsing.  
     Signature must be akin to `R(Args...) noexcept`.  
     Arguments supported out of the box are `(u)int(64|32|16)_t`, `float`, `double`, `std::string`.  
     Brigadier also exposes `Word`, `GreedyString` and `QuotedString`, which are explicitely convertible to `std::string`.  
     You can also inject `Source const&` at any point in the parameters. Usually, for simplicity, keep it in front.
  Returns the child node created, which is an instance of `LiteralTreeNode<S, R>`.

### `ArgumentType<T>`

This object is the main extendability point of Brigadier. It allows you to define your own parameter types and how they should be parsed. The contract for this object looks as follows:

```cpp
template <typename T>
struct ArgumentType {
    using type = T;
    constexpr static const bool optional = false; // This is needed for handling std::optional<T> parameters, but is done out of the box. Leave as is.
  
    static Errorable<type> Read(StringReader& reader);
};
```
