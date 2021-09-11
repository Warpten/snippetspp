/* 
 * This file is part of the snippetspp distribution (https://github.com/Warpten/snippetspp).
 * Copyright (c) 2021 Warpten.
 * 
 * This program is free software: you can redistribute it and/or modify  
 * it under the terms of the GNU General Public License as published by  
 * the Free Software Foundation, version 3.
 *
 * This program is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

static_assert(__cplusplus >= 201703L, "Brigadier only supports C++17 and upwards.");

#if __has_include(<format>) && __cplusplus >= 202002L && defined(__cpp_lib_format)
# include <format>
# define BRIGADIER_FORMAT_NAMESPACE std
#elif __has_include(<fmt/format.h>)
# include <fmt/format.h>
# define BRIGADIER_FORMAT_NAMESPACE fmt
#else
static_assert(false, "Brigadier requires std::format or fmt::format");
#endif

#include <array>
#include <cassert>
#include <charconv>
#include <optional>
#include <functional>
#include <string_view>
#include <type_traits>
#include <unordered_map>

#include <boost/algorithm/string.hpp>
#include <boost/callable_traits/args.hpp>
#include <boost/callable_traits/is_noexcept.hpp>
#include <boost/callable_traits/return_type.hpp>

#if __cplusplus >= 202002L
namespace Brigadier::Details
{
    template <typename T>
    using remove_cvref = std::remove_cvref<T>;

    template< class T >
    using remove_cvref_t = typename remove_cvref<T>::type;
}
#else
namespace Brigadier::Details
{
    template <typename T>
    struct remove_cvref {
        typedef std::remove_cv_t<std::remove_reference_t<T>> type;
    };

    template< class T >
    using remove_cvref_t = typename remove_cvref<T>::type;
}
#endif

namespace Brigadier {
    template <typename T, typename Enable = void> struct _ParameterExtractor;
    template <typename T, typename... Ts> struct Tree;
    template <typename N, typename P> struct TreePath;
    template <typename Fn> struct CommandNode;
    template <typename Fn, typename... Ts> struct DetailedCommandNode;

    struct BareNode;

#if __cpp_concepts >= 201907L
    template <typename T>
    concept HelpPrinter = requires (const T& t, std::string_view sv, bool b) {
        { t.NotifyBeginCommand() };
        { t.NotifyEndCommand() };
        { t.Notifyliteral(sv) };
        { t.NotifyPaameter(sv, b) };
        { t.NotifyCommandDescription(sv) };
        { t.NotifyParameterDescription(sv, sv, b) };
    };
#else
    struct HelpPrinter {
        virtual ~HelpPrinter() { }

        virtual void NotifyBeginCommand() = 0;
        virtual void NotifyEndCommand() = 0;

        virtual void NotifyLiteral(std::string_view literal) = 0;
        virtual void NotifyParameter(std::string_view name, bool required) = 0;
        virtual void NotifyCommandDescription(std::string_view description) = 0;
        virtual void NotifyParameterDescription(std::string_view name, std::optional<std::string_view> description, bool required) = 0;
    };
#endif

    namespace Details {
        // ---- IsBareNode ----
        namespace {
            template <typename T>
            using IsBareNode = std::is_same<T, BareNode>;
        }

        // ---- IsSimpleCommandNode ----
        namespace {
            template <typename T> struct IsSimpleCommandNode : std::false_type { };
            template <typename Fn> struct IsSimpleCommandNode<CommandNode<Fn>> : std::true_type { };
        }

        // ---- IsDetailedCommandNode ----
        namespace {
            template <typename T> struct IsDetailedCommandNode : std::false_type { };
            template <typename Fn, typename... Ts> struct IsDetailedCommandNode<DetailedCommandNode<Fn, Ts...>> : std::true_type { };
            template <typename T> struct IsDetailedCommandNode<T const&> : IsDetailedCommandNode<T> { };
            template <typename T> struct IsDetailedCommandNode<T&> : IsDetailedCommandNode<T> { };
        }

        // ---- IsNode ----
        namespace {
            template <typename T> struct IsNode {
                enum { value = IsDetailedCommandNode<T>::value || IsSimpleCommandNode<T>::value || IsBareNode<T>::value };
            };
        }

        // ---- IsTree ----
        namespace {
            template <typename T> struct IsTree : std::false_type { };
            template <typename T, typename... Ts> struct IsTree<Tree<T, Ts...>> : std::true_type { };
            template <typename T> struct IsTree<T const&> : IsTree<T> { };
            template <typename T> struct IsTree<T&> : IsTree<T> { };
        }

        // ---- TupleSize ----
        // Like std::tuple_size but handles empty tuples properly
        namespace {
            template <typename T> struct TupleSize;
            template <typename... Ts> struct TupleSize<std::tuple<Ts...>> { constexpr static const size_t value = sizeof...(Ts); };
            template <> struct TupleSize<std::tuple<>> { constexpr static const size_t value = 0; };
        }
        
        // ---- ValidateTree ----
        namespace {
            template <typename U, typename T>
            struct ValidateTree : std::true_type { };

            template <typename U, typename T, typename... Ts>
            struct ValidateTree<U, Tree<T, Ts...>> {
                enum { value = sizeof...(Ts) == 0 ? true : (std::is_same_v<U, Ts> && ...) };
            };
        }

        // ---- is_optional
        namespace {
            template <typename T> struct is_optional : std::false_type { };
            template <typename T> struct is_optional<std::optional<T>> : std::true_type { };
            template <typename T>
            constexpr static const bool is_optional_v = is_optional<T>::value;
        }

        /// <summary>
        /// Iterates over the provided tuple.
        /// </summary>
        /// <param name="tuple">The tuple to iterate over</param>
        /// <param name="fn">A callable of signature R(auto, Args...) where auto will be the tuple member.</param>
        /// <param name="condition">A Callable of signature bool(R). If it returns true, iteration is aborted early. This is basically an exit condition</param>
        /// <param name="args">Extra arguments to pass to fn</param>
        template <
            size_t I,
            typename Tuple, 
            typename Fn,
            typename Condition,
            typename... Args>
        constexpr static auto IterateUntil(
            Tuple&& tuple,
            Fn&& fn,
            Condition&& condition,
            Args&&... args) noexcept
        {
            auto result { fn(std::get<I>(tuple), std::forward<Args&&>(args)...) };
            if constexpr (I + 1 == std::tuple_size<std::decay_t<Tuple>>::value)
                return result;
            else {
                if (condition(result))
                    return result;
                else {
                    return IterateUntil<I + 1>(std::forward<Tuple&&>(tuple),
                        std::forward<Fn&&>(fn),
                        std::forward<Condition&&>(condition),
                        std::forward<Args&&>(args)...
                    );
                }
            }
        }

        template <
            size_t I,
            typename Tuple, 
            typename Fn,
            typename... Args>
        constexpr static auto Iterate(Tuple&& tuple,
            Fn&& fn,
            Args&&... args) noexcept
        {
            if constexpr (std::tuple_size<std::decay_t<Tuple>>::value > 0) {
                fn(std::get<I>(tuple), std::forward<Args&&>(args)...);
                if constexpr (I + 1 != std::tuple_size<std::decay_t<Tuple>>::value)
                    Iterate<I + 1>(std::forward<Tuple&&>(tuple),
                        std::forward<Fn&&>(fn),
                        std::forward<Args&&>(args)...
                    );
            }
        }

        // ---- std::is_reference_wrapper ----
        namespace {
            template <typename T> struct is_reference_wrapper : std::false_type { };
            template <typename T> struct is_reference_wrapper<std::reference_wrapper<T>> : std::true_type { };

            template <typename T>
            constexpr static const bool is_reference_wrapper_v = is_reference_wrapper<T>::value;
        }

        // ---- ParameterIsInput ----
        // Determines wether a parameter is parsed from the command text.
        namespace {
            template <typename T, typename Enable = void> struct ParameterIsInput {
                enum { value = false };
            };

            template <typename U>
            static std::true_type test(decltype(&_ParameterExtractor<U>::_Extract));
            static std::false_type test(...);

            template <typename T>
            struct ParameterIsInput<T, std::enable_if_t<decltype(test<T>(0))::value>>
            {
                enum { value = true };
            };
            template <typename T>
            struct ParameterIsInput<std::optional<T>> {
                enum { value = ParameterIsInput<T>::value };
            };
        }

        // ---- FilterHandlerParams ----
        // Filters types of a tuple based on a conditional, returning a reduced tuple.
        namespace {
            template <template <class...> typename Conditional, typename Tuple> struct FilterHandlerParams;
            template <template <class...> typename Conditional, typename T, typename... Ts>
            struct FilterHandlerParams<Conditional, std::tuple<T, Ts...>> {
                using type = std::conditional_t<Conditional<T>::value,
                    decltype(
                        std::tuple_cat(
                            std::declval<std::tuple<T>>(),
                            std::declval<typename FilterHandlerParams<Conditional, std::tuple<Ts...>>::type
                            >()
                        )
                    ),
                    typename FilterHandlerParams<Conditional, std::tuple<Ts...>>::type>;
            };
            template <template <class...> typename Conditional>
            struct FilterHandlerParams<Conditional, std::tuple<>> {
                using type = std::tuple<>;
            };
        }

        // ---- ValidateParameterInfo ----
        // Validations parameter information
        namespace {
            template <typename Fn, typename... Ts>
            struct ValidateParameterInfo
            {
                using source_tuple = typename FilterHandlerParams<
                    ParameterIsInput,
                    boost::callable_traits::args_t<Fn>
                >::type;
                using target_tuple = std::tuple<Ts...>;

                enum {
                    value = std::is_same_v<source_tuple, target_tuple>
                };
            };
        }
    }

    template <typename T, typename... Ts>
    struct Tree {
        using value_type = T;

        T _value;
        std::tuple<Ts...> _children;

        template<class V, class... Args>
        explicit constexpr Tree(V&& value, Args&&... children) noexcept
            : _value(std::forward<V>(value)), _children(std::forward<Args>(children)...)
        { }

        explicit constexpr Tree(T&& value, Ts&&... children) noexcept
            : _value(std::forward<T&&>(value)), _children(std::forward<Ts>(children)...)
        { }

        template <typename... Us>
        constexpr Tree<T, Ts..., Us...> And(Us&&... children) {
            return std::make_from_tuple<Tree<T, Ts..., Us...>>(
                std::tuple {
                    _value,
                    std::tuple_cat(_children, std::tuple { std::forward<Us&&>(children)... })
                }
            );
        }
    };

    inline constexpr struct TEmpty { } Empty;

    template<class ValueT, class... Children>
    Tree(ValueT&&, Children&&... c) -> Tree<Details::remove_cvref_t<ValueT>, Details::remove_cvref_t<Children>...>;

    struct String {
        String() noexcept {
            _val = nullptr;
            _sz = 0;
            _owning = false;
        }

        explicit String(std::string_view sv) noexcept {
            _val = const_cast<char*>(sv.data());
            _sz = sv.length();
            _owning = false;
        }

        explicit String(const std::string& str) noexcept
        {
            _val = new char[str.length()];
            std::memcpy(_val, str.data(), str.length());
            _sz = str.length();
            _owning = true;
        }

        String(String&& other) noexcept
        {
            _val = other._val;
            _sz = other._sz;
            _owning = other._owning;

            other._val = nullptr;
            other._owning = false;
            other._sz = 0;
        }

        String(String const& other) noexcept
        {
            if (other._owning)
            {
                _val = new char[other._sz];
                _sz = other._sz;
                std::memcpy(_val, other._val, other._sz);
                _owning = true;
            }
            else
            {
                _val = other._val;
                _sz = other._sz;
                _owning = false;
            }
        }

        ~String() noexcept {
            if (_owning)
                delete[] _val;

            _val = nullptr;
            _sz = 0;
            _owning = false;
        }

        operator const std::string_view() const noexcept { return std::string_view { _val, _sz }; }

    private:
        char* _val;
        size_t _sz;
        bool _owning;
    };

    enum class ValidationResult : uint8_t {
        Failure,
        Children,
        Execute
    };

    struct QuotedString : String { using String::String; };
    struct Word : String { using String::String; };
    struct GreedyString : String { using String::String; };

    template <typename T>
    struct _ParameterExtractor<T, std::enable_if_t<std::is_integral_v<T>>> {
        constexpr static const T Default { };

        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<T>
        {
            T value;
            auto result = std::from_chars(
                reader.data(),
                reader.data() + reader.size(),
                value, 
                10);

            bool success = result.ec == std::errc { };
            if (success) {
                reader = result.ptr;
                return value;
            }

            return std::nullopt;
        }
    };

    template <typename T>
    struct _ParameterExtractor<T, std::enable_if_t<std::is_floating_point_v<T>>> {
        constexpr static const T Default { };

        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<T>
        {
            T value;
            auto result = std::from_chars(
                reader.data(),
                reader.data() + reader.size(),
                value,
                std::chars_format::general | std::chars_format::scientific | std::chars_format::fixed
            );

            bool success = result.ec == std::errc { };
            if (success) {
                reader = result.ptr;
                return value;
            }

            return std::nullopt;
        }
    };

    template <>
    struct _ParameterExtractor<Word, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<Word>
        {
            auto pos = reader.find(' ', 0);
            if (pos == std::string_view::npos)
                return Word { reader };

            if (pos == 0)
                return std::nullopt;

            return Word { reader.substr(0, pos) };
        }
    };

    template <>
    struct _ParameterExtractor<GreedyString, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<GreedyString>
        {
            return GreedyString { reader };
        }
    };

    template <>
    struct _ParameterExtractor<QuotedString, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<QuotedString>
        {
            bool success = (reader[0] == '"' || reader[0] == '\'');
            if (!success)
                return std::nullopt;

            auto delim = reader[0];
            auto searchIndex = 1;

            bool escapes = false;

            while (true) {
                auto pos = reader.find(delim, searchIndex);
                success = pos != std::string_view::npos;
                if (!success)
                    return std::nullopt;

                // Escaped, keep searching
                if (reader[pos - 1] == '\\' && reader[pos - 2] != '\\') {
                    escapes = true;
                    searchIndex = pos + 1;
                    continue;
                } else {
                    // Found end quote
                    searchIndex = pos;
                    break;
                }
            }

            std::string_view data { reader.substr(1, searchIndex - 1) };
            reader = reader.substr(searchIndex + 1);

            // Remove escape sequences, keep quotes
            if (escapes)
            {
                std::string owningCopy { data };
                boost::erase_all(owningCopy, "\\");

                return QuotedString { std::move(owningCopy) };
            }

            return QuotedString { data };
        }
    };

    struct BareNode {
        constexpr BareNode(std::string_view const literal) noexcept : _literal(literal) { }

        constexpr BareNode(BareNode const&) = default;
        constexpr BareNode(BareNode&&) = default;

        constexpr ValidationResult Validate(std::string_view& reader) const noexcept {
            bool success = reader.find(_literal) == 0;
            
            if (success) {
                reader = reader.substr(_literal.length());
                return ValidationResult::Children;
            }

            return ValidationResult::Failure;
        }

        template <typename... Ts>
        constexpr Tree<BareNode, Ts...> Then(Ts&&... parameters) const noexcept {
            return Tree { *this, std::forward<Ts&&>(parameters)... };
        }

        std::string_view literal() const { return _literal; }


    private:
        std::string_view _literal;
    };

    template <typename Fn>
    struct CommandNode {
        constexpr CommandNode(std::string_view literal, Fn&& fn) noexcept : _literal(literal), _fn(fn) {
            static_assert(boost::callable_traits::is_noexcept_v<Fn>, "Function handlers must be noexcept.");
        }

        constexpr CommandNode(CommandNode<Fn> const&) = default;
        constexpr CommandNode(CommandNode<Fn>&&) noexcept = default;

        //> Returns true if execution succeeded, false otherwise.
        template <typename S>
        constexpr auto TryExecute(std::string_view const& reader, S& source) const noexcept
            -> bool
        {
            if (reader.find(_literal) != 0)
                return false;

            auto mutableReader = reader.substr(_literal.length());

            using args_t = boost::callable_traits::args_t<Fn>;

            bool success = true;
            auto args { _ExtractParameters<S>(source, success, mutableReader, static_cast<args_t*>(nullptr)) };

            if (success)
                std::apply(_fn, std::move(args));

            return success;
        }

        constexpr std::string_view literal() const { return _literal; }

    private:
        std::string_view _literal;
        Fn _fn;

        template <typename S, typename... Ts>
        constexpr auto _ExtractParameters(S& source, bool& success, std::string_view& reader, std::tuple<Ts...>* tpl = nullptr) const {
            return std::tuple {  _ExtractParameter<S, Ts>(source, reader, success)... };
        }

        template <typename S, typename T>
        constexpr auto _ExtractParameter(S& source, std::string_view& reader, bool& success) const noexcept {
            
            using decayed_type = std::decay_t<T>;

            if constexpr (std::is_same_v<T, S&>) {
                return std::ref(source);
            } else if constexpr (std::is_same_v<T, S const&>) {
                return std::cref(source);
            } else if constexpr (std::is_same_v<T, CommandNode<Fn>&>) {
                return std::ref(*this);
            } else if constexpr (std::is_same_v<T, CommandNode<Fn> const&>) {
                return std::cref(*this);
            } else if constexpr (Details::is_optional_v<T>) {
                static_assert(Details::ParameterIsInput<decayed_type>::value, "Missing implementation of _ParameterExtractor");

                using underlying_type = typename decayed_type::value_type;

                // If we already failed, fail fast
                if (success) {
                    std::string_view copy { reader.substr(1) };
                    auto result { _ParameterExtractor<underlying_type>::_Extract(copy) };
                    if (result.has_value())
                        reader = copy;

                    return result;
                }

                return std::optional<underlying_type> { };
            } else {
                // If we already failed, fail fast
                if (!success)
                    return decayed_type { };

                static_assert(Details::ParameterIsInput<decayed_type>::value, "Missing implementation of _ParameterExtractor");

                reader = reader.substr(1);
                auto result { _ParameterExtractor<decayed_type>::_Extract(reader) };
                if (result.has_value())
                    return result.value();

                success = false;
                return decayed_type { };
            }
        }
    };
    
    template <typename U>
    struct ParameterMeta {
        using type = U;

        constexpr static const bool Required = !Details::is_optional_v<U>;

        explicit constexpr ParameterMeta(std::string_view const name, std::string_view const description) noexcept : _name(name), _description(description) { }
        explicit constexpr ParameterMeta(std::string_view const name) noexcept : _name(name) { }

        constexpr const std::string_view name() const { return _name; }
        constexpr std::optional<const std::string_view> const& description() const noexcept { return _description; }

    private:
        const std::string_view _name;
        std::optional<const std::string_view> _description = std::nullopt;
    };

    template <typename Fn, typename... Ts>
    struct DetailedCommandNode : CommandNode<Fn> {
        using expected_params_t = boost::callable_traits::args_t<Fn>;

        using Base = CommandNode<Fn>;

        constexpr DetailedCommandNode(std::string_view literal,
            std::string_view description,
            Fn&& fn,
            Ts&&... parameters)
            noexcept
            : Base(literal, std::forward<Fn&&>(fn)), _parameters({ parameters... }), _description(description)
        {
            static_assert(Details::ValidateParameterInfo<Fn, typename Ts::type...>::value,
                "Parameter informations do not match the command.");
        }
        
        constexpr std::string_view literal() const { return CommandNode<Fn>::literal(); }
        constexpr std::string_view description() const { return _description; }

        template <typename Op>
        void ForEachParameter(Op&& fn) const noexcept {
            Details::Iterate<0u>(_parameters, [fn](auto parameter) {
                fn(parameter.name(), parameter.description(), decltype(parameter)::Required);
                return false;
            });
        }

    private:
        std::tuple<Ts...> _parameters;
        std::string_view _description;
    };

    template <typename S>
    struct TreeParser final {
        template <typename T, typename U>
        struct TreePath final {
            using node_type = T;

            constexpr TreePath(T const& node, U const& parent) noexcept : _node(node), _parent(parent) { }

            template <typename V>
            TreePath<V, TreePath<T, U>> ChainWith(V const& node) const {
                return TreePath<V, TreePath<T, U>> { node, *this };
            }

            template <typename Op>
            void Traverse(Op&& fn) const {
                _parent.Traverse(std::forward<Op&&>(fn));
                fn(_node);
            }

            T const& node() const noexcept { return _node; }

        private:
            T const& _node;
            U const& _parent;
        };

        template <typename T>
        struct TreePath<T, void> final {
            using node_type = T;

            constexpr explicit TreePath(T const& node) noexcept : _node(node) { }

            template <typename V>
            TreePath<V, TreePath<T, void>> ChainWith(V const& node) const {
                return TreePath<V, TreePath<T, void>> { node, *this };
            }

            template <typename Op>
            void Traverse(Op&& op) const { op(_node); }

            T const& node() const noexcept { return _node; }
        private:
            T const& _node;
        };

        template <typename T>
        constexpr static auto Parse(std::string_view input, T const& root, S& source) noexcept
            -> bool
        {
            TreePath<T, void> rootPath { root };

            return _ProcessImpl<bool>(input, std::move(rootPath), [&source](auto treePath, std::string_view reader) noexcept -> bool {
                return treePath.node().template TryExecute<S>(reader, source);
            });
        }

#if __cpp_lib_concepts >= 201907L
        template <typename T, typename U>
        constexpr static auto PrintHelp(std::string_view input, T const& root, U& printer) noexcept
#else
        template <typename T>
        constexpr static auto PrintHelp(std::string_view input, T const& root, HelpPrinter& printer) noexcept
#endif
            -> void
        {
#if __cpp_lib_concepts >= 201907L
            static_assert(HelpPrinter<U>, "Printer lacks complete definition");
#endif

            TreePath<T, void> rootPath { root };

            return _ProcessImpl<void>(input, std::move(rootPath), [&printer](auto treePath, std::string_view) noexcept -> void {
                printer.NotifyBeginCommand();
                treePath.Traverse([&printer](auto node) {
                    if constexpr (Details::IsTree<decltype(node)>::value)
                        printer.NotifyLiteral(node._value.literal());
                    else if constexpr (Details::IsNode<decltype(node)>::value)
                        printer.NotifyLiteral(node.literal());
                });

                if constexpr (Details::IsTree<decltype(treePath.node())>::value) {
                    if constexpr (Details::IsDetailedCommandNode<decltype(treePath.node()._value)>::value) {
                        treePath.node()._value.ForEachParameter([&printer](auto name, auto description, auto required) {
                            printer.NotifyParameter(name, required);
                        });

                        printer.NotifyCommandDescription(treePath.node()._value.description());

                        treePath.node()._value.ForEachParameter([&printer](auto name, auto description, auto required) {
                            printer.NotifyParameterDescription(name, description, required);
                        });
                    } else if constexpr (Details::IsSimpleCommandNode<decltype(treePath.node()._value)>::value) {
                        printer.NotifyParameter("parameters...", true);
                    } else if constexpr (Details::IsBareNode<decltype(treePath.node()._value)>::value) {
                        // TODO: Implement help printing for subcommands (recursively!)
                    }
                } else {
                    if constexpr (Details::IsDetailedCommandNode<decltype(treePath.node())>::value) {
                        treePath.node().ForEachParameter([&printer](auto name, auto description, auto required) {
                            printer.NotifyParameter(name, required);
                        });
                        
                        printer.NotifyCommandDescription(treePath.node().description());

                        treePath.node().ForEachParameter([&printer](auto name, auto description, auto required) {
                            printer.NotifyParameterDescription(name, description, required);
                        });
                    } else if constexpr (Details::IsSimpleCommandNode<decltype(treePath.node())>::value) {
                        printer.NotifyParameter("parameters...", true);
                    } else if constexpr (Details::IsBareNode<decltype(treePath.node())>::value) {
                        // TODO: Implement help printing for subcommands (recursively!)
                    }
                }

                printer.NotifyEndCommand();
            });
        }

    private:
        template <typename R, typename Operation, typename N, typename P, typename... Ts>
        constexpr static auto _ProcessImpl(std::string_view reader, TreePath<Tree<N, Ts...>, P>&& pathNode, Operation&& operation) noexcept
            -> R
        {
            using return_type = R;

            if constexpr (std::is_same_v<N, BareNode>)
            {
                ValidationResult validationResult { pathNode.node()._value.Validate(reader) };
                switch (validationResult)
                {
                    case ValidationResult::Children:
                    {
                        if constexpr (std::is_void_v<return_type>) {
                            return Details::Iterate<0u>(pathNode.node()._children,
                                [&pathNode, &operation](auto childNode, auto reader) noexcept -> return_type {
                                    return _ProcessImpl<R>(reader, pathNode.ChainWith(childNode), operation);
                                }, reader.substr(1));
                        } else {
                            return Details::IterateUntil<0u>(pathNode.node()._children,
                                [&pathNode, &operation](auto childNode, auto reader) noexcept -> return_type {
                                    return _ProcessImpl<R>(reader, pathNode.ChainWith(childNode), operation);
                                }, [](auto r) { return r == true; }, reader.substr(1));
                        }
                    }
                    case ValidationResult::Execute:
                        assert(false && "Unreachable");
                        break;
                    default:
                        break;
                }

                if constexpr (!std::is_void_v<return_type>)
                    return return_type { };
            } else {
                return operation(pathNode, reader);
            }
        }

        template <typename R, typename Operation, typename N, typename P>
        constexpr static auto _ProcessImpl(std::string_view reader, TreePath<N, P>&& pathNode, Operation&& operation) noexcept
            -> R
        {
            using return_type = R;

            // static_assert(IsPathToBareNode);
            if constexpr(std::is_same_v<N, BareNode>)
            {
                ValidationResult validationResult { pathNode.node().Validate(reader) };
                switch (validationResult)
                {
                    case ValidationResult::Children:
                    {
                        return Details::Iterate<0u>(pathNode.node()._children,
                            [&pathNode, &operation](auto childNode, auto reader) noexcept -> R {
                                return _ProcessImpl<R>(reader, pathNode.ChainWith(childNode), operation);
                            }, [](auto r) { return r == true; },  reader.substr(1));
                    }
                    case ValidationResult::Execute:
                        assert(false && "Unreachable");
                        break;
                    default:
                        break;
                }

                if constexpr (!std::is_void_v<return_type>)
                    return return_type { };
            } else {
                return operation(pathNode, reader);
            }
        }

    };

    template <typename Fn>
    constexpr static CommandNode<Fn> Command(std::string_view literal, Fn&& fn) noexcept {
        return CommandNode<Fn> { literal, std::forward<Fn&&>(fn) };
    }

    template <typename Fn, typename... Ts>
    constexpr static DetailedCommandNode<Fn, Ts...> Command(std::string_view literal, std::string_view description, Fn&& fn, Ts&&... params) noexcept {
        return DetailedCommandNode<Fn, Ts...> { literal, description, std::forward<Fn&&>(fn), std::forward<Ts&&>(params)... };
    }

    constexpr static BareNode Node(std::string_view literal) noexcept {
        return BareNode { literal };
    }
}
