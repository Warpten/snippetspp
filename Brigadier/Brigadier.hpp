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

#ifndef BRIGADIER_HEADER_GUARD_HPP__
#define BRIGADIER_HEADER_GUARD_HPP__

static_assert(__cplusplus >= 201703L, "Brigadier only supports C++17 and upwards.");

#if __cplusplus >= 202002L
# define BRIGADIER_CPP20
#else
# define BRIGADIER_CPP17
#endif

#if __cpp_concepts >= 201907L
# define BRIGADIER_CONCEPTS_ENABLED
#endif

#if defined(BRIGADIER_CPP20) && defined(__cpp_lib_format)
# include <format>
# define BRIGADIER_FORMAT_NAMESPACE std
#elif __has_include(<fmt/format.h>)
# include <fmt/format.h>
# define BRIGADIER_FORMAT_NAMESPACE fmt
#else
static_assert(false, "Brigadier requires std::format or fmt::format");
#endif

#if defined(_WIN32)
# define BRIGADIER_UNREACHABLE __assume(0)
#elif defined(__clang__) || defined(__gcc__)
# define BRIGADIER_UNREACHABLE __builtin_unreachable()
#else
# define BRIGADIER_UNREACHABLE
#endif

#include <array>
#include <cassert>
#include <charconv>
#include <optional>
#include <functional>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <unordered_map>

#include <iostream>

#include <boost/algorithm/string.hpp>
#include <boost/callable_traits/args.hpp>
#include <boost/callable_traits/is_noexcept.hpp>

#if defined(BRIGADIER_CPP20)
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
        using type = std::remove_cv_t<std::remove_reference_t<T>>;
    };

    template <typename T>
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
    
    inline constexpr struct TEmpty { } Empty;

    namespace Details {
        namespace {
            template <typename T>
            struct IsTupleT : std::false_type { };

            template <typename... Ts>
            struct IsTupleT<std::tuple<Ts...>> : std::true_type { };

            template <typename T>
            constexpr static const bool IsTuple = IsTupleT<std::decay_t<T>>::value;
        }

        // ---- IsBareNode ----
        namespace {
            template <typename T>
            constexpr static const bool IsBareNode = std::is_same_v<std::decay_t<T>, BareNode>;
        }

        // ---- IsSimpleCommandNode ----
        namespace {
            //> Determines wether or not T is a specialization of CommandNode<Fn>.
            template <typename T> struct IsSimpleCommandNodeT : std::false_type { };
            template <typename Fn> struct IsSimpleCommandNodeT<CommandNode<Fn>> : std::true_type { };

            template <typename T>
            constexpr static const bool IsSimpleCommandNode = IsSimpleCommandNodeT<std::decay_t<T>>::value;
        }

        // ---- IsDetailedCommandNode ----
        namespace {
            //> Determines wether or not T is a specialization of DetailedCommandNode<Fn, ParameterMeta<Ts>...>.
            template <typename T> struct IsDetailedCommandNodeT : std::false_type { };
            template <typename Fn, typename... Ts> struct IsDetailedCommandNodeT<DetailedCommandNode<Fn, Ts...>> : std::true_type { };

            template <typename T>
            constexpr static const bool IsDetailedCommandNode = IsDetailedCommandNodeT<std::decay_t<T>>::value;
        }

        // ---- IsNode ----
        namespace {
            template <typename T>
            constexpr static const bool IsNode = IsDetailedCommandNode<T> || IsSimpleCommandNode<T> || IsBareNode<T>;
        }

        // ---- IsTree ----
        namespace {
            template <typename T> struct IsTreeT : std::false_type { };
            template <typename T, typename... Ts> struct IsTreeT<Tree<T, Ts...>> : std::true_type { };

            template <typename T>
            constexpr static const bool IsTree = IsTree<std::decay_t<T>>;
        }

        // ---- TupleSize ----
        // Like std::tuple_size but handles empty tuples properly
        namespace {
            template <typename T> struct TupleSizeT;
            template <typename... Ts> struct TupleSizeT<std::tuple<Ts...>> { constexpr static const size_t value = sizeof...(Ts); };

            template <typename T>
            constexpr static const size_t TupleSize = TupleSizeT<std::decay_t<T>>::value;
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

        namespace { // Any(Tuple, Predicate)
            template <typename Tuple, typename Predicate, size_t... Is>
            constexpr auto _Any(Tuple&& tuple, Predicate&& predicate, std::index_sequence<Is...>) noexcept {
                return (predicate(std::get<Is>(tuple)) || ...);
            }

            template <typename Tuple, typename Predicate>
            constexpr auto Any(Tuple&& tuple, Predicate&& pred) {
                return _Any(std::forward<Tuple&&>(tuple), 
                    std::forward<Predicate&&>(pred),
                    std::make_index_sequence<std::tuple_size_v<std::decay_t<Tuple>>>()
                );
            }
        }

        namespace { // All(Tuple, Predicate)
            template <typename Tuple, typename Predicate, size_t... Is>
            constexpr void _All(Tuple&& tuple, Predicate&& predicate, std::index_sequence<Is...>) noexcept {
                (predicate(std::get<Is>(tuple)), ...);
            }

            template <typename Tuple, typename Predicate>
            constexpr auto All(Tuple&& tuple, Predicate&& pred) {
                return _All(std::forward<Tuple&&>(tuple), 
                    std::forward<Predicate&&>(pred), 
                    std::make_index_sequence<std::tuple_size_v<std::decay_t<Tuple>>>()
                );
            }
        }

        namespace { // Iterate(Tuple, Predicate, Condition)
            template <typename...Ts, typename Predicate, typename Condition>
            constexpr auto Iterate(std::tuple<Ts...> const& tuple, Predicate&& pred, Condition&& cond) {
                return _Iterate(tuple, std::forward<Predicate&&>(pred), std::forward<Condition&&>(cond));
            }

            template <typename Tuple, typename Predicate, typename Condition, size_t... Is>
            constexpr auto _Iterate(Tuple&& tuple, Predicate&& predicate, Condition&& condition) noexcept {
                return Any(std::forward<Tuple&&>(tuple), [&predicate, &condition](auto elem) {
                    auto&& value = predicate(elem);
                    if (condition(value))
                        return value;

                    return std::decay_t<decltype(value)> { };
                });
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
            struct ParameterIsInput<T, std::enable_if_t<decltype(test<T>(0))::value>> {
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
                template <typename ... Us>
                static constexpr auto decay_types(std::tuple<Us...> const &) -> std::tuple<std::decay_t<Us>...>;

                template <typename T>
                using decay_tuple = decltype(decay_types(std::declval<T>()));

                using source_tuple = typename FilterHandlerParams<
                    ParameterIsInput,
                    decay_tuple<boost::callable_traits::args_t<Fn>>
                >::type;
                using target_tuple = std::tuple<std::decay_t<Ts>...>;

                enum {
                    value = std::is_same_v<source_tuple, target_tuple>
                };
            };
        }

        using namespace std::string_view_literals;
        
#if defined(BRIGADIER_CONCEPTS_ENABLED)
        template <typename T>
        concept HasNotifyBegin = requires (T& t) {
            { t.NotifyBeginCommand() } -> std::same_as<void>;
        };
        template <typename T>
        concept HasNotifyEnd = requires (T& t) {
            { t.NotifyEndCommand() } -> std::same_as<void>;
        };
        template <typename T>
        concept HasNotifyLiteral = requires (T& t) {
            { t.NotifyLiteral("foo"sv) } -> std::same_as<void>;
        };
        template <typename T>
        concept HasNotifyParameter = requires (T& t) {
            { t.NotifyParameter("foo"sv, true) } -> std::same_as<void>;
        };
        template <typename T>
        concept HasNotifyCommandDescription = requires (T& t) {
            { t.NotifyCommandDescription("foo"sv) } -> std::same_as<void>;
        };
        template <typename T>
        concept HasNotifyParameterDescription = requires (T& t) {
            { t.NotifyParameterDescription("foo"sv, std::optional<std::string_view> { }, true) } -> std::same_as<void>;
        };
        template <typename T>
        concept HasNotifyTemplateParameterDescription = requires (T& t) {
            { t.template NotifyParameterDescription<TEmpty>("foo"sv, std::optional<std::string_view> { }, true) } -> std::same_as<void>;
        };
#else
        namespace {
            template <typename> struct SFINAE : std::true_type { };
#define MAKE_PSEUDO_CONCEPT(CONCEPT, FNCALL)                                                             \
            template <typename T>                                                                        \
            static auto test_##CONCEPT##(int) -> SFINAE<decltype(std::declval<T>().##FNCALL##)>;         \
                                                                                                         \
            template <typename>                                                                          \
            static auto test_##CONCEPT##(long) -> std::false_type;                                       \
                                                                                                         \
            template <typename T>                                                                        \
            constexpr static const bool CONCEPT = decltype(test_##CONCEPT##<T>(0))::value;

            MAKE_PSEUDO_CONCEPT(HasNotifyBegin, NotifyBeginCommand());
            MAKE_PSEUDO_CONCEPT(HasNotifyEnd, NotifyEndCommand());
            MAKE_PSEUDO_CONCEPT(HasNotifyLiteral, NotifyLiteral("foo"sv));
            MAKE_PSEUDO_CONCEPT(HasNotifyParameter, NotifyParameter("foo"sv, true));
            MAKE_PSEUDO_CONCEPT(HasNotifyCommandDescription, NotifyCommandDescription("foo"sv));
            MAKE_PSEUDO_CONCEPT(HasNotifyParameterDescription, NotifyParameterDescription("foo"sv, std::optional<std::string_view> { }, true));
            MAKE_PSEUDO_CONCEPT(HasNotifyTemplateParameterDescription, template NotifyParameterDescription<TEmpty>("foo"sv, std::optional<std::string_view> { }, true));
#undef MAKE_PSEUDO_CONCEPT
        }
#endif
    }


    /// <summary>
    /// 
    /// </summary>
    /// <typeparam name="T">Type of the node</typeparam>
    /// <typeparam name="Ts">Types of childrend nodes</typeparam>
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

        String(String const& other) noexcept = delete;

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
            if (pos == std::string_view::npos) {
                auto source = reader;
                reader = reader.substr(reader.length());
                return Word { source };
            }

            if (pos == 0)
                return std::nullopt;

            auto data = reader.substr(0, pos);
            reader = reader.substr(pos);
            return Word { data };
        }
    };

    template <>
    struct _ParameterExtractor<GreedyString, void> {
        static auto _Extract(std::string_view& reader) noexcept
            -> std::optional<GreedyString>
        {
            std::string_view source = reader;
            reader = reader.substr(reader.length());

            return GreedyString { source };
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
                owningCopy.erase(std::remove(owningCopy.begin(), owningCopy.end(), '\\'), owningCopy.end());

                return QuotedString { std::move(owningCopy) };
            }

            return QuotedString { data };
        }
    };

    struct BareNode {
        constexpr BareNode(std::string_view const literal) noexcept : _literal(literal) { }

        constexpr BareNode(BareNode const&) = default;
        constexpr BareNode(BareNode&&) = default;

        constexpr ValidationResult Validate(std::string_view reader) const noexcept {
            bool success = reader.find(_literal) == 0;
            
            if (success)
                return ValidationResult::Children;

            return ValidationResult::Failure;
        }

        template <typename... Ts>
        constexpr Tree<BareNode, Ts...> Then(Ts&&... parameters) const noexcept {
            return Tree { *this, std::forward<Ts&&>(parameters)... };
        }

        constexpr std::string_view literal() const { return _literal; }

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

        constexpr ValidationResult Validate(std::string_view reader) const noexcept
        {
            if (reader.find(_literal) != 0)
                return ValidationResult::Failure;

            return ValidationResult::Execute;
        }

        //> Returns true if execution succeeded, false otherwise.
        template <typename S>
        constexpr auto TryExecute(std::string_view reader, const S& source) const noexcept
            -> bool
        {
            using args_t = boost::callable_traits::args_t<Fn>;

            bool success = true;
            auto args { _ExtractParameters<S>(source, success, reader, static_cast<args_t*>(nullptr)) };

            if (success)
                std::apply(_fn, std::move(args));

            return success;
        }

        constexpr std::string_view literal() const { return _literal; }

    private:
        std::string_view _literal;
        Fn _fn;

        template <typename S, typename... Ts>
        constexpr auto _ExtractParameters(const S& source, bool& success, std::string_view& reader, std::tuple<Ts...>* tpl = nullptr) const {
            return std::tuple {  _ExtractParameter<S, Ts>(source, reader, success)... };
        }

        template <typename S, typename T>
        constexpr auto _ExtractParameter(const S& source, std::string_view& reader, bool& success) const noexcept {
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
                using underlying_type = typename decayed_type::value_type;

                static_assert(Details::ParameterIsInput<underlying_type>::value, "Missing implementation of _ParameterExtractor");

                // If we already failed, fail fast
                if (success && reader.length() > 0) {
                    std::string_view copy { reader };
                    auto result { _ParameterExtractor<underlying_type>::_Extract(copy) };
                    if (result.has_value()) {
                        if (copy.size() > 1)
                            reader = copy.substr(1);
                        else
                            reader = copy;
                    }

                    return result;
                }

                return std::optional<underlying_type> { };
            } else {
                success = reader.length() > 0;
                // If we already failed, fail fast
                if (!success)
                    return decayed_type { };

                static_assert(Details::ParameterIsInput<decayed_type>::value, "Missing implementation of _ParameterExtractor");

                auto result { _ParameterExtractor<decayed_type>::_Extract(reader) };
                if (result.has_value()) {
                    if (reader.size() > 1)
                        reader = reader.substr(1);

                    return std::move(result.value());
                }

                success = false;
                return decayed_type { };
            }
        }
    };

    template <typename Fn>
    CommandNode(Fn) -> CommandNode<Fn>;
    
    template <typename U>
    struct ParameterMeta {
        using type = U;

        constexpr static const bool Required = !Details::is_optional_v<U>;

        explicit constexpr ParameterMeta(std::string_view const name, std::string_view const description) noexcept : _name(name), _description(description) { }
        explicit constexpr ParameterMeta(std::string_view const name) noexcept : _name(name) { }

        constexpr const std::string_view name() const { return _name; }
        constexpr std::optional<const std::string_view> const& description() const noexcept { return _description; }
        constexpr static bool required() { return Required; }

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
        
        constexpr std::string_view literal() const noexcept { return CommandNode<Fn>::literal(); }
        constexpr std::string_view description() const noexcept { return _description; }

        template <typename Op>
        constexpr void ForEachParameter(Op&& fn) const noexcept {
            Details::All(_parameters, std::forward<Op&&>(fn));
        }

    private:
        std::tuple<Ts...> _parameters;
        std::string_view _description;
    };

    template <typename S>
    struct TreeParser final {
        template <typename T>
        struct TreePathBase {
            constexpr TreePathBase(T const& node) noexcept : _value(node) { }

            template <typename Operation>
            constexpr void Traverse(Operation&& operation) const noexcept {
                operation(_value);
            }

            constexpr T const& node() const noexcept { return _value; }
            constexpr std::tuple<> children() const noexcept { return std::tuple { }; }

            constexpr static const std::size_t children_count = 0;
            constexpr static const bool has_node = true;
        private:
            T const& _value;
        };

        template <>
        struct TreePathBase<void> {
            template <typename Operation>
            constexpr void Traverse(Operation&&) const noexcept { }
            
            constexpr static const std::size_t children_count = 0;
            constexpr static const bool has_node = false;
        };

        template <typename T, typename... Ts>
        struct TreePathBase<Tree<T, Ts...>> {
            constexpr TreePathBase(Tree<T, Ts...> const& node) noexcept : _value(node) { }

            template <typename Operation>
            constexpr void Traverse(Operation&& operation) const noexcept {
                operation(_value._value);
            }

            constexpr T const& node() const noexcept { return _value._value; }
            constexpr std::tuple<Ts...> const& children() const noexcept { return _value._children; }

            constexpr static const std::size_t children_count = sizeof...(Ts);
            constexpr static const bool has_node = true;

        private:
            Tree<T, Ts...> const& _value;
        };

        /// <typeparam name="T">Type of this node of the path.</typeparam>
        /// <typeparam name="P">Type of the parent in this path. *will* be TreePath<X, Y>.
        template <typename T, typename P>
        struct TreePath final : TreePathBase<T> {
            using Base = TreePathBase<T>;

            constexpr TreePath(T const& node, P const& parent) noexcept : Base(node), _parent(parent) { }

            template <typename V>
            constexpr TreePath<V, TreePath<T, P>> ChainWith(V const& node) const noexcept {
                return TreePath<V, TreePath<T, P>> { node, *this };
            }

            template <typename Operation>
            constexpr void Traverse(Operation&& operation) const noexcept {
                _parent.Traverse(std::forward<Operation&&>(operation));
                Base::Traverse(std::forward<Operation&&>(operation));
            }

            constexpr auto children() const noexcept { return Base::children(); }
            constexpr P const& parent() const noexcept { return _parent; }
            
            constexpr static const bool has_parent = true;
        private:
            P const& _parent;
        };

        template <typename T>
        struct TreePath<T, void> final : TreePathBase<T> {
            using Base = TreePathBase<T>;

            constexpr explicit TreePath(T const& node) noexcept : Base(node) { }

            template <typename V>
            constexpr TreePath<V, TreePath<T, void>> ChainWith(V const& node) const noexcept {
                return TreePath<V, TreePath<T, void>> { node, *this };
            }
            
            template <typename Operation>
            constexpr void Traverse(Operation&& operation) const noexcept {
                Base::Traverse(std::forward<Operation&&>(operation));
            }

            constexpr auto node() const noexcept { return Base::node(); }
            constexpr auto children() const noexcept { return Base::children(); }
            constexpr TreePath<T, void> const& parent() const noexcept { return *this; }

            constexpr static const bool has_parent = false;
        };

        template <typename T, typename C>
        TreePath(T, C) -> TreePath<T, C>;

        template <typename T>
        TreePath(T) -> TreePath<T, void>;

        template <typename Root>
        constexpr static auto Parse(std::string_view input, Root&& root, const S& source) noexcept
            -> bool
        {
            auto executionCallback = [](auto path, std::string_view input, S const& source) -> bool {
                if constexpr (Details::IsBareNode<decltype(path.node())>)
                    return false;
                else // Attempt to execute the input.
                    return path.node().TryExecute(input, source);
            };

            auto validationCallback = [](auto path, std::string_view input, S const& source) -> ValidationResult {
                // Evaluate which path to take during traversal.
                return path.node().Validate(input);
            };

            auto traversalCallback = [](auto path, std::string_view input, S const& source) {
                using namespace std::string_view_literals;

                // Called as input for each child node.
                //   Return a string_view to the remainder of the string
                //   and forward the source for TryExecute.
                std::string_view childInput = input.length() > (path.node().literal().length() + 1)
                    ? input.substr(path.node().literal().length() + 1)
                    : "";
                return std::tuple {
                    childInput,
                    std::cref(source)
                };
            };

            std::tuple traversalParameters { input, std::cref(source) };

            return _Walk<TraversalFlags::None>(root,
                executionCallback,
                validationCallback,
                traversalCallback,
                traversalParameters);
        }

        template <typename Root, typename Printer>
        constexpr static auto PrintHelp(std::string_view input, Root&& root, Printer&& printer) noexcept {
            constexpr auto printCallback = [](auto path, Printer&& printer, auto self) -> void {
                using node_type = std::decay_t<decltype(path.node())>;

                if constexpr (Details::IsSimpleCommandNode<node_type> || Details::IsDetailedCommandNode<node_type>) {
                    printer.NotifyBeginCommand();

                    path.Traverse([&printer](auto pathNode) noexcept -> void {
                        static_assert(Details::IsNode<decltype(pathNode)>, "Traversing an ill-formed path");

                        printer.NotifyLiteral(pathNode.literal());
                    });

                    if constexpr (Details::IsSimpleCommandNode<node_type>) {
                        printer.NotifyParameter("parameters...", true);
                    }
                    else if constexpr (Details::IsDetailedCommandNode<node_type>) {
                        path.node().ForEachParameter([&printer](auto parameter) noexcept -> void {
                            printer.NotifyParameter(parameter.name(), parameter.required());
                            });

                        printer.NotifyCommandDescription(path.node().description());

                        path.node().ForEachParameter([&printer](auto parameter) noexcept -> void {
                            if constexpr (Details::HasNotifyParameterDescription<Printer>) {
                                printer.NotifyParameterDescription(parameter.name(), parameter.description(), parameter.required());
                            }
                            else if constexpr (Details::HasNotifyTemplateParameterDescription<Printer>) {
                                printer.template NotifyParameterDescription<typename decltype(parameter)::type>(parameter.name(), parameter.description(), parameter.required());
                            }
                        });
                    }

                    printer.NotifyEndCommand();
                }
                else if constexpr (Details::IsBareNode<node_type>) {
                    // Iterate and explore each child by just giving them their literal
                    Details::All(path.children(), [&path, &printer, &self](auto childNode) {
                        self(path.ChainWith(childNode), std::forward<Printer&&>(printer), self);
                    });
                }
            };

            auto executionCallback = [&printCallback](auto path, std::string_view input, Printer&& printer) -> bool {
                printCallback(path, std::forward<Printer&&>(printer), printCallback);

                // Return false so that if we are traversing a bare node we may dig deeper
                return false;
            };

            auto validationCallback = [](auto path, std::string_view input, Printer&& printer) -> ValidationResult {
                ValidationResult result = path.node().Validate(input);

                // Adjust to Execute, so that we call Operation&& and can proceed to dig in available children.
                if (result == ValidationResult::Failure)
                    return ValidationResult::Execute;

                return result;
            };

            auto traversalCallback = [](auto path, std::string_view input, Printer&& printer) {
                std::string_view childInput = input.length() > (path.node().literal().length() + 1)
                    ? input.substr(path.node().literal().length() + 1)
                    : "";
                return std::tuple{
                    childInput,
                    std::forward<Printer&&>(printer)
                };
            };

            std::tuple traversalArgs { input, printer };

            return _Walk<TraversalFlags::AllChildren | TraversalFlags::AllRoots>(root,
                executionCallback,
                validationCallback,
                traversalCallback,
                traversalArgs);
        }

    private:
        enum TraversalFlags : uint32_t {
            None        = 0x00,
            AllChildren = 0x01,
            AllRoots    = 0x02
        };

        template <uint32_t Flags, typename Operation, typename Filter, typename Transform, typename Args, typename Root>
        constexpr static bool _Walk(Root&& root, Operation&& operation, Filter&& filter, Transform&& transform, Args&& args) noexcept {
            if constexpr (Details::IsTuple<Root>)
                return _WalkRoots<Flags>(root, operation, filter, transform, args, std::make_index_sequence<Details::TupleSize<Root>>());
            else
                return _WalkRoot<Flags>(TreePath { root }, operation, filter, transform, args);
        }

        template <uint32_t Flags, typename Operation, typename Filter, typename Transform, typename Args, typename Roots, size_t... Is>
        constexpr static bool _WalkRoots(Roots&& roots, Operation&& operation, Filter&& filter, Transform&& transform, Args&& args, std::index_sequence<Is...>) noexcept {
            if constexpr ((Flags & TraversalFlags::AllRoots) != 0)
            {
                bool success = false;
                ((success |= _WalkRoot<Flags>(TreePath { std::get<Is>(roots) },
                    operation, 
                    filter, 
                    transform, 
                    std::forward<Args&&>(args))), ...);
                return success;
            }
            else
                return (_WalkRoot<Flags>(TreePath { std::get<Is>(roots) }, operation, filter, transform, std::forward<Args&&>(args)) || ...);
        }

        template <uint32_t Flags, typename Child, typename Parent, typename Operation, typename Filter, typename Transform, typename Args>
        constexpr static auto _WalkRoot(TreePath<Child, Parent> const& root, Operation&& operation, Filter&& filter, Transform&& transform, Args&& args) noexcept 
            -> bool
        {
            auto executionArguments = std::tuple_cat(std::tuple { root }, args);
            ValidationResult validationResult = std::apply(filter, executionArguments);
            switch (validationResult) {
                case ValidationResult::Children:
                {
                    auto childrenParameters = std::apply(transform, executionArguments);

                    auto childrenHandler = [&](auto childNode) -> bool {
                        return _WalkRoot<Flags>(root.ChainWith(childNode),
                            std::forward<Operation&&>(operation),
                            std::forward<Filter&&>(filter),
                            std::forward<Transform&&>(transform),
                            childrenParameters);
                    };

                    if constexpr ((Flags & TraversalFlags::AllChildren) != 0) {
                        Details::All(root.children(), childrenHandler);
                        return true;
                    } else {
                        return Details::Any(root.children(), childrenHandler);
                    }

                    // Shut up false positive warnings
                    break;
                }
                case ValidationResult::Failure:
                    return false;
                case ValidationResult::Execute:
                    std::apply(operation,
                        std::tuple_cat(std::tuple { root }, 
                            std::apply(transform, std::tuple_cat(std::tuple { root }, args))
                        )
                    );
                    return true;
                default:
                    BRIGADIER_UNREACHABLE;
            }
        }
    };

    template <typename Fn>
    constexpr static CommandNode<Fn> Command(std::string_view literal, Fn&& fn) noexcept {
        return CommandNode { literal, std::forward<Fn&&>(fn) };
    }

    template <typename Fn, typename... Ts>
    constexpr static DetailedCommandNode<Fn, Ts...> Command(std::string_view literal, std::string_view description, Fn&& fn, Ts&&... params) noexcept {
        return DetailedCommandNode<Fn, Ts...> { literal, description, std::forward<Fn&&>(fn), std::forward<Ts&&>(params)... };
    }

    constexpr static BareNode Node(std::string_view literal) noexcept {
        return BareNode { literal };
    }
}

#if defined(BRIGADIER_CONCEPTS_ENABLED)
# undef BRIGADIER_CONCEPTS_ENABLED
#endif

#if defined(BRIGADIER_CPP20)
# undef BRIGADIER_CPP20
#elif defined(BRIGADIER_CPP17)
# undef BRIGADIER_CPP17
#endif

#if defined(BRIGADIER_FORMAT_NAMESPACE)
# undef BRIGADIER_FORMAT_NAMESPACE
#endif

#endif // BRIGADIER_HEADER_GUARD_HPP__
