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

#if __cplusplus >= 202002L
namespace Brigadier::Details
{
    template <typename T>
    using remove_cvref = std::remove_cvref<T>;

    template< class T >
    using remove_cvref_t = typename remove_cvref<T>::type;
}
#else
namespace Brigadier
{
    namespace Details
    {
        template <typename T>
        struct remove_cvref {
            typedef std::remove_cv_t<std::remove_reference_t<T>> type;
        };

        template< class T >
        using remove_cvref_t = typename remove_cvref<T>::type;
    }
}
#endif

namespace Brigadier {
    namespace Details {
        template <bool B, bool T, bool F>
        struct conditional_v;
        
        template <bool T, bool F>
        struct conditional_v<true, T, F> { constexpr static const bool value = T; };

        template <bool T, bool F>
        struct conditional_v<false, T, F> { constexpr static const bool value = F; };

        template <typename T> struct TupleSize;
        template <typename... Ts> struct TupleSize<std::tuple<Ts...>> { constexpr static const size_t value = sizeof...(Ts); };
        template <> struct TupleSize<std::tuple<>> { constexpr static const size_t value = 0; };

        template <typename T, typename... Ts>
        struct ValidateTree {
            constexpr static const bool value = conditional_v<
                sizeof...(Ts) == 0, 
                true, 
                (std::is_same_v<T, Ts> && ...)
            >::value;
        };

        template <typename T>
        struct is_optional : std::false_type { };

        template <typename T>
        struct is_optional<std::optional<T>> : std::true_type { };

        template <typename T>
        constexpr static const bool is_optional_v = is_optional<T>::value;

        template <
            auto Start,
            auto End,
            auto Inc,
            typename F,
            typename... Args
        >
        constexpr auto constexpr_for(F&& f, Args&&... args)
        {
            if constexpr (Start < End)
            {
                auto r { f(std::integral_constant<decltype(Start), Start> { }, std::forward<Args&&>(args)...) };
                using return_type = decltype(r);

                if constexpr (std::is_convertible_v<return_type, bool>) {
                    if (!static_cast<bool>(r))
                        return false;
                    
                    return constexpr_for<Start + Inc, End, Inc, F>(std::forward<F&&>(f), std::forward<Args&&>(args)...);
                } else {
                    return constexpr_for<Start + Inc, End, Inc, F>(std::forward<F&&>(f), std::forward<Args&&>(args)...);
                }
            }

            return true;
        }

        template <typename T> struct is_reference_wrapper : std::false_type { };
        template <typename T> struct is_reference_wrapper<std::reference_wrapper<T>> : std::true_type { };

        template <typename T>
        constexpr static const bool is_reference_wrapper_v = is_reference_wrapper<T>::value;

        template <typename T>
        struct HasParameterExtractor
        {
        private:
            template <typename U>
            static std::true_type test(decltype(&_ParameterExtractor<U>::_Extract));
            static std::false_type test(...);

        public:
            enum { value = decltype(test<T>(0))::value };
        };
    }

    template <typename T>
    struct Errorable final {
        template <typename... Ts>
        static Errorable<T> MakeError(std::string_view fmt, Ts&&... args) noexcept {
            return Errorable<T> {
                error_t {},
                BRIGADIER_FORMAT_NAMESPACE::format(fmt, std::forward<Ts&&>(args)...)
            };
        }

        template <typename... Ts>
        static Errorable<T> MakeSuccess(Ts&&... args) noexcept {
            return Errorable<T> {
                success_t {},
                std::forward<Ts&&>(args)...
            };
        }

        constexpr operator bool() const { return _value.has_value(); }
        constexpr operator T const& () const { return _value.value(); }

        template <typename U>
        Errorable<U> Map(std::function<U(T const&)> fn) const noexcept {
            if (!_value.has_value())
                return Errorable<U>::MakeError(_error.value());
            return Errorable<U>::MakeSuccess(std::move(fn(_value.value())));
        }

        constexpr T const& value() const noexcept { return _value.value(); }
        constexpr T const& value() noexcept { return _value.value(); }

        constexpr std::string_view error() const noexcept { return _error.value(); }

        struct error_t {};
        struct success_t {};

        Errorable(error_t, std::string err) noexcept
            : _value(std::nullopt), _error(std::move(err))
        { }

        template <typename... Ts>
        Errorable(success_t, Ts&&... args) noexcept
            : _value(std::in_place, std::forward<Ts&&>(args)...), _error(std::nullopt)
        {

        }

        Errorable(Errorable<T> const&) = delete;
        Errorable(Errorable<T>&&) noexcept = default;

    private:
        std::optional<T> _value;
        std::optional<std::string> _error;
    };

    template <> struct Errorable<std::string> final {
        operator bool() const { return _success; }
        operator std::string const& () const { return _value; }

        std::string const& value() const noexcept { return _value; }
        std::string const& value() noexcept { return _value; }

        template <typename... Ts>
        static Errorable<std::string> MakeError(std::string_view fmt, Ts&&... args) noexcept {
            return Errorable<std::string> { false, BRIGADIER_FORMAT_NAMESPACE::format(fmt, std::forward<Ts&&>(args)...) };
        }

        static Errorable<std::string> MakeSuccess(std::string&& value) noexcept {
            return Errorable<std::string> { true, std::move(value) };
        }

        Errorable(bool success, std::string&& value) noexcept
            : _value(value), _success(success)
        {

        }

        Errorable(Errorable<std::string> const&) = delete;
        Errorable(Errorable<std::string>&&) noexcept = default;

    private:
        std::string _value;
        bool _success;
    };

    template <typename T, typename... Ts>
    struct Tree {
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

    template <typename Fn> struct CommandNode;
    struct BareNode;

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

        explicit String(const char* data, size_t sz) noexcept
        {
            _val = new char[sz];
            std::memcpy(_val, data, sz);
            _sz = sz;
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

    template <typename T, typename Enable = void>
    struct _ParameterExtractor;

    template <typename T>
    struct _ParameterExtractor<T, std::enable_if_t<std::is_integral_v<T>>> {
        static auto _Extract(std::string_view& reader) noexcept {
            T value;
            auto result = std::from_chars(
                reader.data(),
                reader.data() + reader.size(),
                value, 
                10);

            bool success = result.ec == std::errc { };
            if (success) {
                reader = result.ptr;
                return Errorable<T>::MakeSuccess(std::move(value));
            }

            return Errorable<T>::MakeError("Expected integer");
        }
    };

    template <typename T>
    struct _ParameterExtractor<T, std::enable_if_t<std::is_floating_point_v<T>>> {
        static auto _Extract(std::string_view& reader) noexcept {
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
                return Errorable<T>::MakeSuccess(std::move(value));
            }

            return Errorable<T>::MakeError("Expected decimal");
        }
    };

    template <typename T>
    struct _ParameterExtractor<std::optional<T>, void> {
        static auto _Extract(std::string_view& reader) noexcept {
            auto ret = _ParameterExtractor<T>::_Extract(reader);
            if (!ret) {
                reader = { reader.data() - 1, reader.size() + 1 };
                return Errorable<std::optional<T>>::MakeSuccess(std::nullopt);
            }

            return ret.template Map<std::optional<T>>([](auto&& value) noexcept {
                return std::make_optional(std::move(value));
            });
        }
    };

    template <>
    struct _ParameterExtractor<Word, void> {
        static auto _Extract(std::string_view& reader) noexcept {
            auto pos = reader.find(' ', 0);
            if (pos == std::string_view::npos)
                return Errorable<Word>::MakeSuccess(reader);

            if (pos == 0)
                return Errorable<Word>::MakeError("Empty word found");

            return Errorable<Word>::MakeSuccess(reader.substr(0, pos));
        }
    };

    template <>
    struct _ParameterExtractor<GreedyString, void> {
        static auto _Extract(std::string_view& reader) noexcept {
            return Errorable<GreedyString>::MakeSuccess(reader);
        }
    };

    template <>
    struct _ParameterExtractor<QuotedString, void> {
        static auto _Extract(std::string_view& reader) noexcept {
            bool success = (reader[0] == '"' || reader[0] == '\'');
            if (!success)
                return Errorable<QuotedString>::MakeError("Expected opening quote");

            auto delim = reader[0];
            auto searchIndex = 1;

            bool escapes = false;

            while (true) {
                auto pos = reader.find(delim, searchIndex);
                success = pos != std::string_view::npos;
                if (!success)
                    return Errorable<QuotedString>::MakeError("Expected closing quote");

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

                return Errorable<QuotedString>::MakeSuccess(owningCopy.c_str(), owningCopy.length());
            }

            return Errorable<QuotedString>::MakeSuccess(data);
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
                using underlying_type = typename T::value_type;

                std::string_view copy { reader.substr(1) };
                auto result { _ParameterExtractor<underlying_type>::_Extract(copy) };
                if (result) {
                    reader = copy;
                    return std::make_optional<underlying_type>(result.value());
                }

                return std::optional<underlying_type> { std::nullopt };
            } else {
                static_assert(Details::HasParameterExtractor<decayed_type>::value, "Missing implementation of _ParameterExtractor");

                reader = reader.substr(1);
                auto result { _ParameterExtractor<decayed_type>::_Extract(reader) };
                success &= !!result;
                if (!result)
                    return decayed_type {};
                return result.value();
            }
        }

        template <typename... Ts>
        constexpr static auto _Validate(std::tuple<Ts...> const& tpl) noexcept
        {
            return Details::constexpr_for<0u, sizeof...(Ts), 1>([](auto i, auto&& tpl) {
                using element_type = std::tuple_element_t<i, std::tuple<Ts...>>;

                if constexpr (!Details::is_reference_wrapper_v<element_type>)
                    return !!std::get<i>(tpl);
                
                return true;
            }, tpl);
        }
    };

    template <typename S>
    struct TreeParser final {
        template <typename T>
        constexpr static auto Parse(std::string_view input, T const root, S& source) noexcept
            -> bool
        {
            auto mutableView { input };
            return _Parse(mutableView, root, source);
        }

    private:
        template <typename T, typename... Ts>
        constexpr static auto _Parse(std::string_view& reader, Tree<T, Ts...> const root, S& source) noexcept
            -> bool
        {
            if constexpr (std::is_same_v<T, BareNode>) {
                ValidationResult rootResult { root._value.Validate(reader) };

                switch (rootResult) {
                    case ValidationResult::Failure:
                        break;
                    case ValidationResult::Children:
                    {
                        // Iterate each child.
                        // Just call Parse on each child in the tuple `root._children`,
                        // stopping on the first one that works.
                        if constexpr (sizeof...(Ts) == 0)
                            break;
                        else {
                            // Skip space to next child.
                            reader = reader.substr(1);

                            return ForEachNode<0u>(root._children, [&source](auto node, auto reader) {
                                return _Parse(reader, node, source);
                            }, [](auto result) -> bool {
                                return result != false;
                            }, std::forward<std::string_view&>(reader));
                        }
                    }
                    case ValidationResult::Execute:
                        assert(false && "unreachable");
                        break;
                }

                return false;
            } else { // Implied CommandNode<Fn>, Fn hidden here.
                static_assert(Details::ValidateTree<TEmpty, Ts...>::value,
                    "Malformed tree: command nodes cannot have any children.");

                return _Parse(reader, root._value, source);
            }
        }

        template <typename Fn>
        constexpr static auto _Parse(std::string_view& reader, CommandNode<Fn> const& root, S& source) noexcept
            -> bool
        {
            return root.template TryExecute<S>(reader, source);
        }

        template <
            size_t I,
            typename Tuple, 
            typename Fn,
            typename Condition,
            typename... Args>
        constexpr static auto ForEachNode(
            Tuple&& tuple,
            Fn&& fn,
            Condition&& condition,
            Args&&... args)
        {
            if constexpr (Details::TupleSize<std::decay_t<Tuple>>::value > 0) {
                auto result { fn(std::get<I>(tuple), std::forward<Args&&>(args)...) };
                if constexpr (I + 1 == std::tuple_size<std::decay_t<Tuple>>::value)
                    return result;
                else {
                    if (condition(result))
                        return result;
                    else
                        return ForEachNode<I + 1>(std::forward<Tuple&&>(tuple),
                            std::forward<Fn&&>(fn),
                            std::forward<Condition&&>(condition),
                            std::forward<Args&&>(args)...
                        );
                }
            }
        }
    };

    template <typename Fn>
    constexpr static CommandNode<Fn> Command(std::string_view literal, Fn&& fn) {
        return CommandNode<Fn> { literal, std::forward<Fn&&>(fn) };
    }

    constexpr static BareNode Node(std::string_view literal) {
        return BareNode { literal };
    }
}
