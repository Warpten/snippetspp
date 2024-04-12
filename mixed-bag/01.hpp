#include <cstdint>
#include <cstdio>
#include <array>
#include <algorithm>
#include <utility>
#include <new>
#include <tuple>
#include <string>

#include <boost/callable_traits/args.hpp>

// blz/Tuple.hpp

namespace blz {
    namespace dtl {
        template <std::size_t I, typename T>
        struct TupleElement {
            using value_type = T;

            T value;
        };

        template <typename... > struct Tpl;
        template <std::size_t... Is, typename... Ts>
        struct Tpl<std::index_sequence<Is...>, Ts...> final : TupleElement<Is, Ts>... {
            constexpr Tpl(Ts&&... values) : TupleElement<Is, Ts>(std::forward<Ts&&>(values))... { }
        };

        template <std::size_t U, typename T>
        auto slice_i(blz::dtl::TupleElement<U, T>*) -> TupleElement<U, T>;

        template <typename T, std::size_t U>
        auto slice_t(blz::dtl::TupleElement<U, T>*) -> TupleElement<U, T>;
    }

    template <typename... Ts>
    using Tuple = dtl::Tpl<std::make_index_sequence<sizeof...(Ts)>, Ts...>;
}

namespace std {
    template <std::size_t I, typename... Ts>
    struct tuple_element<I, blz::Tuple<Ts...>> {
        using element_type = decltype(blz::dtl::slice_i<I>(std::declval<blz::Tuple<Ts...>*>()));
        using type = typename element_type::value_type;
    };

    template <typename T, typename... Ts>
    constexpr auto get(blz::Tuple<Ts...> const& tpl) {
        return static_cast<decltype(blz::dtl::slice_t<T>(&tpl)) const&>(tpl).value;
    }

    template <typename T, typename... Ts>
    constexpr auto get(blz::Tuple<Ts...>& tpl) {
        return static_cast<decltype(blz::dtl::slice_t<T>(&tpl))&>(tpl).value;
    }

    template <typename T, typename... Ts>
    constexpr auto get(blz::Tuple<Ts...>&& tpl) {
        return static_cast<decltype(blz::dtl::slice_t<T>(&tpl))&&>(tpl).value;
    }

    template <typename T, typename... Ts>
    constexpr auto get(const blz::Tuple<Ts...>&& tpl) {
        return static_cast<decltype(blz::dtl::slice_t<T>(&tpl)) const&&>(tpl).value;
    }

    template <std::size_t I, typename... Ts>
    constexpr auto get(blz::Tuple<Ts...>& tpl) {
        return static_cast<tuple_element_t<I, blz::Tuple<Ts...>> &>(tpl).value;
    }

    template <std::size_t I, typename... Ts>
    constexpr auto get(blz::Tuple<Ts...> const& tpl) {
        return static_cast<tuple_element_t<I, blz::Tuple<Ts...>> const&>(tpl).value;
    }
    
    template <std::size_t I, typename... Ts>
    constexpr auto get(blz::Tuple<Ts...>&& tpl) {
        return static_cast<tuple_element_t<I, blz::Tuple<Ts...>>&&>(tpl).value;
    }
    
    template <std::size_t I, typename... Ts>
    constexpr auto get(const blz::Tuple<Ts...>&& tpl) {
        return static_cast<const tuple_element_t<I, blz::Tuple<Ts...>>&&>(tpl).value;
    }
}

// blz/Union.hpp

namespace blz {
    template <typename T, auto... Vs> struct Tagged { };

    namespace dtl {
        template <typename T>
        constexpr static const bool is_tagged_v = false;
        
        template <typename T, auto... Vs>
        constexpr static const bool is_tagged_v<Tagged<T, Vs...>> = true;

        template <std::size_t I, typename T>
        struct Case {
            template <typename U>
            Case(void* storage, U value, std::size_t& idx) {
                emplace(storage, idx, std::forward<U>(value));
            }

            using value_type = T;
            constexpr static const std::size_t index = I;

            static T& read(std::byte* storage) noexcept {
                return *std::launder(reinterpret_cast<T*>(storage));
            }

            static const T& read(const std::byte* storage) noexcept {
                return *std::launder(reinterpret_cast<const T*>(storage));
            }

            template <typename... Ts>
            static void emplace(void* storage, std::size_t& idx, Ts... args) 
                noexcept(std::is_nothrow_constructible_v<T, Ts...>)
            {
                if constexpr (std::is_constructible_v<T, Ts...>) {
                    new (storage) T(std::forward<Ts>(args)...);
                    idx = index;
                }
            }

            static void copy(std::byte* dst, std::byte const* src) {
                if constexpr (std::is_copy_constructible_v<T>) {
                    new (dst) T(read(src));
                }
            }

            static void destroy(std::byte* storage) {
                read(storage).~T();
            }
        };


#if __cplusplus >= 202302L
        template <bool B>
#else
        template <bool B, typename T>
#endif
        struct UnionBase {
            constexpr bool can_monostate() const { return B; }

#if __cplusplus >= 202302L
            template <typename Self>
            bool has_value(this Self&& self) {
                return B && self._index != 0;
            }
#else
            bool has_value() const {
                return B && static_cast<const T*>(this)->_index != 0;
            }
#endif
        };

        template <bool, typename...> struct Union;
        template <bool M, std::size_t... Is, typename... Ts>
        struct Union<M, std::index_sequence<Is...>, Ts...> :
#if __cplusplus >= 202302L
            public UnionBase<M>
#else
            public UnionBase<M, Union<M, std::index_sequence<Is...>, Ts...>>
#endif
            , private Case<Is, Ts>...
        {
            template <typename T>
            explicit Union(T value) : Case<Is, Ts>(_storage, value, _index)... { }

            ~Union() {
                _deleters[_index - (M ? 1 : 0)](_storage);
            }

            Union(Union&& other) noexcept((std::is_nothrow_move_constructible_v<Ts> && ...))
            {
                std::swap(_storage, other._storage);
                _index = other._index;
            }

            Union(Union const& other) noexcept((std::is_nothrow_copy_constructible_v<Ts> && ...))
            {
                _index = other._index;
                _copies[_index - (M ? 1 : 0)](_storage, other._storage);
            }

public:
            template <typename T, typename... Args>
            void emplace(Args... args) {
                // Delete old value
                _deleters[_index - (M ? 1 : 0)](_storage);

                // Emplace new
                slice_t<T>()->emplace(_storage, _index, std::forward<Args>(args)...);
            }

#if __cplusplus >= 202302L
            template <typename Self, typename... Vs>
            bool visit(this Self&& self, Vs... visitors) {
                if constexpr (M) {
                    if (self._index == 0)
                        return false;
                }

                return (self.select_case_for_visitor(visitors) || ... || false);
            }
#else
            template <typename... Vs>
            bool visit(Vs... visitors) {
                if constexpr (M) {
                    if (_index == 0)
                        return false;
                }

                return (select_case_for_visitor(visitors) || ... || false);
            }

            template <typename... Vs>
            bool visit(Vs... visitors) const{
                if constexpr (M) {
                    if (_index == 0)
                        return false;
                }

                return (select_case_for_visitor(visitors) || ... || false);
            }
#endif

            std::size_t index() const noexcept {
                return _index - (M ? 1 : 0);
            }

            template <typename T>
            T const& unsafe_get() const noexcept {
                return slice_t<T>(this)->read(_storage);
            }

            template <std::size_t I>
            auto unsafe_get() const noexcept {
                return slice_i<I + (M ? 1 : 0)>(this)->read(_storage);
            }

            template <typename T>
            T const& get() const {
                if (_index != case_t<T>::index)
                    throw "invalid runtime index";

                return unsafe_get<T>();
            }

        private:

#if __cplusplus >= 202302L
            template <typename Self, typename Visitor>
            bool select_case_for_visitor(this Self&& self, Visitor visitor) {
                using visitor_args = boost::callable_traits::args_t<Visitor, Tuple>;
                using arg_type = std::decay_t<std::tuple_element_t<0, visitor_args>>;

                using slice_type = case_t<arg_type>;

                if (self._index == slice_type::index) {
                    visitor(static_cast<slice_type>(self).read(self._storage), self._index);
                    return true;
                }

                return false;
            }
#else
            template <typename V>
            bool select_case_for_visitor(V v) {
                using visitor_args = boost::callable_traits::args_t<V, Tuple>;
                using arg_type = std::decay_t<std::tuple_element_t<0, visitor_args>>;

                using slice_type = case_t<arg_type>;

                if (_index == slice_type::index) {
                    v(slice_type::read(_storage), _index);
                    return true;
                }
                return false;
            }

            template <typename V>
            bool select_case_for_visitor(V v) const {
                using visitor_args = boost::callable_traits::args_t<V, Tuple>;
                using arg_type = std::decay_t<std::tuple_element_t<0, visitor_args>>;

                using slice_type = case_t<arg_type>;
            
                if (_index == slice_type::index) {
                    v(slice_type::read(_storage), _index);
                    return true;
                }
                return false;
            }
#endif

            template <typename T, std::size_t I>
            static Case<I, T>&& slice_type(Case<I, T>&& self);

            template <std::size_t I, typename T>
            static Case<I, T>&& slice_index(Case<I, T>&& self);

            template <typename T>
            using case_t = std::decay_t<decltype(slice_type<T>(std::declval<Union<M, std::index_sequence<Is...>, Ts...>>()))>;

            template <std::size_t I>
            using case_i = std::decay_t<decltype(slice_index<I>(std::declval<Union<M, std::index_sequence<Is...>, Ts...>>()))>;

        public:
            alignas(std::max({ alignof(Ts)... }))
            std::byte _storage[std::max({ sizeof(Ts)... })];
            std::size_t _index;

        private:
            using deleter_type = void(*)(std::byte*);
            constexpr static const std::array<deleter_type, sizeof...(Ts)> _deleters { Case<Is, Ts>::destroy... };

            using copy_type = void(*)(std::byte*, std::byte const*);
            constexpr static const std::array<copy_type, sizeof...(Ts)> _copies { Case<Is, Ts>::copy... };
        };
    }

    namespace dtl {
        template <bool M, typename T> struct offset;

        template <bool M, std::size_t... Is>
        struct offset<M, std::index_sequence<Is...>> {
            using type = std::conditional_t<M, std::index_sequence<(M + Is)...>, std::index_sequence<Is...>>;
        };

        template <bool M, typename Seq>
        using offset_t = typename offset<M, Seq>::type;
    }

    template <bool Monostate, typename... Ts>
    using Union = dtl::Union<Monostate, dtl::offset_t<Monostate, std::make_index_sequence<sizeof...(Ts)>>, Ts...>;
}

struct A {
    std::string str;
};
struct B {};
struct C {};
struct D {};

void foo(blz::Union<true, int, float, A> const& u) {
    u.visit([](int const& i, std::size_t index) {
        printf("Integer: %p %zu\n", std::addressof(i), index);
    }, [](float const& f, std::size_t index) {
        printf("Float: %p %f %zu\n", std::addressof(f), f, index);
    }, [](A const& a, std::size_t index) {
        printf("A: %p, %s\n", std::addressof(a), a.str.c_str());
        // a.str = "hello world";
    });

    u.visit([](A const& a, std::size_t index) {
        printf("A: %p, %s\n", std::addressof(a), a.str.c_str());
    });
}

int main(int, char**) {
    blz::Union<true, int, float, A> u { A { "exceedingly large string so that SSO doesn't kick in" }};
    foo(u);
    printf("%p, %p, %zu\n", std::addressof(u), std::addressof(u._storage), u.index());

    printf("%c\n", u.has_value() ? 'Y' : 'N');
}
