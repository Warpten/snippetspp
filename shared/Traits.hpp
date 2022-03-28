/* 
 * This file is part of the snippetspp distribution (https://github.com/Warpten/snippetspp).
 * Copyright (c) 2022 Warpten.
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

namespace Traits {
	// Type sequence.
	template <typename...> struct TypeSequence { };

	/**************************************************************/

	// Ensures all types in a sequence are distinct.
	namespace details {
		template <typename Needle, typename... Haystack>
		constexpr static const size_t occurences
			= ((std::is_same_v<Needle, Haystack> ? 1 : 0) + ... );
	}

	/**************************************************************/

	template <typename... Ts>
	constexpr static const bool IsUniq = ((details::occurences<Ts, Ts...> == 1) && ...);

	/**************************************************************/

	// Lisp HEAD function
	template <typename> struct _Head;
	template <typename T, typename... Ts> struct _Head<TypeSequence<T, Ts...>> { using type = T; };
	template <typename T> using Head = typename _Head<T>::type;

	// Lisp TAIL function
	template <typename> struct _Tail;
	template <typename T, typename... Ts> struct _Tail<TypeSequence<T, Ts...>> { using type = TypeSequence<Ts...>; };

	// Lisp CONS fonction
	template <typename, typename> struct _Cons;
	template <typename T, typename... Ts>
	struct _Cons<T, TypeSequence<Ts...>> { using type = TypeSequence<T, Ts...>; };
	template <typename T, typename Ts> using Cons = typename _Cons<T, Ts>::type;

	/**************************************************************/

	template <typename T> struct CallableTraits;

	template <typename R, typename... Args>
	struct CallableTraits<R(*)(Args...)> {
		using return_type = R;
		using argument_types = TypeSequence<Args...>;

		template <size_t I>
		using Argument = std::tuple_element_t<I, argument_types>;
	};

	template <typename R, typename C, typename... Args>
	struct CallableTraits<R(C::*)(Args...)> {
		using return_type = R;
		using owner_type = C;
		using owner_pointer_type = C*;

		using argument_types = std::conditional_t<
			std::is_member_function_pointer_v<R(C::*)(Args...)>,
			TypeSequence<owner_pointer_type, Args...>,
			TypeSequence<Args...>
		>;

		template <size_t I>
		using Argument = std::tuple_element_t<I, TypeSequence<Args...>>;
	};

	template <typename R, typename C, typename... Args>
	struct CallableTraits<R(C::*)(Args...) noexcept> : CallableTraits<R(C::*)(Args...)> { };

	template <typename R, typename C, typename... Args>
	struct CallableTraits<R(C::*)(Args...) const> {
		using return_type = R;
		using owner_type = C;
		using owner_pointer_type = C const*;

		using argument_types = std::conditional_t<
			std::is_member_function_pointer_v<R(C::*)(Args...) const>,
			TypeSequence<owner_pointer_type, Args...>,
			TypeSequence<Args...>
		>;

		template <size_t I>
		using Argument = std::tuple_element_t<I, TypeSequence<Args...>>;
	};

	template <typename R, typename C, typename... Args>
	struct CallableTraits<R(C::*)(Args...) const noexcept> : CallableTraits<R(C::*)(Args...) const> { };

	template <typename T>
	struct CallableTraits<T&&> : CallableTraits<T> { };

	template <typename T>
	struct CallableTraits<T&> : CallableTraits<T> { };

	template <typename T>
	struct CallableTraits<T const&> : CallableTraits<T> { };

	template <typename T>
	struct CallableTraits : CallableTraits<decltype(&T::operator())> { };

	/**************************************************************/

    template <template <typename...> typename, typename...> struct Filter;
    template <template <typename...> typename Predicate> struct Filter<Predicate> {
        template <typename...>
        using Execute = TypeSequence<>;
    };

    template <template <typename...> typename Predicate, typename Head, typename... Tail>
    struct Filter<Predicate, Head, Tail...> {
        template <typename... PredicateParams>
        using Execute = std::conditional_t<
            Predicate<Head, PredicateParams...>::value,
            Cons<
                Head,
                typename Filter<Predicate, Tail...>::template Execute<PredicateParams...>
            >,
            typename Filter<Predicate, Tail...>::template Execute<PredicateParams...>
        >;
    };
}

namespace std {
    // recursive case
    template< std::size_t I, class Head, class... Tail >
    struct tuple_element<I, Traits::TypeSequence<Head, Tail...>>
        : tuple_element<I - 1, Traits::TypeSequence<Tail...>> { };
 
    // base case
    template< class Head, class... Tail >
    struct tuple_element<0, Traits::TypeSequence<Head, Tail...>> {
        using type = Head;
    };
}
