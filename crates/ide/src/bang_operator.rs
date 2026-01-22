use syntax::syntax_kind::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BangOperatorMetadata {
    pub name: &'static str,
    pub needs_type_annotation: bool,
    pub min_args: usize,
    pub max_args: Option<usize>,
    pub syntax_kind: SyntaxKind,
    pub documentation: &'static str,
}

pub fn get_metadata_for_syntax_kind(kind: SyntaxKind) -> Option<&'static BangOperatorMetadata> {
    OPS.iter().find(|meta| meta.syntax_kind == kind)
}

pub fn get_metadata_for_name(name: &str) -> Option<&'static BangOperatorMetadata> {
    OPS.iter().find(|meta| meta.name == name)
}

// ref: https://github.com/llvm/llvm-project/blob/a69cddef43f64d5307c0d5f2f01e5176ac05729a/llvm/docs/TableGen/ProgRef.rst
pub const OPS: &[BangOperatorMetadata] = &[
    BangOperatorMetadata {
        name: "add",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XAdd,
        documentation: r#"`!add(` *a* `,` *b* `, ...)`

This operator adds *a*, *b*, etc., and produces the sum."#,
    },
    BangOperatorMetadata {
        name: "and",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XAnd,
        documentation: r#"`!and(` *a* `,` *b* `, ...)`

This operator does a bitwise AND on *a*, *b*, etc., and produces the result. A logical AND can be performed if all the arguments are either 0 or 1. This operator is short-circuit to 0 when the left-most operand is 0."#,
    },
    BangOperatorMetadata {
        name: "cast",
        needs_type_annotation: true,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XCast,
        documentation: r#"`!cast<` *type* `>(` *a* `)`

This operator performs a cast on *a* and produces the result. If *a* is not a string, then a straightforward cast is performed, say between an `int` and a `bit`, or between record types. This allows casting a record to a class. If a record is cast to `string`, the record's name is produced.

If *a* is a string, then it is treated as a record name and looked up in the list of all defined records. The resulting record is expected to be of the specified *type*.

For example, if `!cast<` *type* `>(` *name* `)` appears in a multiclass definition, or in a class instantiated inside a multiclass definition, and the *name* does not reference any template arguments of the multiclass, then a record by that name must have been instantiated earlier in the source file. If *name* does reference a template argument, then the lookup is delayed until `defm` statements instantiating the multiclass (or later, if the defm occurs in another multiclass and template arguments of the inner multiclass that are referenced by *name* are substituted by values that themselves contain references to template arguments of the outer multiclass).

If the type of *a* does not match *type*, TableGen raises an error."#,
    },
    BangOperatorMetadata {
        name: "concat",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XCon,
        documentation: r#"`!con(` *a* `,` *b* `, ...)`

This operator concatenates the DAG nodes *a*, *b*, etc. Their operations must equal.

`!con((op a1:$name1, a2:$name2), (op b1:$name3))`

results in the DAG node `(op a1:$name1, a2:$name2, b1:$name3)`."#,
    },
    BangOperatorMetadata {
        name: "cond",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XCond,
        documentation: r#"`!cond(` *cond1* `:` *val1* `,` *cond2* `:` *val2* `, ...,` *condn* `:` *valn* `)`

This operator tests *cond1* and returns *val1* if the result is true. If false, the operator tests *cond2* and returns *val2* if the result is true. And so forth. An error is reported if no conditions are true.

This example produces the sign word for an integer:

```
!cond(!lt(x, 0) : "negative", !eq(x, 0) : "zero", true : "positive")
```"#,
    },
    BangOperatorMetadata {
        name: "dag",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XDag,
        documentation: r#"`!dag(` *op* `,` *arguments* `,` *names* `)`

This operator creates a DAG node with the given operator and arguments. The *arguments* and *names* arguments must be lists of equal length or uninitialized (`?`). The *names* argument must be of type `list<string>`.

Due to limitations of the type system, *arguments* must be a list of items of a common type. In practice, this means that they should either have the same type or be records with a common parent class. Mixing `dag` and non-`dag` items is not possible. However, `?` can be used.

Example: `!dag(op, [a1, a2, ?], ["name1", "name2", "name3"])` results in `(op a1-value:$name1, a2-value:$name2, ?:$name3)`."#,
    },
    BangOperatorMetadata {
        name: "div",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XDiv,
        documentation: r#"`!div(` *a* `,` *b* `)`

This operator performs signed division of *a* by *b*, and produces the quotient. Division by 0 produces an error. Division of INT64_MIN by -1 produces an error."#,
    },
    BangOperatorMetadata {
        name: "empty",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XEmpty,
        documentation: r#"`!empty(` *a* `)`

This operator produces 1 if the string, list, or DAG *a* is empty; 0 otherwise. A dag is empty if it has no arguments; the operator does not count."#,
    },
    BangOperatorMetadata {
        name: "eq",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XEq,
        documentation: r#"`!eq(` *a* `,` *b* `)`

This operator produces 1 if *a* is equal to *b*; 0 otherwise. The arguments must be `bit`, `bits`, `int`, `string`, or record values. Use `!cast<string>` to compare other types of objects."#,
    },
    BangOperatorMetadata {
        name: "exists",
        needs_type_annotation: true,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XExists,
        documentation: r#"`!exists<` *type* `>(` *name* `)`

This operator produces 1 if a record of the given *type* whose name is *name* exists; 0 otherwise. *name* should be of type *string*."#,
    },
    BangOperatorMetadata {
        name: "filter",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XFilter,
        documentation: r#"`!filter(` *var* `,` *list* `,` *predicate* `)`

This operator creates a new `list` by filtering the elements in *list*. To perform the filtering, TableGen binds the variable *var* to each element and then evaluates the *predicate* expression, which presumably refers to *var*. The predicate must produce a boolean value (`bit`, `bits`, or `int`). The value is interpreted as with `!if`: if the value is 0, the element is not included in the new list. If the value is anything else, the element is included."#,
    },
    BangOperatorMetadata {
        name: "find",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XFind,
        documentation: r#"`!find(` *string1* `,` *string2* `[`, *start* `])`

This operator searches for *string2* in *string1* and produces its position. The starting position of the search may be specified by *start*, which can range between 0 and the length of *string1*; the default is 0. If the string is not found, the result is -1."#,
    },
    BangOperatorMetadata {
        name: "foldl",
        needs_type_annotation: false,
        min_args: 5,
        max_args: Some(5),
        syntax_kind: SyntaxKind::XFoldl,
        documentation: r#"`!foldl(` *init* `,` *list* `,` *acc* `,` *var* `,` *expr* `)`

This operator performs a left-fold over the items in *list*. The variable *acc* acts as the accumulator and is initialized to *init*. The variable *var* is bound to each element in the *list*. The expression is evaluated for each element and presumably uses *acc* and *var* to calculate the accumulated value, which `!foldl` stores back in *acc*. The type of *acc* is the same as *init*; the type of *var* is the same as the elements of *list*; *expr* must have the same type as *init*.

The following example computes the total of the `Number` field in the list of records in `RecList`:

```
int x = !foldl(0, RecList, total, rec, !add(total, rec.Number));
```

If your goal is to filter the list and produce a new list that includes only some of the elements, see `!filter`."#,
    },
    BangOperatorMetadata {
        name: "foreach",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XForEach,
        documentation: r#"`!foreach(` *var* `,` *sequence* `,` *expr* `)`

This operator creates a new `list`/`dag` in which each element is a function of the corresponding element in the *sequence* `list`/`dag`. To perform the function, TableGen binds the variable *var* to an element and then evaluates the expression. The expression presumably refers to the variable *var* and calculates the result value.

If you simply want to create a list of a certain length containing the same value repeated multiple times, see `!listsplat`."#,
    },
    BangOperatorMetadata {
        name: "ge",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XGe,
        documentation: r#"`!ge(` *a* `,` *b* `)`

This operator produces 1 if *a* is greater than or equal to *b*; 0 otherwise. The arguments must be `bit`, `bits`, `int`, or `string` values."#,
    },
    BangOperatorMetadata {
        name: "getdagarg",
        needs_type_annotation: true,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XGetDagArg,
        documentation: r#"`!getdagarg<` *type* `>(` *dag* `,` *key* `)`

This operator retrieves the argument from the given *dag* node by the specified *key*, which is either an integer index or a string name. If that argument is not convertible to the specified *type*, `?` is returned."#,
    },
    BangOperatorMetadata {
        name: "getdagname",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XGetDagName,
        documentation: r#"`!getdagname(` *dag* `,` *index* `)`

This operator retrieves the argument name from the given *dag* node by the specified *index*. If that argument has no name associated, `?` is returned."#,
    },
    BangOperatorMetadata {
        name: "getdagop",
        needs_type_annotation: true,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XGetDagOp,
        documentation: r#"`!getdagop(` *dag* `)` --or-- `!getdagop<` *type* `>(` *dag* `)`

This operator produces the operator of the given *dag* node. Example: `!getdagop((foo 1, 2))` results in `foo`. Recall that DAG operators are always records.

The result of `!getdagop` can be used directly in a context where any record class at all is acceptable (typically placing it into another dag value). But in other contexts, it must be explicitly cast to a particular class. The `<` *type* `>` syntax is provided to make this easy.

For example, to assign the result to a value of type `BaseClass`, you could write either of these:

```
BaseClass b = !getdagop<BaseClass>(someDag);
BaseClass b = !cast<BaseClass>(!getdagop(someDag));
```

But to create a new DAG node that reuses the operator from another, no cast is necessary:

```
dag d = !dag(!getdagop(someDag), args, names);
```"#,
    },
    BangOperatorMetadata {
        name: "gt",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XGt,
        documentation: r#"`!gt(` *a* `,` *b* `)`

This operator produces 1 if *a* is greater than *b*; 0 otherwise. The arguments must be `bit`, `bits`, `int`, or `string` values."#,
    },
    BangOperatorMetadata {
        name: "head",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XHead,
        documentation: r#"`!head(` *a* `)`

This operator produces the zeroth element of the list *a*. (See also `!tail`.)"#,
    },
    BangOperatorMetadata {
        name: "if",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XIf,
        documentation: r#"`!if(` *test* `,` *then* `,` *else* `)`

This operator evaluates the *test*, which must produce a `bit` or `int`. If the result is not 0, the *then* expression is produced; otherwise the *else* expression is produced."#,
    },
    BangOperatorMetadata {
        name: "initialized",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XInitialized,
        documentation: r#"`!initialized(` *a* `)`

This operator produces 1 if *a* is not the uninitialized value (`?`) and 0 otherwise."#,
    },
    BangOperatorMetadata {
        name: "instances",
        needs_type_annotation: true,
        min_args: 0,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XInstances,
        documentation: r#"`!instances<` *type* `>([` *regex* `])`

This operator produces a list of records whose type is *type*. If *regex* is provided, only records whose name matches the regular expression *regex* will be included. The format of *regex* is ERE (Extended POSIX Regular Expressions).

If `!instances` is in a class/multiclass/foreach, only these records of *type* that have been instantiated will be considered."#,
    },
    BangOperatorMetadata {
        name: "interleave",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XInterleave,
        documentation: r#"`!interleave(` *list* `,` *delim* `)`

This operator concatenates the items in the *list*, interleaving the *delim* string between each pair, and produces the resulting string. The list can be a list of string, int, bits, or bit. An empty list results in an empty string. The delimiter can be the empty string."#,
    },
    BangOperatorMetadata {
        name: "isa",
        needs_type_annotation: true,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XIsA,
        documentation: r#"`!isa<` *type* `>(` *a* `)`

This operator produces 1 if the type of *a* is a subtype of the given *type*; 0 otherwise."#,
    },
    BangOperatorMetadata {
        name: "le",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XLe,
        documentation: r#"`!le(` *a* `,` *b* `)`

This operator produces 1 if *a* is less than or equal to *b*; 0 otherwise. The arguments must be `bit`, `bits`, `int`, or `string` values."#,
    },
    BangOperatorMetadata {
        name: "listconcat",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XListConcat,
        documentation: r#"`!listconcat(` *list1* `,` *list2* `, ...)`

This operator concatenates the list arguments *list1*, *list2*, etc., and produces the resulting list. The lists must have the same element type."#,
    },
    BangOperatorMetadata {
        name: "listflatten",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XListFlatten,
        documentation: r#"`!listflatten(` *list* `)`

This operator flattens a list of lists *list* and produces a list with all elements of the constituent lists concatenated. If *list* is of type `list<list<X>>` the resulting list is of type `list<X>`. If *list*'s element type is not a list, the result is *list* itself."#,
    },
    BangOperatorMetadata {
        name: "listremove",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XListRemove,
        documentation: r#"`!listremove(` *list1* `,` *list2* `)`

This operator returns a copy of *list1* removing all elements that also occur in *list2*. The lists must have the same element type."#,
    },
    BangOperatorMetadata {
        name: "listsplat",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XListSplat,
        documentation: r#"`!listsplat(` *value* `,` *count* `)`

This operator produces a list of length *count* whose elements are all equal to the *value*. For example, `!listsplat(42, 3)` results in `[42, 42, 42]`."#,
    },
    BangOperatorMetadata {
        name: "log2",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XLog2,
        documentation: r#"`!logtwo(` *a* `)`

This operator produces the base 2 log of *a* and produces the integer result. The log of 0 or a negative number produces an error. This is a flooring operation."#,
    },
    BangOperatorMetadata {
        name: "lt",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XLt,
        documentation: r#"`!lt(` *a* `,` *b* `)`

This operator produces 1 if *a* is less than *b*; 0 otherwise. The arguments must be `bit`, `bits`, `int`, or `string` values."#,
    },
    BangOperatorMetadata {
        name: "match",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XMatch,
        documentation: r#"`!match(` *str* `,` *regex* `)`

This operator produces 1 if the *str* matches the regular expression *regex*. The format of *regex* is ERE (Extended POSIX Regular Expressions)."#,
    },
    BangOperatorMetadata {
        name: "mul",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XMul,
        documentation: r#"`!mul(` *a* `,` *b* `, ...)`

This operator multiplies *a*, *b*, etc., and produces the product."#,
    },
    BangOperatorMetadata {
        name: "ne",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XNe,
        documentation: r#"`!ne(` *a* `,` *b* `)`

This operator produces 1 if *a* is not equal to *b*; 0 otherwise. The arguments must be `bit`, `bits`, `int`, `string`, or record values. Use `!cast<string>` to compare other types of objects."#,
    },
    BangOperatorMetadata {
        name: "not",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XNot,
        documentation: r#"`!not(` *a* `)`

This operator performs a logical NOT on *a*, which must be an integer. The argument 0 results in 1 (true); any other argument results in 0 (false)."#,
    },
    BangOperatorMetadata {
        name: "or",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XOr,
        documentation: r#"`!or(` *a* `,` *b* `, ...)`

This operator does a bitwise OR on *a*, *b*, etc., and produces the result. A logical OR can be performed if all the arguments are either 0 or 1. This operator is short-circuit to -1 (all ones) the left-most operand is -1."#,
    },
    BangOperatorMetadata {
        name: "range",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XRange,
        documentation: r#"`!range([` *start* `,]` *end* `[,` *step* `])`

This operator produces half-open range sequence `[start : end : step)` as `list<int>`. *start* is `0` and *step* is `1` by default. *step* can be negative and cannot be 0. If *start* `<` *end* and *step* is negative, or *start* `>` *end* and *step* is positive, the result is an empty list `[]<int>`.

For example:

* `!range(4)` is equivalent to `!range(0, 4, 1)` and the result is `[0, 1, 2, 3]`.
* `!range(1, 4)` is equivalent to `!range(1, 4, 1)` and the result is `[1, 2, 3]`.
* The result of `!range(0, 4, 2)` is `[0, 2]`.
* The results of `!range(0, 4, -1)` and `!range(4, 0, 1)` are empty.

`!range(` *list* `)`

Equivalent to `!range(0, !size(list))`."#,
    },
    BangOperatorMetadata {
        name: "repr",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XRepr,
        documentation: r#"`!repr(` *value* `)`

Represents *value* as a string. String format for the value is not guaranteed to be stable. Intended for debugging purposes only."#,
    },
    BangOperatorMetadata {
        name: "setdagarg",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XSetDagArg,
        documentation: r#"`!setdagarg(` *dag* `,` *key* `,` *arg* `)`

This operator produces a DAG node with the same operator and arguments as *dag*, but replacing the value of the argument specified by the *key* with *arg*. That *key* could be either an integer index or a string name."#,
    },
    BangOperatorMetadata {
        name: "setdagname",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XSetDagName,
        documentation: r#"`!setdagname(` *dag* `,` *key* `,` *name* `)`

This operator produces a DAG node with the same operator and arguments as *dag*, but replacing the name of the argument specified by the *key* with *name*. That *key* could be either an integer index or a string name."#,
    },
    BangOperatorMetadata {
        name: "setdagop",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XSetDagOp,
        documentation: r#"`!setdagop(` *dag* `,` *op* `)`

This operator produces a DAG node with the same arguments as *dag*, but with its operator replaced with *op*.

Example: `!setdagop((foo 1, 2), bar)` results in `(bar 1, 2)`."#,
    },
    BangOperatorMetadata {
        name: "shl",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XShl,
        documentation: r#"`!shl(` *a* `,` *count* `)`

This operator shifts *a* left logically by *count* bits and produces the resulting value. The operation is performed on a 64-bit integer; the result is undefined for shift counts outside 0...63."#,
    },
    BangOperatorMetadata {
        name: "size",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XSize,
        documentation: r#"`!size(` *a* `)`

This operator produces the size of the string, list, or dag *a*. The size of a DAG is the number of arguments; the operator does not count."#,
    },
    BangOperatorMetadata {
        name: "sra",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XSra,
        documentation: r#"`!sra(` *a* `,` *count* `)`

This operator shifts *a* right arithmetically by *count* bits and produces the resulting value. The operation is performed on a 64-bit integer; the result is undefined for shift counts outside 0...63."#,
    },
    BangOperatorMetadata {
        name: "srl",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XSrl,
        documentation: r#"`!srl(` *a* `,` *count* `)`

This operator shifts *a* right logically by *count* bits and produces the resulting value. The operation is performed on a 64-bit integer; the result is undefined for shift counts outside 0...63."#,
    },
    BangOperatorMetadata {
        name: "strconcat",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XStrConcat,
        documentation: r#"`!strconcat(` *str1* `,` *str2* `, ...)`

This operator concatenates the string arguments *str1*, *str2*, etc., and produces the resulting string."#,
    },
    BangOperatorMetadata {
        name: "sub",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(2),
        syntax_kind: SyntaxKind::XSub,
        documentation: r#"`!sub(` *a* `,` *b* `)`

This operator subtracts *b* from *a* and produces the arithmetic difference."#,
    },
    BangOperatorMetadata {
        name: "subst",
        needs_type_annotation: false,
        min_args: 3,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XSubst,
        documentation: r#"`!subst(` *target* `,` *repl* `,` *value* `)`

This operator replaces all occurrences of the *target* in the *value* with the *repl* and produces the resulting value. The *value* can be a string, in which case substring substitution is performed.

The *value* can be a record name, in which case the operator produces the *repl* record if the *target* record name equals the *value* record name; otherwise it produces the *value*."#,
    },
    BangOperatorMetadata {
        name: "substr",
        needs_type_annotation: false,
        min_args: 2,
        max_args: Some(3),
        syntax_kind: SyntaxKind::XSubstr,
        documentation: r#"`!substr(` *string* `,` *start* `[`, *length* `])`

This operator extracts a substring of the given *string*. The starting position of the substring is specified by *start*, which can range between 0 and the length of the string. The length of the substring is specified by *length*; if not specified, the rest of the string is extracted. The *start* and *length* arguments must be integers."#,
    },
    BangOperatorMetadata {
        name: "tail",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XTail,
        documentation: r#"`!tail(` *a* `)`

This operator produces a new list with all the elements of the list *a* except for the zeroth one. (See also `!head`.)"#,
    },
    BangOperatorMetadata {
        name: "tolower",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XToLower,
        documentation: r#"`!tolower(` *a* `)`

This operator converts a string input *a* to lower case."#,
    },
    BangOperatorMetadata {
        name: "toupper",
        needs_type_annotation: false,
        min_args: 1,
        max_args: Some(1),
        syntax_kind: SyntaxKind::XToUpper,
        documentation: r#"`!toupper(` *a* `)`

This operator converts a string input *a* to upper case."#,
    },
    BangOperatorMetadata {
        name: "xor",
        needs_type_annotation: false,
        min_args: 2,
        max_args: None,
        syntax_kind: SyntaxKind::XXor,
        documentation: r#"`!xor(` *a* `,` *b* `, ...)`

This operator does a bitwise EXCLUSIVE OR on *a*, *b*, etc., and produces the result. A logical XOR can be performed if all the arguments are either 0 or 1."#,
    },
];
