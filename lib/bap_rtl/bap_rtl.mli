
(**
    {2 Intro}

    This module is the only one that needed to write lifter functions.
    The main idea is to make a life of lifter writers as easy as
    possible. This implies that only few lines of code should be
    enough to describe any instruction.

    We introduce RTL - the language we expect to be very expressive,
    with lots of details hidden under the hood, so a user should not
    care about minor details.
    Also we add some useful abstractions that adds brevity and
    therefore simplicity to user code.

    So proposed usage is just to open at the very beginning of your
    module:

   {[
     open Bap_rtl.Std
   ]}

    {2 RTL}

    RTL contains contains expressions, operations over expressions and
    statements. Basically, any line that ended with ';' is a
    (compound) statement, and any part of it is either a statement,
    expression(s), or operation over expression(s).

    {3 Bitwidth and Signedness}

    Any expression in RTL has a notion of signedness and bitwidth.

    If an operation is applied to expressions with different signs,
    any unsigned expression is implicitly casted to signed one.

    If an operation is applied to expressions with different bitwidths,
    an expression with the smaller bitwidth is implicitly casted
    (extended) to the bitwidth of another expression.

    {3 Expressions}

    There are only few ways to construct an expression:
     - from instruction operand
     - from constant
     - from string
     - by defining temporary variable

    To construct an expression that denotes an immediate and treat
    its content as an unsigned value, one should write:

   {v
     let ra = unsigned imm op.(0)
              -------- ------- ------
                 ^         ^      ^
                 |         |      |
      content is |         |      |
      unsigned __|         |      |
                           |      |
                 claim immediate from operands array at index 0
    v}

    Also one may create variables for convenience:

   {[
     let x = unsigned var halfword
   ]}

    that is a creation of variable of bitwidth 16. Other useful
    bitwidthes are bit, byte, halfword, word, doubleword, quadroword.
    And also it's possible to create a variable of arbitrary bitwidth:

   {[
     let x = signed var (bitwidth 10)
   ]}

    or create an epxression from integer constant:

   {[
     let x = unsigned const word 42
   ]}

    and create an expression from string:

   {[
     let x = unsigned of_string "0xFFFF_FFFF_FFFF"
   ]}

    {4 Extraction}

    There is a general way to extract a part of an expression:
    [extract exp from to], where a [from] denotes a more significant
    bit than [to].

    Also, there are few more convenient and readable ways to extract, e.g.:
    - [low word x]  - extract fist (the least significant) word from [x]
    - [high byte x] - extract last (the most significant) byte
    - [last x 5]    - extract last (the most significant) bits
    - [first x 2]   - extract second bit
    - [msb x]       - extract the most significant bit

    Note, that extraction of a bigger width from expression is
    also possible, see example below.

    Extraction always results to an unsigned expression. And
    sign bit interpretation depends on further using of a result
    expression. So don't use it for sign casting, just use
    expression as it is where signedness matter.

    Example 1. Apply extract explicitly. The result is 0x0000_FFFF.
   {[
     let x = signed const halfword 0xFFFF in
     let y = signed var word in
     RTL.[
       y := last word x;
     ];
   ]}

    Example 2. Assignment to signed, without extraction.
    The result is 0xFFFF_FFFF.
   {[
     let x = signed const halfword 0xFFFF in
     let y = signed var word in
     RTL.[
       y := x;
     ]
   ]}

    {4 Concatenation}

    Concatenation always return an unsigned expression. It's still
    possible to use a result as signed expressions, e.g. by assigning
    it to signed expression or use in signed binary/unary operation.

    {4 Operators}

    There are lot's math operators: [plus], [modulo], [less than] etc.
    All they take one or more expressions and also return expression:
    [x + y], [lnot y], [x lsl y] ...
    There are not any special signed versions of operators. But if
    signedness is matter, then use signed operands. Like in example
    below, [<] is a signed comparison:

   {[
     let x = signed const halfword 0xFAAA in
     let y = unsigned var bit in
     RTL.[
       y := x < zero;
     ]
   ]}

    The result is true (1), since [x] is signed. But if we will replace
    x definition to [unsigned const halfword 0xFFFF] then result will be
    false (0) and [<] will be unsigned comparison.

    The same is true for shift operators: [>>] and [<<].
    There are not separate logical shift operators. But shift is a
    logical one if operand is unsigned. And otherwise, shift is
    an arithmetical one if operand is signed.

   {[
     let x = signed const halfword 0xFAAA in
     let s = unsigned const halfword 4 in
     let y = unsigned var halfword in
     RTL.[
       y := x >> s;
     ]
   ]}
    If [x] is signed, like in example above, then shift is
    arithmetical, and result is 0xFFAA. If [x] is unsigned, then shift
    is logical and result is Ox0FAA.

    {4 Assignment}

    The only operator that takes an expression and returns a statement
    is an assignment. It is a very important and expressive operator
    in RTL. The right-hand side of an assignment is always treated to
    have the same sign and width as a left one:
   {[
     ra := zero;
     rb := rc ^ rd;
   ]}
    Assuming, that [zero] is just one bit and all [ra], [rb], [rc], [rd] are 32-bit
    expressions we will get [ra], with all bits set to zero, and [rb]
    equaled to [rd], since a concatenation [rc ^ rb] returns a 64 bit
    expression while we may assign only 32 bit.

    An expression in left hand side of assignment is always either of
    expressions:
     - constructed with var/reg constructors
     - all expressions from cpu model, except pc
     - extraction or concatenation of two cases above
    So there are few examples of correct assignment:

   {[
     low byte rt := ra + rb;
     cpu.cr := zero;
     nbit cpu.cr 1 := one;
   ]}

    {2 Bit,byte,whatever Order}

    Everywhere in this module, where a notion of bit position
    (byte,word ...) does matter, a numeration starts from
    the least significant bit (byte,word, ...).

    Example 1. [y] will be set to 0xCD, because first byte
    requested:
   {[
     let x = unsigned const halfword 0xABCD in
     let y = unsigned var byte in
     RTL.[
       y := first byte x;
     ];
   ]}

    Example 2. [y] will be set to 0xAB, because last byte
    requested:
   {[
     let x = unsigned const halfword 0xABCD in
     let y = unsigned var byte in
     RTL.[
       y := last byte x;
     ];
   ]}

    Example 3. [y] will be set to one, because second bit
    requested (numeration starts from zero):
   {[
     let x = unsigned of_string "0b1010010" in
     let y = unsigned var bit in
     RTL.[
       y := nth bit x 1;
     ];
   ]}

    {2 Model}

    There are not any mandatory rules and requirements that
    one should follow to represent target CPU: it's completly
    user choice how to model memory, register, flags etc.

    But there is a way, that helps to create models.
    There are two aspects that we most carry about: memory and
    registers.

    {3 Memory}

    Basicly, we can't express load and store operations dependless
    of a knowledge of a target architecture: there are different
    size of address space, different endianness. But in most cases,
    those things persist for a particular target, i.e. once
    target model is defined, it shold be sufficient to fix
    this knowledge and don't mention it in lifter, because it
    leads to duplicated, more verbose and less readable code.

    {3 Registers}

    Reading and writing to registers are a bit less comlicated
    operations: we just need to create properly variables for each of
    them and don't care about anything else: RTL will do all
    remaining job for us. But there are lot's of code that
    user have to write to describe register: some of them
    are easily represented both as variables and expressions,
    some of them are just part of some bigger register and
    we can reperesent them only like an expression, and some registers
    have aliases and/or integer indexes associated with them.
    Those make search of a register more compilcated, and it's again
    a place where code become more verbose and less readable.

    {3 RTL cpu model}

    We still can't create an abstract model of cpu for lifters,
    since all architectures are different. So it's a completly
    lifter writer task to describe a cpu model. But we provide
    a few helpful building blocks to simplify this task.

    So, when memory model is defined, we can include
    it in cpu model and reduce user code for load/store instructions.
    E.g., a general way to load somehing from memory will look
    something like following:
   {[
     RTL.[
       x := load mem addr endian size
     ]
   ]}
    and if we fix memory variable and endianess, we will get something
    like following:
   {[
     RTL.[
       x := load addr size
     ]
   ]}

    Also it helps to create and search for registers variables and
    expressions. E.g., to create a register, that has an alias and
    also could be refered by some integer index, one can write:
   {[
     let () = add_reg "A" 32 ~aliases:["A0"] ~index:0 gpr
   ]}
    And then, depending on user needs and instruction operands
    representation, this register could be found by name or by alias:
   {[
     let x = find gpr ops.(0)
   ]}
    or by index:
   {[
     let x = findi gpr 15
   ]}

    {2 Misc}

    There are few useful constructions that either a part of RTL
    ([if_], [foreach]) or simplify code ([when_], [ifnot], [switch]).

    {2 Complete example}

    To be more concrete let's create an artificial example.
   {[
     1 let sort_of_add cpu ops =
     2   let rt = unsigned reg ops.(0) in
     3   let ra = signed reg ops.(1) in
     4   let im = unsigned imm ops.(2) in
     5   let rc = unsigned reg ops.(3) in
     6   let tm = signed var doubleword in
     7   let xv = unsigned const word 42 in
     8   let sh = unsinged const byte 2 in
     9   RTL.[
    10        rt := ra + im;
    11        tm = cpu.load rt halfword + xv;
    12        rc := (tm lsl sh) + cpu.ca;
    13    ]
   ]}

    There is a lifter for instruction [SomeSortOfAdd]. It's required
    it has two arguments: cpu model and operand array.
    An author carefully read an ISA of target architecture and
    figured out that this instruction has four operands, and that the
    first, the second and the fourth argument are registers and the
    third one is an immediate. And instruction has the following
    semantics.
    An effective address is a sum of the content of [ra] register and
    immediate. An effective address is stored in [rt] register. A
    halfword stored at this address must be summed with 42, shifted
    left twice and summed with carry flag. And the result must be
    written to [rc] register.

    How did author implement lifter for this instruction:
    - [line 1] - defined a function with two arguments
    - [lines 2-5] - parsed instruction operands
    - [lines 6-8] - defined useful constants
    - [lines 9-13] - wrote RTL code for this instruction.

    What happens on each line of RTL code:
    - [line 10]: sum of signed [ra] and unsigned imm is a signed expression,
             because one of the operands is signed. But an unsigned
             result is placed in [rt], since [rt] is unsigned too.
    - [line 11]: load from memory at address from [rt] is summed with 42
             and assigned to variable [tm]. Note, there are two width
             extension under the hood: loaded halfword is extended to
             up to a word bitwidth (since it's a bigger bitwidth among
             sum operand) and than sum extended to a doubleword
             bitwidth with respect to a [tm] sign. So, the result of
             this sum is treated as a signed.
    - [line 12]: Logical shift returns an unsigned result which is summed
             with unsigned value. The interesting part is that it's
             safe to add one-bit value (flag is one bit width) and a
             doubleword.
*)

open Core_kernel.Std
open Bap.Std

module Std : sig

  type exp [@@deriving bin_io, compare, sexp]
  type rtl [@@deriving bin_io, compare, sexp]

  (** Set of operators. Briefly it contains next operators:
      - assignment
      - math operators: +, -, *, \, %, <, >, <= , >= , =, <>
      - logical operators: lsl, lsr, lnot, land, lor, lxor  *)
  module RTL : sig

    (** [x := y] - assignment *)
    val ( := ) : exp -> exp -> rtl

    (** [x + y] - sum *)
    val ( + ) : exp -> exp -> exp

    (** [x - y] - substraction *)
    val ( - ) : exp -> exp -> exp

    (** [x * y] - multiplication *)
    val ( * ) : exp -> exp -> exp

    (** [x / y] - division *)
    val ( / ) : exp -> exp -> exp

    (** [x ^ y] - concatenation *)
    val ( ^ ) : exp -> exp -> exp

    (** [x % y] - modulo*)
    val ( % ) : exp -> exp -> exp

    (** [x < y] - less than*)
    val ( < ) : exp -> exp -> exp

    (** [x > y] - greater than*)
    val ( > ) : exp -> exp -> exp

    (** [x <= y] - less than or equal*)
    val ( <= ) : exp -> exp -> exp

    (** [x >= y] - greater than or equal *)
    val ( >= ) : exp -> exp -> exp

    (** [x = y] - equal *)
    val ( = ) : exp -> exp -> exp

    (** [x <> y] - not equal *)
    val ( <> ) : exp -> exp -> exp

    (** [x << y] - shift left *)
    val ( << )  : exp -> exp -> exp

    (** [x >> y] - shift right *)
    val ( >> )  : exp -> exp -> exp

    (** [x lor y] - logical or *)
    val ( lor )  : exp -> exp -> exp

    (** [x land y] - logical and *)
    val ( land ) : exp -> exp -> exp

    (** [x lxor y] - lofical xor*)
    val ( lxor ) : exp -> exp -> exp

    (** [lnot x] - logical not*)
    val lnot : exp -> exp

    (** [load mem addr endian size] - loads a data of [size]
        at [addr] from [mem] with [endian]. *)
    val load : var -> exp -> endian -> size -> exp

    (** [store mem addr data endian size] - stores a [data]
        of [size] at [addr] from [mem] with [endian]. *)
    val store : var -> exp -> exp -> endian -> size -> rtl

    (** [extract hi lo e] - extracts portion of [e] starting
        from bit [lo] to bit [hi], all bounds are inclusive.
        Bits indexes start from the least significant bit. *)
    val extract : int -> int -> exp -> exp

    (** [width e] - returns a bitwidth of an [e] *)
    val width : exp -> int

    (** [if_ cond then_ else_] *)
    val if_ : exp -> rtl list -> rtl list -> rtl

    (** [jmp addr] - jump to an address [addr] *)
    val jmp : exp -> rtl

    (** [foreach step e rtl] - repeat [rtl] for each [step] of [e].
        One must create an iteration variable to iterate over some
        expression. So, in example below, assuming the first operand
        is a 64-bit register, [cnt] will be equal to 8:
        ...
        let reg = unsigned reg ops.(0) in
        let cnt = unsigned const byte in
        let byte_i = unsigned var byte in
        RTL.[
           cnt := zero;
           foreach byte_i reg [
               cnt := cnt + one;
           ]
        ]
        ...

        One can use iteration variable to change content of register,
        e.g. :
        ...
        RTL.[
           cnt := zero;
           foreach byte_i reg [
               if_ (cnt = zero) [
                   byte_i := zero;
               ]
               cnt := cnt + one;
           ]
        ]
        ...
        will set a most significant byte of [reg] to zero *)
    val foreach : inverse:bool -> exp -> exp -> rtl list -> rtl

    (** [message m] - embeds a string [m] in code *)
    val message : string -> rtl

  end

  (** Operands and registers bitwidth.  *)
  type bitwidth

  val bit  : bitwidth
  val byte : bitwidth
  val word : bitwidth
  val halfword : bitwidth
  val doubleword : bitwidth
  val quadword : bitwidth
  val bitwidth : int -> bitwidth

  (** expression constructor  *)
  type 'a ec

  (** [signed ec] - returnst a signed expression from given [ec] *)
  val signed : 'a ec -> 'a

  (** [unsigned ec] - returns an unsigned expression from given [ec] *)
  val unsigned : 'a ec -> 'a

  (** imm constructor - constructs an immediate from operand *)
  val imm : (op -> exp) ec

  (** var constructor - constructs a variable of bitwidth *)
  val var : (bitwidth -> exp) ec

  (** const constructor - constructs a constant of [bitwidth] and integer *)
  val const : (bitwidth -> int -> exp) ec

  (** [of_string] - constructs an expression from string.
      String must be either in a decimal, binary, octal or hexadecimal format.
      Bitwidth of an expression is defined as following:
      if format is decimal then bitwidth equals to a number of significant bits
      else bitwidth equals to a number of all listed bits in a string.
      Examples:
       - bitwidth of [unsigned of_string "0b00"] is eqauls to 2
       - bitwidth of [unsigned of_string "0o474"] is eqauls to 9;
       - bitwidth of [unsigned of_string "0b03FA"] is eqauls to 16;
       - bitwidth of [unsigned of_string "42"] is eqauls to 6; *)
  val of_string : (string -> exp) ec

  (** [zero] is a one bit length expression set to zero *)
  val zero : exp

  (** [one] is a one bit length expression set to one *)
  val one  : exp

  (** [low width e] - extracts low [width] bits from [e]  *)
  val low : bitwidth -> exp -> exp

  (** [high width e] - extracts high [width] bits from [e]  *)
  val high : bitwidth -> exp -> exp

  (** [first e n] - extracts first [n] bits from [e], starting from
      the least significant bit *)
  val first : exp -> int -> exp

  (** [last e n] - extracts last [n] bits from [e], where the
      last bit is the most significant bit *)
  val last : exp -> int -> exp

  (** [nth width e n] - extracts a portion of [e] of width [width] at
      index [n], where each index points to a portion of width [width].
      Indexes are zero based and started from the least significant portion.
      E.g. [nth halfword e 1] extracts a second halfword from [e] *)
  val nth : bitwidth -> exp -> int -> exp

  (** [msb e] - extracts the most significant bit from [e] *)
  val msb : exp -> exp

  (** [lsb e] - extracts the least significant bit from [e] *)
  val lsb : exp -> exp

  (** [when_ cond rtl] = if_ cond rtl [] *)
  val when_ : exp -> rtl list -> rtl

  (** [ifnot cond rtl] = if_ cond [] rtl *)
  val ifnot : exp -> rtl list -> rtl

  (** switch clause  *)
  type clause

  (** [switch x clauses] - create a switch construction.
      Example:
      ...
      ra := <...>
      switch (x) [
        case one   [ rs := <...>; ];
        case zero  [ rt := <...>;
                     rs := <...>; ];
        default [rs := zero];
      ]
      ...  *)
  val switch  : exp -> clause list -> rtl

  (** [case exp code] - creates a switch case *)
  val case : exp -> rtl list -> clause

  (** [default code] - creates a switch default *)
  val default : rtl list -> clause

  (** [width e] - returns width of [e] as an expression *)
  val width : exp -> exp

  (** Building blocks for cpu model representation. One can
      think about this module as a handsome (but not mandatory!)
      helper to describe a desireable target.
      Basicly, Model = Memory + Registers. *)
  module Model : sig

    (** Memory representation *)
    module type M = sig
      val mem : var
      val endian : endian
    end

    (** Register class  *)
    module Reg_class : sig
      type t [@@deriving bin_io,compare,sexp]

      (** [create name] - creates a new register class [name] *)
      val create : string -> t

      (** Few predefined register classes  *)

      (** General Purpose Registers *)
      val gpr : t

      (** Floating Point Registers *)
      val fpr : t

      (** Flags  *)
      val flag : t

      (** Vector registers  *)
      val vector : t

    end

    type cls = Reg_class.t [@@deriving bin_io,compare,sexp]

    exception Register_not_found of string

    module Make(M : M) : sig

      (** [load addr size] - returns an exp, that describes a loading
          of a chunk of [size] from memory at [addr] *)
      val load : exp -> bitwidth -> exp

      (** [store addr data size] - returns a statement, that
          describes a storing [data] of [size] to a memory at [addr] *)
      val store : exp -> exp -> bitwidth -> rtl

      (** [add_reg reg ~aliases ~index cls] - adds a new register
          [reg] of class [cls] to a model. Register name, aliases and
          [index] could be used later to find this register. *)
      val add_reg : var -> ?aliases:string list -> ?index:int -> cls  -> unit

      (** [add_reg' name exp ~aliases ~index cls] - the same as
          above, but adds an expression for register [name] to a
          model. This could be useful when register in question can't
          be represented as a separate variable, e.g. in case when a
          particular bits of a bigger registers have special meaning.
          Then one could register them as a separate name:
          {[add_reg' name (extrace 16 8 bigger_reg gpr]} *)
      val add_reg' : string -> exp -> ?aliases:string list -> ?index:int -> cls  -> unit

      (** [find cls reg] - return an expression for register [reg]
          class [cls] if such is in a model.
          Raise Register_not_found otherwise.*)
      val find  : cls -> reg -> exp

      (** [findi cls index] - return an expression for register with
          [index] abd class [cls] if such is in a model.
          Raise Register_not_found otherwise.*)
      val findi : cls -> int -> exp
    end

  end

  (** [bil_of_rtl rtl] - returns a bil code *)
  val bil_of_rtl : rtl list -> bil

end
