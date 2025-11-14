class BaseReg {
  int id;
}

class Instruction<int opc> {
  int opcode = opc;
}

// Simple multiclass
multiclass SimpleMulticlass {
  def _variant1 : BaseReg;
  def _variant2 : BaseReg;
}

// Multiclass with template arguments
multiclass TemplatedMulticlass<int base_opc, string prefix> {
  def _rr : Instruction<base_opc> {
    string Name = prefix # "_rr";
  }
  def _ri : Instruction<!add(base_opc, 1)> {
    string Name = prefix # "_ri";
  }
}

// Multiclass with anonymous defs
multiclass AnonymousMulticlass {
  def _named : BaseReg;
  def "": BaseReg;
  def : BaseReg;
}

// Nested multiclass (multiclass inheriting from multiclass)
multiclass ParentMulticlass {
  def _parent : BaseReg;
}

multiclass ChildMulticlass : ParentMulticlass {
  def _child : BaseReg;
}

// Defm instantiation
defm inst1 : SimpleMulticlass;
defm inst2 : TemplatedMulticlass<0x10, "ADD">;

// Anonymous defm
defm : SimpleMulticlass;

// Defm in multiclass (nested defm)
multiclass OuterMulticlass {
  defm _inner : SimpleMulticlass;
}

defm outer_inst : OuterMulticlass;

// Multiclass with defvar
multiclass MulticlassWithDefvar<int x> {
  defvar doubled = !add(x, x);
  def _value : Instruction<doubled>;
}

defm computed : MulticlassWithDefvar<5>;
