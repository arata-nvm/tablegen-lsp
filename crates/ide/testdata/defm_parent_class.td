class Base;
class Extra1;
class Extra2;

multiclass MC {
  def _A : Base;
  def _B : Base;
}

defm inst_single : MC, Extra1;
defm inst_multi : MC, Extra1, Extra2;

multiclass OuterMC {
  defm inner : MC, Extra1, Extra2;
}

defm outer_inst : OuterMC;
